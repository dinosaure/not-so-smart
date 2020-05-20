open Rresult
open Lwt_backend
open Store_backend

let ( <.> ) f g x = f (g x)

let src = Logs.Src.create "not-so-smart.fetch"

module Log = (val Logs.src_log src : Logs.LOG)

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

module Crt = struct
  module Thin = Carton_lwt.Thin.Make (Uid)

  type fd = { fd : Lwt_unix.file_descr; max : int64 }

  let create filename =
    let open Lwt.Infix in
    let filename = Fpath.to_string filename in
    Lwt_unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 >>= fun fd ->
    let st = Unix.LargeFile.stat filename in
    Lwt.return_ok { fd; max = st.st_size }

  let map { fd; _ } ~pos len =
    let res =
      Mmap.V1.map_file
        (Lwt_unix.unix_file_descr fd)
        ~pos Bigarray.char Bigarray.c_layout false [| len |] in
    let res = Bigarray.array1_of_genarray res in
    Lwt.return res

  let append { fd; _ } payload =
    let open Lwt.Infix in
    let rec go off len =
      if len <= 0
      then Lwt.return_unit
      else
        Lwt_unix.write fd (Bytes.of_string payload) off len
        >>= fun len' -> go (off + len') (len - len') in
    go 0 (String.length payload)

  let fs =
    {
      Thin.create;
      Thin.append;
      Thin.map;
      Thin.close = (fun { fd; _ } -> Lwt_unix.close fd);
    }

  let digest ~kind ?(off = 0) ?len buf =
    let len =
      match len with Some len -> len | None -> Bigstringaf.length buf - off
    in
    let ctx = Digestif.SHA1.empty in

    let ctx =
      match kind with
      | `A -> Digestif.SHA1.feed_string ctx (Fmt.strf "commit %d\000" len)
      | `B -> Digestif.SHA1.feed_string ctx (Fmt.strf "tree %d\000" len)
      | `C -> Digestif.SHA1.feed_string ctx (Fmt.strf "blob %d\000" len)
      | `D -> Digestif.SHA1.feed_string ctx (Fmt.strf "tag %d\000" len) in
    let ctx = Digestif.SHA1.feed_bigstring ctx ~off ~len buf in
    Digestif.SHA1.(Uid.of_hex (to_hex (get ctx)))

  let transmit_of_filename filename =
    let filename = Fpath.to_string filename in
    let ic = open_in filename in
    let rs = Bytes.create De.io_buffer_size in
    let go ~brk =
      seek_in ic (Int64.to_int brk) ;
      let len = input ic rs 0 (Bytes.length rs) in
      Lwt.return (rs, 0, len) in
    go

  let ( >>? ) x f =
    let open Lwt.Infix in
    x >>= function Ok x -> f x | Error _ as err -> Lwt.return err

  let run ~light_load ~heavy_load stream filename =
    let open Lwt.Infix in
    ( Bos.OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp ->
      Log.debug (fun m ->
          m "Start to verify incoming PACK file (%a)." Fpath.pp tmp) ;
      Lwt.catch
        (fun () -> Thin.verify ~digest tmp fs stream)
        (fun exn ->
           Printexc.print_backtrace stdout ;
           Lwt.return_error (`Msg (Printexc.to_string exn))) >>? function
      | (_, [], weight) ->
        Log.debug (fun m -> m "Given PACK file is not thin.") ;
        Bos.OS.Path.move tmp filename |> Lwt.return >>? fun () ->
        Lwt.return_ok weight
      | (n, uids, weight) ->
        Log.debug (fun m -> m "Income PACK file: %Ld byte(s)." weight) ;
        Thin.canonicalize ~light_load ~heavy_load
          ~transmit:(transmit_of_filename tmp) filename fs n uids weight )
    >>= function
    | Ok weight ->
        Log.debug (fun m ->
            m "Store %Ld byte(s) into %a." weight Fpath.pp filename) ;
        Lwt.return_unit
    | Error err -> failwithf "%a" Rresult.R.pp_msg err
end

module Tuyau = struct
  type t = Conduit_lwt.flow

  type +'a fiber = 'a Lwt.t

  include Conduit_lwt
end

module Fetch = Nss.Fetch.Make (Scheduler) (struct include Lwt let yield = pause end) (Tuyau) (Uid) (Ref)

let connect ~capabilities path ~resolvers ?want domain_name store access
    fetch_cfg pack =
  let open Lwt.Infix in
  Log.debug (fun m -> m "Try to connect to <%a>." Domain_name.pp domain_name) ;
  Conduit_lwt_unix.flow resolvers domain_name >>= function
  | Error err ->
      Log.err (fun m -> m "<%a> unavailable." Domain_name.pp domain_name) ;
      failwithf "%a" Conduit_lwt_unix.pp_error err
  | Ok flow -> (
      Log.info (fun m ->
          m "Connected to <%a%s>." Domain_name.pp domain_name path) ;
      Fetch.fetch_v1 ~capabilities ?want ~host:domain_name path flow store
        access fetch_cfg (fun (payload, off, len) ->
            let v = String.sub payload off len in
            pack (Some (v, 0, len)))
      >>= fun () ->
      Log.debug (fun m -> m "PACK file downloaded!") ;
      pack None ;
      (* End of pack! *)
      Conduit_lwt.close flow >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failwithf "%a" Conduit_lwt.pp_error err)

let resolvers =
  Conduit_lwt.register_resolver ~key:Conduit_lwt_unix_tcp.endpoint
    (Conduit_lwt_unix_tcp.resolv_conf ~port:9418)
    Conduit.empty

let fetch uri ?(version = `V1) ?(capabilities = []) want path filename =
  let light_load uid = lightly_load lwt path uid |> Scheduler.prj in
  let heavy_load uid = heavily_load lwt path uid |> Scheduler.prj in
  let access = access lwt path in
  let store = store_inj (Hashtbl.create 0x100) in
  match (version, Uri.scheme uri, Uri.host uri, Uri.path uri) with
  | `V1, Some "git", Some domain_name, path ->
      let fetch_cfg = Nss.Fetch.configuration capabilities in
      let domain_name = Domain_name.(host_exn (of_string_exn domain_name)) in
      let fiber =
        let open Lwt.Infix in
        let stream, pack = Lwt_stream.create () in
        Lwt.catch
          (fun () ->
            Lwt.join
              [
                connect ~capabilities path ~resolvers ~want domain_name store
                  access fetch_cfg pack;
                Crt.run ~light_load ~heavy_load
                  (fun () -> Lwt_stream.get stream)
                  filename;
              ]
            >>= Lwt.return_ok)
          (function
            | Failure err -> Lwt.return_error (R.msgf "%s" err)
            | exn ->
                Printexc.print_backtrace stderr ;
                Log.err (fun m ->
                    m "Got an unexpected error: %s" (Printexc.to_string exn)) ;
                Lwt.return_error (R.msgf "%s" (Printexc.to_string exn))) in
      Log.debug (fun m -> m "Launch the lwt fiber.") ;
      Lwt_main.run fiber
  | _ -> R.error_msgf "Invalid uri: %a" Uri.pp uri

let fetch all thin _depth no_done no_progress level style_renderer repository
    want path filename =
  let capabilities =
    let ( $ ) x f = f x in
    [ `Multi_ack; `Multi_ack_detailed; `Side_band; `Side_band_64k; `Ofs_delta ]
    $ fun caps ->
    if thin
    then `Thin_pack :: caps
    else
      caps $ fun caps ->
      if no_progress
      then `No_progress :: caps
      else caps $ fun caps -> if no_done then `No_done :: caps else caps in
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Fmt.stderr ()) ;
  let want =
    match (all, want) with
    | true, _ -> `All
    | false, _ :: _ -> `Some want
    | false, [] -> `All in
  fetch repository ~capabilities want path filename

open Cmdliner

let uri =
  let parser = R.ok <.> Uri.of_string in
  let pp = Uri.pp in
  Arg.conv (parser, pp)

let reference =
  let parser = R.ok <.> Ref.v in
  Arg.conv (parser, Ref.pp)

let directory =
  let parser x =
    match Fpath.of_string x with
    | Ok v when Sys.is_directory x && Fpath.is_abs v -> R.ok v
    | Ok v -> R.error_msgf "Invalid directory <%a>" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv (parser, pp)

let filename =
  let parser = Fpath.of_string in
  let pp = Fpath.pp in
  Arg.conv (parser, pp)

let repository =
  let doc = "The URL to the remote repository." in
  Arg.(required & pos 0 (some uri) None & info [] ~docv:"<repository>" ~doc)

let references =
  let doc =
    "The remote heads to update from. This is relative to FETCH_DIR (e.g. \
     \"HEAD\", \"refs/heads/master\"). When unspecified, update from all heads \
     the remote side has." in
  Arg.(value & pos_right 0 reference [] & info [] ~docv:"<refs>..." ~doc)

let local =
  let env = Arg.env_var "FETCH_DIR" in
  let doc =
    "Set the path to the repository. This can also be controlled by settings \
     the FETCH_DIR environment variable. It must be an absolute path." in
  Arg.(value & opt directory (Fpath.v ".") & info ~env [ "git-dir" ] ~doc)

let verbosity =
  let env = Arg.env_var "FETCH_LOGS" in
  Logs_cli.level ~env ()

let renderer =
  let env = Arg.env_var "FETCH_FMT" in
  Fmt_cli.style_renderer ~env ()

let no_progress =
  let doc = "Do not show the progress." in
  Arg.(value & flag & info [ "no-progress" ] ~doc)

let no_done =
  let doc = "Reduce slightly latency over HTTP." in
  Arg.(value & flag & info [ "no-done" ] ~doc)

let depth =
  let doc = "Limit fetching to ancestor-chains not longer than $(i,n)." in
  Arg.(value & opt (some int) None & info [ "depth" ] ~doc)

let thin =
  let doc =
    "Fetch a \"thin\" pack, which records objects in $(i,deltified) form based \
     on objects not included in the pack to reduce network traffic." in
  Arg.(value & flag & info [ "thin" ] ~doc)

let all =
  let doc = "Fetch all remote refs." in
  Arg.(value & flag & info [ "all" ] ~doc)

let output =
  let doc = "PACK file output." in
  Arg.(required & opt (some filename) None & info [ "o"; "output" ] ~doc)

let fetch =
  let doc = "Receive missing objects from another repository." in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Invoke $(b,git-upload-pack) on a possibly remote repository and asks \
         it to send objects missing from this repository. to update named \
         heads. The list of commits available locally is found out by scanning \
         the local refs/ hierarchy and sent to $(b,git-upload-pack) running on \
         the other end.";
      `P
        "This command degenerates to download everything to complete the asked \
         refs from the remote side when the local side does not have common \
         ancestor commit.";
    ] in
  ( Term.(
      const fetch
      $ all
      $ thin
      $ depth
      $ no_done
      $ no_progress
      $ verbosity
      $ renderer
      $ repository
      $ references
      $ local
      $ output),
    Term.info "fetch" ~doc ~exits ~man )
