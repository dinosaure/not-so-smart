open Rresult
open Lwt_backend
open Store_backend

let src = Logs.Src.create "push"

module Log = (val Logs.src_log src : Logs.LOG)

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let ( <.> ) f g x = f (g x)

module Crt = struct
  open Lwt.Infix

  module Verbose = struct
    type 'a fiber = 'a Lwt.t

    let succ () = Lwt.return_unit

    let print () = Lwt.return_unit

    let flush () = Lwt.return_unit
  end

  module Delta = Carton_lwt.Enc.Delta (Uid) (Verbose)

  let deltify ~light_load ~heavy_load ?(threads = 4) (uids : Uid.t list) =
    let fold (uid : Uid.t) =
      light_load uid >|= fun (kind, length) ->
      Carton_lwt.Enc.make_entry ~kind ~length uid in
    Lwt_list.map_p fold uids >|= Array.of_list >>= fun entries ->
    Delta.delta
      ~threads:(List.init threads (fun _thread -> heavy_load))
      ~weight:10 ~uid_ln:Uid.length entries
    >>= fun targets -> Lwt.return (entries, targets)

  let header = Bigstringaf.create 12

  let pack ~(heavy_load : Uid.t Carton_lwt.Enc.load) stream targets =
    let offsets = Hashtbl.create (Array.length targets) in
    let find uid =
      match Hashtbl.find offsets uid with
      | v -> Lwt.return_some v
      | exception Not_found -> Lwt.return_none in
    let uid =
      { Carton.Enc.uid_ln = Uid.length; Carton.Enc.uid_rw = Uid.to_raw_string }
    in
    let b =
      {
        Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.q = De.Queue.create 0x10000;
        Carton.Enc.w = De.make_window ~bits:15;
      } in
    let ctx = ref Digestif.SHA1.empty in
    let cursor = ref 0 in
    Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12 ;
    let header = Bigstringaf.to_string header in
    stream (Some header) ;
    ctx := Digestif.SHA1.feed_string !ctx header ;
    cursor := !cursor + 12 ;
    let encode_targets targets =
      Log.debug (fun m ->
          m "Start to encode %d object(s)." (Array.length targets)) ;
      let encode_target idx =
        Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor ;
        Carton_lwt.Enc.encode_target ~b ~find ~load:heavy_load ~uid
          targets.(idx) ~cursor:!cursor
        >>= fun (len, encoder) ->
        let rec go encoder =
          match Carton.Enc.N.encode ~o:b.o encoder with
          | `Flush (encoder, len) ->
              let payload = Bigstringaf.substring b.o ~off:0 ~len in
              stream (Some payload) ;
              ctx := Digestif.SHA1.feed_string !ctx payload ;
              cursor := !cursor + len ;
              let encoder =
                Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
              go encoder
          | `End -> Lwt.return () in
        let payload = Bigstringaf.substring b.o ~off:0 ~len in
        stream (Some payload) ;
        ctx := Digestif.SHA1.feed_string !ctx payload ;
        cursor := !cursor + len ;
        let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
        go encoder in
      let rec go idx =
        if idx < Array.length targets
        then encode_target idx >>= fun () -> go (succ idx)
        else Lwt.return () in
      go 0 in
    encode_targets targets >>= fun () ->
    let uid = Digestif.SHA1.((to_raw_string <.> get) !ctx) in
    stream (Some uid) ;
    stream None ;
    Lwt.return_unit
end

let pack ~light_load ~heavy_load uids =
  let open Lwt.Infix in
  Crt.deltify ~light_load ~heavy_load uids >>= fun (_, targets) ->
  let stream, pusher = Lwt_stream.create () in
  let stream () = Lwt_stream.get stream in
  Lwt.return (stream, Crt.pack ~heavy_load pusher targets)

module Tuyau = struct
  type t = Conduit_lwt.flow

  type +'a fiber = 'a Lwt.t

  include Conduit_lwt
end

module Push = Nss.Push.Make (Scheduler) (Lwt) (Tuyau) (Uid) (Ref)

let connect ~capabilities path ~resolvers cmds domain_name store access push_cfg
    pack =
  let open Lwt.Infix in
  Log.debug (fun m -> m "Try to connect to <%a>." Domain_name.pp domain_name) ;
  Conduit_lwt_unix.flow resolvers domain_name >>= function
  | Error err ->
      Log.err (fun m -> m "<%a> unavailable." Domain_name.pp domain_name) ;
      failwithf "%a" Conduit_lwt_unix.pp_error err
  | Ok flow -> (
      Log.info (fun m ->
          m "Connected to <%a%s>." Domain_name.pp domain_name path) ;
      Push.push ~capabilities cmds ~host:domain_name path flow store access
        push_cfg pack
      >>= fun () ->
      Conduit_lwt.close flow >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failwithf "%a" Conduit_lwt.pp_error err)

let resolvers =
  Conduit_lwt.register_resolver ~key:Conduit_lwt_unix_tcp.endpoint
    (Conduit_lwt_unix_tcp.resolv_conf ~port:9418)
    Conduit.empty

let push uri ?(version = `V1) ?(capabilities = []) cmds path =
  let access =
    {
      Sigs.get = get_object_for_packer lwt path;
      Sigs.parents = (fun _uid _store -> assert false);
      Sigs.deref = deref lwt path;
      Sigs.locals = (fun _store -> assert false);
    } in
  let light_load uid = lightly_load lwt path uid |> Scheduler.prj in
  let heavy_load uid = heavily_load lwt path uid |> Scheduler.prj in
  let pack = pack ~light_load ~heavy_load in
  let store = store_inj (Hashtbl.create 0x100) in
  match (version, Uri.scheme uri, Uri.host uri, Uri.path uri) with
  | `V1, Some "git", Some domain_name, path ->
      let push_cfg = Nss.Push.configuration () in
      let domain_name = Domain_name.(host_exn (of_string_exn domain_name)) in
      let fiber =
        let open Lwt.Infix in
        Lwt.catch
          (fun () ->
            connect ~capabilities path ~resolvers cmds domain_name store access
              push_cfg pack
            >>= Lwt.return_ok)
          (function
            | Failure err -> Lwt.return_error (R.msgf "%s" err)
            | exn ->
                Log.err (fun m ->
                    m "Got an unexpected error: %s" (Printexc.to_string exn)) ;
                Lwt.return_error (R.msgf "%s" (Printexc.to_string exn))) in
      Log.debug (fun m -> m "Launch the lwt fiber.") ;
      Lwt_main.run fiber
  | _ -> R.error_msgf "Invalid uri: %a" Uri.pp uri

let push level style_renderer repository cmds path =
  let capabilities =
    [ `Report_status; `Multi_ack; `Multi_ack_detailed; `Ofs_delta ] in
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Fmt.stderr ()) ;
  Logs.debug (fun m -> m "Process %d command(s)." (List.length cmds)) ;
  push repository ~capabilities cmds path

open Cmdliner

let uri =
  let parser = R.ok <.> Uri.of_string in
  let pp = Uri.pp in
  Arg.conv (parser, pp)

let command =
  let parser x =
    match Astring.String.cut ~sep:":" x with
    | Some ("", remote) -> R.ok (`Delete (Ref.v remote))
    | Some (local, "") -> R.ok (`Create (Ref.v local))
    | Some (local, remote) -> R.ok (`Update (Ref.v local, Ref.v remote))
    | None -> R.ok (`Update (Ref.v x, Ref.v x)) in
  let pp ppf = function
    | `Delete reference -> Fmt.pf ppf ":%a" Ref.pp reference
    | `Create reference -> Fmt.pf ppf "%a:" Ref.pp reference
    | `Update (a, b) ->
        if a = b then Ref.pp ppf a else Fmt.pf ppf "%a:%a" Ref.pp a Ref.pp b
  in
  Arg.conv (parser, pp) ~docv:"<ref>"

let directory =
  let parser x =
    match Fpath.of_string x with
    | Ok v when Sys.is_directory x && Fpath.is_abs v -> R.ok v
    | Ok v -> R.error_msgf "Invalid directory <%a>" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv (parser, pp)

let repository =
  let doc = "The URL to the remote repository." in
  Arg.(required & pos 0 (some uri) None & info [] ~docv:"<repository>" ~doc)

let local =
  let env = Arg.env_var "PUSH_DIR" in
  let doc =
    "Set the path to the repository. This can also be controlled by settings \
     the PUSH_DIR environment variable. It must be an absolute path." in
  Arg.(value & opt directory (Fpath.v ".") & info ~env [ "git-dir" ] ~doc)

let verbosity =
  let env = Arg.env_var "PUSH_LOGS" in
  Logs_cli.level ~env ()

let renderer =
  let env = Arg.env_var "PUSH_FMT" in
  Fmt_cli.style_renderer ~env ()

let commands =
  let doc = "Specify what destination is destination of a push operation." in
  Arg.(value & pos_right 0 command [] & info [] ~docv:"<refs>..." ~doc)

let push =
  let doc = "Update remote refs along with associated objects." in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Updates remote refs using local refs, while sending objects necessary \
         to complete the given refs.";
    ] in
  ( Term.(const push $ verbosity $ renderer $ repository $ commands $ local),
    Term.info "push" ~doc ~exits ~man )
