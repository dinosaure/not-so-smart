open Rresult
open Lwt_backend
open Store_backend

let ( <.> ) f g = fun x -> f (g x)
let identity x = x

let src = Logs.Src.create "fetch"

module Log = (val Logs.src_log src : Logs.LOG)

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let references want have =
  match want with
  | `None -> []
  | `All -> List.fold_left (fun acc -> function
      | (uid, _, false) -> uid :: acc
      | _ -> acc) [] have
  | `Some refs ->
    let fold acc (uid, refname, peeled) =
      if List.exists ((=) refname) refs && not peeled
      then uid :: acc else acc in
    List.fold_left fold [] have

module V1 = struct
  let fetch ~capabilities ?want:(refs = `None) ~host path
      flow store access fetch_cfg =
    let push_stdout, _no_progress =
      if List.exists ((=) `No_progress) capabilities
      then ignore, true else
        let mutex = Lwt_mutex.create () in
        let print v =
          Lwt.async @@ fun () ->
          Lwt_mutex.with_lock mutex @@ fun () ->
          Fmt.pr "%s%!" v ;
          if not (String.contains v '\r') then Fmt.pr "\n%!" ;
          Lwt.return_unit in
        print, false in
    let prelude ctx =
      let open Smart in
      let* () =
        send ctx proto_request (Proto_request.upload_pack ~host ~version:1 path)
      in
      let* v = recv ctx advertised_refs in
      Log.debug (fun m -> m "Got %a" Smart.Advertised_refs.pp v) ;
      let uids = references refs (Smart.Advertised_refs.refs v) in
      (* update ctx (Smart.Advertised_refs.capabilities v) ; *)
      return uids in
    let pack ctx =
      let open Smart in
      recv ctx
        (pack ~push_stdout ~push_stderr:(Fmt.epr "%s\n%!")
           ~push_pack:ignore) in
    let ctx = Smart.make capabilities in
    let compare (a : hex) (b : hex) = String.compare (a :> string) (b :> string) in
    let negotiator = Neg.negotiator ~compare in
    let open Lwt.Infix in
    Neg.tips lwt access store negotiator |> lwt_prj >>= fun () ->
    Neg.run lwt lwt_fail lwt_io flow (prelude ctx) |> lwt_prj >>= fun uids ->
    (* XXX(dinosaure): unsafe part. *)
    let uids = List.map of_hex uids in
    Neg.find_common lwt lwt_io flow fetch_cfg access store negotiator ctx uids
    |> lwt_prj
    >>= fun res ->
    if res < 0 then Log.warn (fun m -> m "No common commits") ;
    Neg.run lwt lwt_fail lwt_io flow (pack ctx) |> lwt_prj

  let connect ~capabilities path ~resolvers ?want domain_name store access
      fetch_cfg =
    let open Lwt.Infix in
    Log.debug (fun m -> m "Try to connect to <%a>." Domain_name.pp domain_name) ;
    Conduit_lwt_unix.flow resolvers domain_name >>= function
    | Error err ->
      Log.err (fun m -> m "<%a> unavailable." Domain_name.pp domain_name) ;
      failwithf "%a" Conduit_lwt_unix.pp_error err
    | Ok flow -> (
        Log.info (fun m -> m "Connected to <%a/%s>." Domain_name.pp domain_name path) ;
        fetch ~capabilities ?want ~host:domain_name path flow store access
          fetch_cfg
        >>= fun () ->
        Conduit_lwt.close flow >>= function
        | Ok () -> Lwt.return_unit
        | Error err -> failwithf "%a" Conduit_lwt.pp_error err)
end

let resolvers =
  Conduit_lwt.register_resolver ~key:Conduit_lwt_unix_tcp.endpoint
    (Conduit_lwt_unix_tcp.resolv_conf ~port:9418)
    Conduit.empty

let multi_ack capabilities =
  match
    ( List.exists (( = ) `Multi_ack) capabilities,
      List.exists (( = ) `Multi_ack_detailed) capabilities )
  with
  | true, true | false, true -> `Detailed
  | true, false -> `Some
  | false, false -> `None

let fetch uri ?(version= `V1) ?(no_done = false) ?(capabilities = []) want path =
  let access = access lwt path in
  let store = store_inj (Hashtbl.create 0x100) in
  match version, Uri.scheme uri, Uri.host uri, Uri.path uri with
  | `V1, Some "git", Some domain_name, path ->
      let fetch_cfg =
        { Neg.stateless = false; multi_ack = multi_ack capabilities; no_done
        ; to_hex= to_hex; of_hex= of_hex }
      in
      let domain_name = Domain_name.(host_exn (of_string_exn domain_name)) in
      let fiber =
        let open Lwt.Infix in
        Lwt.catch
          (fun () ->
            V1.connect ~capabilities path ~resolvers ~want domain_name
              store access fetch_cfg
            >>= Lwt.return_ok)
          (function
            | Failure err -> Lwt.return_error (R.msgf "%s" err)
            | exn ->
              Log.err (fun m -> m "Got an unexpected error: %s" (Printexc.to_string exn)) ;
              Lwt.return_error (R.msgf "%s" (Printexc.to_string exn)))
      in
      Log.debug (fun m -> m "Launch the lwt fiber.") ;
      Lwt_main.run fiber
  | _ -> R.error_msgf "Invalid uri: %a" Uri.pp uri

let fetch all thin _depth no_done no_progress level style_renderer repository want path =
  let capabilities =
    let ( $ ) x f = f x in
    [ `Multi_ack; `Multi_ack_detailed
    ; `Side_band; `Side_band_64k
    ; `Ofs_delta ]
    $ fun caps -> if thin then `Thin_pack :: caps else caps
    $ fun caps -> if no_progress then `No_progress :: caps else caps
    $ fun caps -> if no_done then `No_done :: caps else caps in
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Fmt.stderr ()) ;
  let want = match all, want with
    | true, _ -> `All
    | false, _ :: _ -> `Some want
    | false, [] -> `All in
  fetch repository ~no_done ~capabilities want path

open Cmdliner

let uri =
  let parser = R.ok <.> Uri.of_string in
  let pp = Uri.pp in
  Arg.conv (parser, pp)

let reference =
  let parser x = Fpath.of_string x >>| Fpath.to_string in
  let pp = Fmt.string in
  Arg.conv (parser, pp)

let directory =
  let parser x = match Fpath.of_string x with
    | Ok v when Sys.is_directory x && Fpath.is_abs v -> R.ok v
    | Ok v -> R.error_msgf "Invalid directory <%a>" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv (parser, pp)

let repository =
  let doc = "The URL to the remote repository." in
  Arg.(required & pos 0 (some uri) None & info [] ~docv:"<repository>" ~doc)

let references =
  let doc = "The remote heads to update from. This is relative to FETCH_DIR (e.g. \"HEAD\", \"refs/heads/master\"). \
             When unspecified, update from all heads the remote side has." in
  Arg.(value & pos_right 1 reference [] & info [] ~docv:"<refs>..." ~doc)

let local =
  let env = Arg.env_var "FETCH_DIR" in
  let doc = "Set the path to the repository. This can also be controlled by settings the FETCH_DIR environment variable. \
             It must be an absolute path." in
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
  let doc = "Fetch a \"thin\" pack, which records objects in $(i,deltified) form based on objects not included in \
             the pack to reduce network traffic." in
  Arg.(value & flag & info [ "thin" ] ~doc)

let all =
  let doc = "Fetch all remote refs." in
  Arg.(value & flag & info [ "all" ] ~doc)

let fetch =
  let doc = "Receive missing objects from another repository." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Invoke $(b,git-upload-pack) on a possibly remote repository and asks it to send objects missing from this repository. \
          to update named heads. The list of commits available locally is found out by scanning the local refs/ hierarchy and
          sent to $(b,git-upload-pack) running on the other end."
    ; `P "This command degenerates to download everything to complete the asked refs from the remote side when the local side does \
          not have common ancestor commit." ] in
  Term.(const fetch $ all $ thin $ depth $ no_done $ no_progress $ verbosity $ renderer $ repository $ references $ local),
  Term.info "fetch" ~doc ~exits ~man

let () = Term.(exit @@ eval fetch)
