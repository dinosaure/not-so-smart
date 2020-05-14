open Rresult
open Lwt_backend
open Store_backend

let src = Logs.Src.create "push"

module Log = (val Logs.src_log src : Logs.LOG)

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let identity x = x

let ( <.> ) f g = fun x -> f (g x)

let run = Neg.run

type configuration = { stateless : bool }

module Crt = struct
  open Lwt.Infix

  module Verbose = struct
    type 'a fiber = 'a Lwt.t

    let succ () = Lwt.return_unit

    let print () = Lwt.return_unit

    let flush () = Lwt.return_unit
  end

  module Lwt_scheduler = Carton.Make (struct
    type +'a t = 'a Lwt.t
  end)

  let lwt_inj = Lwt_scheduler.inj

  let lwt_prj = Lwt_scheduler.prj

  let lwt =
    let open Lwt_scheduler in
    let open Lwt.Infix in
    {
      Carton.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      Carton.return = (fun x -> inj (Lwt.return x));
    }

  let deltify ~light_load ~heavy_load ?(threads = 4) (uids : Uid.t list) =
    let fold (uid : Uid.t) =
      light_load uid |> lwt_prj >|= fun (kind, length) ->
      Carton.Enc.make_entry ~kind ~length uid in
    Lwt_list.map_p fold uids >|= Array.of_list >>= fun entries ->
    let module Delta = Carton.Enc.Delta (Lwt_scheduler) (Lwt_io) (Uid) (Verbose)
    in
    Delta.delta
      ~threads:(List.init threads (fun _thread -> heavy_load))
      ~weight:10 ~uid_ln:Uid.length entries
    >>= fun targets -> Lwt.return (entries, targets)

  let header = Bigstringaf.create 12

  let pack ~(heavy_load : (Uid.t, Lwt_scheduler.t) Carton.Enc.load) stream
      targets =
    let offsets = Hashtbl.create (Array.length targets) in
    let find uid =
      match Hashtbl.find offsets uid with
      | v -> lwt_inj (Lwt.return_some v)
      | exception Not_found -> lwt_inj Lwt.return_none in
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
      Log.debug (fun m -> m "Start to encode %d object(s)." (Array.length targets)) ;
      let encode_target idx =
        Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor ;
        Carton.Enc.encode_target lwt ~b ~find ~load:heavy_load ~uid
          targets.(idx) ~cursor:!cursor
        |> lwt_prj
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

module V1 = struct
  let deref path reference =
    let failwithf fmt = Fmt.kstrf (fun err -> raise (Failure err)) fmt in
    let fiber =
      let open Bos in
      OS.Dir.set_current path >>= fun () ->
      OS.Cmd.run_out ~err:OS.Cmd.err_null
        Cmd.(v "git" % "show-ref" % "--hash" % reference)
      |> OS.Cmd.out_string ~trim:true in
    match fiber with
    | Ok (uid, (_, `Exited 0)) -> Uid.of_hex uid
    | Ok _ -> failwithf "Local reference <%s> not found." reference
    | Error err ->
        Log.err (fun m -> m "Got an error [deref]: %a" R.pp_msg err) ;
        failwithf "%a" R.pp_msg err

  let to_commands ~capabilities path cmds have =
    let fold acc = function
      | `Create reference ->
          Log.debug (fun m -> m "Create an new reference.") ;
          let uid = deref path reference in
          Smart.Commands.create uid reference :: acc
      | `Delete reference ->
          Log.debug (fun m -> m "Delete an old reference.") ;
          let uid, _, _ =
            List.find
              (fun (_, reference', peeled) ->
                String.equal reference reference' && peeled = false)
              have in
          let uid = Uid.of_hex uid in
          Smart.Commands.delete uid reference :: acc
      | `Update (local, remote) ->
          Log.debug (fun m -> m "Update %s:%s" local remote) ;
          let uid_new = deref path local in
          let uid_old, _, _ =
            List.find
              (fun (_, reference', peeled) ->
                String.equal remote reference' && peeled = false)
              have in
          let uid_old = Uid.of_hex uid_old in
          Smart.Commands.update uid_old uid_new remote :: acc in
    match List.fold_left fold [] cmds with
    | [] -> None
    | head :: tail -> Some (Smart.Commands.v ~capabilities ~others:tail head)

  let packer have cmds =
    let module Set = Set.Make (Uid) in
    let exclude =
      let fold acc (uid, _, _) = Set.add (Uid.of_hex uid) acc in
      List.fold_left fold Set.empty have in
    let exclude =
      let fold acc = function
        | Smart.Commands.Create _ -> acc
        | Smart.Commands.Delete (uid, _) -> Set.add (Uid.of_hex uid) acc
        | Smart.Commands.Update (uid, _, _) -> Set.add (Uid.of_hex uid) acc
      in
      List.fold_left fold exclude cmds in
    let sources =
      let fold acc = function
        | Smart.Commands.Update (_, uid, _) -> Set.add (Uid.of_hex uid) acc
        | Smart.Commands.Create (uid, _) -> Set.add (Uid.of_hex uid) acc
        | Smart.Commands.Delete _ -> acc in
      List.fold_left fold Set.empty cmds in
    (Set.elements exclude, Set.elements sources)

  let pack ~light_load ~heavy_load uids =
    let open Lwt.Infix in
    Crt.deltify ~light_load ~heavy_load uids >>= fun (_, targets) ->
    let stream, pusher = Lwt_stream.create () in
    Lwt.return (stream, Crt.pack ~heavy_load pusher targets)

  let push ~light_load ~heavy_load ~to_commands ~capabilities:caps cmds ~host
      path flow store access push_cfg =
    let fiber ctx =
      let open Smart in
      let* () =
        send ctx proto_request
          (Proto_request.receive_pack ~host ~version:1 path)
      in
      let* v = recv ctx advertised_refs in
      update ctx (Smart.Advertised_refs.capabilities v) ;
      let have = Smart.Advertised_refs.refs v in
      match
        Option.map
          (Smart.Commands.map ~fuid:Uid.to_hex ~fref:identity)
          (to_commands ~capabilities:caps cmds have)
      with
      | None ->
        Log.debug (fun m -> m "Nothing to do with our peer.") ;
        send ctx flush () >>= fun () -> return None
      | Some cmds ->
          send ctx commands cmds >>= fun () ->
          let cmds = Smart.Commands.commands cmds in
          return (Some (have, cmds)) in
    let open Lwt.Infix in
    let ctx = Smart.make caps in
    run lwt lwt_fail lwt_io flow (fiber ctx) |> lwt_prj >>= function
    | None -> Lwt.return_unit
    | Some (have, cmds) -> (
        let exclude, sources = packer have cmds in
        Pck.packer lwt ~compare:Uid.compare access store ~exclude ~sources
        |> lwt_prj
        >>= fun uids ->
        Log.debug (fun m -> m "Prepare a pack of %d object(s)." (List.length uids)) ;
        pack ~light_load ~heavy_load uids >>= fun (stream, th) ->
        let pack = Smart.send_pack ~stateless:push_cfg.stateless false (* side-band *) in
        let rec go () =
          Lwt_stream.get stream >>= function
          | None ->
            let report_status = Smart.shared `Report_status ctx in
            Log.debug (fun m -> m "report-status capability: %b." report_status) ;
            if report_status
            then run lwt lwt_fail lwt_io flow Smart.(recv ctx status) |> lwt_prj
            else
              let cmds = List.map R.ok cmds in
              Lwt.return (Smart.Status.v cmds)
          | Some payload ->
              run lwt lwt_fail lwt_io flow Smart.(send ctx pack payload) |> lwt_prj
              >>= fun () -> go () in
        Lwt.async (fun () -> th) ;
        go () >>= fun status ->
        match Smart.Status.to_result status with
        | Ok () ->
          Log.debug (fun m -> m "Push is done!") ;
          Log.info (fun m -> m "%a" Smart.Status.pp status) ;
          Lwt.return ()
        | Error _ -> assert false)

  let connect ~light_load ~heavy_load ~to_commands ~capabilities path ~resolvers
      cmds domain_name store access push_cfg =
    let open Lwt.Infix in
    Log.debug (fun m -> m "Try to connect to <%a>." Domain_name.pp domain_name) ;
    Conduit_lwt_unix.flow resolvers domain_name >>= function
    | Error err ->
        Log.err (fun m -> m "<%a> unavailable." Domain_name.pp domain_name) ;
        failwithf "%a" Conduit_lwt_unix.pp_error err
    | Ok flow -> (
        Log.info (fun m ->
            m "Connected to <%a%s>." Domain_name.pp domain_name path) ;
        push ~light_load ~heavy_load ~to_commands ~capabilities cmds
          ~host:domain_name path flow store access push_cfg
        >>= fun () ->
        Conduit_lwt.close flow >>= function
        | Ok () -> Lwt.return_unit
        | Error err -> failwithf "%a" Conduit_lwt.pp_error err)
end

let resolvers =
  Conduit_lwt.register_resolver ~key:Conduit_lwt_unix_tcp.endpoint
    (Conduit_lwt_unix_tcp.resolv_conf ~port:9418)
    Conduit.empty

let push uri ?(version = `V1) ?(capabilities = []) cmds path =
  let access =
    {
      Sigs.exists = get_object_for_packer lwt path;
      Sigs.parents = (fun _uid _store -> assert false);
      Sigs.deref = (fun _ref _store -> assert false);
      Sigs.locals = (fun _store -> assert false);
    } in
  let light_load = lightly_load Crt.lwt path in
  let heavy_load = heavily_load Crt.lwt path in
  let to_commands = V1.to_commands path in
  let store = store_inj (Hashtbl.create 0x100) in
  match (version, Uri.scheme uri, Uri.host uri, Uri.path uri) with
  | `V1, Some "git", Some domain_name, path ->
      let push_cfg = { stateless = true } in
      let domain_name = Domain_name.(host_exn (of_string_exn domain_name)) in
      let fiber =
        let open Lwt.Infix in
        Lwt.catch
          (fun () ->
            V1.connect ~light_load ~heavy_load ~to_commands ~capabilities path
              ~resolvers cmds domain_name store access push_cfg
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
  let parser x = match Astring.String.cut ~sep:":" x with
    | Some ("", remote) -> R.ok (`Delete remote)
    | Some (local, "") -> R.ok (`Create local)
    | Some (local, remote) -> R.ok (`Update (local, remote))
    | None -> R.ok (`Update (x, x)) in
  let pp ppf = function
    | `Delete reference -> Fmt.pf ppf ":%s" reference
    | `Create reference -> Fmt.pf ppf "%s:" reference
    | `Update (a, b) -> if a = b then Fmt.string ppf a else Fmt.pf ppf "%s:%s" a b in
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
  ( Term.(
      const push
      $ verbosity
      $ renderer
      $ repository
      $ commands
      $ local),
    Term.info "push" ~doc ~exits ~man )
