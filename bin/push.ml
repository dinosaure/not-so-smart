open Rresult
open Lwt_backend
open Store_backend

let src = Logs.Src.create "push"

module Log = (val Logs.src_log src : Logs.LOG)

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let run = Neg.run

type configuration = { stateless : bool }

let load_object _path _uid = assert false

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

  let heavily_load_object _path _uid = assert false

  let deltify ?(threads = 4) path (uids : Uid.t list) =
    let fold (uid : Uid.t) =
      load_object path uid >>= function
      | Some (kind, length) ->
          Lwt.return (Carton.Enc.make_entry ~kind ~length uid)
      | None -> failwithf "Object <%s> not found" (uid :> string) in
    Lwt_list.map_p fold uids >|= Array.of_list >>= fun entries ->
    let module Delta = Carton.Enc.Delta (Lwt_scheduler) (Lwt_io) (Uid) (Verbose)
    in
    let targets =
      Delta.delta
        ~threads:(List.init threads (fun _ -> heavily_load_object path))
        ~weight:10 ~uid_ln:Uid.length entries in
    Lwt.return (entries, targets)

  let header = Bigstringaf.create 12

  let pack path stream targets =
    let offsets = Hashtbl.create (Array.length targets) in
    let find uid =
      match Hashtbl.find offsets uid with
      | v -> lwt_inj (Lwt.return_some v)
      | exception Not_found -> lwt_inj Lwt.return_none in
    let uid =
      {
        Carton.Enc.uid_ln = Uid.length;
        Carton.Enc.uid_rw = (fun _ -> assert false);
      } in
    let load uid = lwt_inj (heavily_load_object path uid) in
    let b =
      {
        Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.q = De.Queue.create 0x10000;
        Carton.Enc.w = De.make_window ~bits:15;
      } in
    Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12 ;
    stream (Some (header, 0, 12)) ;
    let encode_targets targets =
      let cursor = ref 12 in
      let encode_target idx =
        Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor ;
        Carton.Enc.encode_target lwt ~b ~find ~load ~uid targets.(idx)
          ~cursor:!cursor
        |> lwt_prj
        >>= fun (len, encoder) ->
        let rec go encoder =
          match Carton.Enc.N.encode ~o:b.o encoder with
          | `Flush (encoder, len) ->
              stream (Some (b.o, 0, len)) ;
              cursor := !cursor + len ;
              let encoder =
                Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
              go encoder
          | `End -> Lwt.return () in
        stream (Some (b.o, 0, len)) ;
        cursor := !cursor + len ;
        let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
        go encoder in
      let rec go idx =
        if idx < Array.length targets - 1
        then encode_target idx >>= fun () -> go (succ idx)
        else Lwt.return () in
      go 0 in
    encode_targets targets
end

module V1 = struct
  let to_commands ~capabilities cmds have =
    let fold acc = function
      | `Create (uid, reference) -> Smart.Commands.create uid reference :: acc
      | `Delete (uid, reference) ->
          let uid', _, _ =
            List.find
              (fun (_, reference', peeled) ->
                String.equal reference reference' && peeled = false)
              have in
          if String.equal uid uid'
          then Smart.Commands.delete uid reference :: acc
          else (
            Log.warn (fun m ->
                m "On %s (deleting), uids mismatch local:<%s>, remote:<%s>"
                  reference uid uid') ;
            acc)
      | `Update (uid_old, uid_new, reference) ->
          let uid', _, _ =
            List.find
              (fun (_, reference', peeled) ->
                String.equal reference reference' && peeled = false)
              have in
          if String.equal uid' uid_old
          then Smart.Commands.update uid_old uid_new reference :: acc
          else (
            Log.warn (fun m ->
                m "On %s (updating), uids mismatch local:<%s>, remote:<%s>"
                  reference uid_old uid') ;
            acc) in
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

  let pack _ = assert false

  let push ~capabilities:caps cmds ~host path flow store access push_cfg =
    let fiber ctx =
      let open Smart in
      let* () =
        send ctx proto_request
          (Proto_request.receive_pack ~host ~version:1 path)
      in
      let* v = recv ctx advertised_refs in
      update ctx (Smart.Advertised_refs.capabilities v) ;
      let have = Smart.Advertised_refs.refs v in
      match to_commands ~capabilities:caps cmds have with
      | None -> send ctx flush () >>= fun () -> return None
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
        pack uids >>= fun stream ->
        let pack = Smart.send_pack ~stateless:push_cfg.stateless false in
        let fiber ctx buffer ~off ~len =
          let open Smart in
          send ctx pack (buffer, off, len) in
        let rec go () =
          stream () >>= function
          | None ->
              run lwt lwt_fail lwt_io flow Smart.(recv ctx status) |> lwt_prj
          | Some (buf, off, len) ->
              run lwt lwt_fail lwt_io flow (fiber ctx buf ~off ~len) |> lwt_prj
              >>= fun () -> go () in
        go () >>= fun status ->
        match Smart.Status.to_result status with
        | Ok () -> Lwt.return ()
        | Error _ -> assert false)

  let connect ~capabilities path ~resolvers cmds domain_name store access
      push_cfg =
    let open Lwt.Infix in
    Log.debug (fun m -> m "Try to connect to <%a>." Domain_name.pp domain_name) ;
    Conduit_lwt_unix.flow resolvers domain_name >>= function
    | Error err ->
        Log.err (fun m -> m "<%a> unavailable." Domain_name.pp domain_name) ;
        failwithf "%a" Conduit_lwt_unix.pp_error err
    | Ok flow -> (
        Log.info (fun m ->
            m "Connected to <%a/%s>." Domain_name.pp domain_name path) ;
        push ~capabilities cmds ~host:domain_name path flow store access
          push_cfg
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
  let store = store_inj (Hashtbl.create 0x100) in
  match (version, Uri.scheme uri, Uri.host uri, Uri.path uri) with
  | `V1, Some "git", Some domain_name, path ->
      let push_cfg = { stateless = true } in
      let domain_name = Domain_name.(host_exn (of_string_exn domain_name)) in
      let fiber =
        let open Lwt.Infix in
        Lwt.catch
          (fun () ->
            V1.connect ~capabilities path ~resolvers cmds domain_name store
              access push_cfg
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
