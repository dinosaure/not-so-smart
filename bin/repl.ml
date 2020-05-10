module Lwt_scheduler = Neg.Sigs.Make (struct
  type +'a t = 'a Lwt.t
end)

let lwt =
  let open Lwt_scheduler in
  let open Lwt.Infix in
  {
    Neg.Sigs.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
    Neg.Sigs.return = (fun x -> inj (Lwt.return x));
  }

let pp_error ppf = function
  | `Protocol err -> Smart.pp_error ppf err
  | `Reference_not_found reference ->
      Fmt.pf ppf "Reference <%s> not found" reference
  | #Conduit_lwt.error as err -> Conduit_lwt.pp_error ppf err

let lwt_io =
  let open Lwt_scheduler in
  {
    Neg.Sigs.recv = (fun flow raw -> inj (Conduit_lwt.recv flow raw));
    Neg.Sigs.send = (fun flow raw -> inj (Conduit_lwt.send flow raw));
    Neg.Sigs.pp_error;
  }

let lwt_fail exn = Lwt_scheduler.inj (Lwt.fail exn)

type ('uid, 'ref, 'g, 's) git = {
  access : ('uid, 'ref, 'g, 's) Neg.Find_common.access;
  cfg : Neg.Find_common.configuration;
  capabilities : Smart.Capability.t list;
}

let clone_v1 ?(reference = "refs/heads/master") ~host path flow
    { access; cfg; capabilities } store =
  let prelude ctx =
    let open Smart in
    let* () =
      send ctx proto_request (Proto_request.upload_pack ~host ~version:1 path)
    in
    let* v = recv ctx advertised_refs in
    match Smart.Advertised_refs.reference ~equal:String.equal reference v with
    | None -> fail (`Reference_not_found reference)
    | Some uid ->
        update ctx (Smart.Advertised_refs.capabilities v) ;
        return uid in
  let pack ctx =
    let open Smart in
    recv ctx
      (pack ~push_stdout:(Fmt.pr "%s\n%!") ~push_stderr:(Fmt.epr "%s\n%!")
         ~push_pack:ignore) in
  let ctx = Smart.make capabilities in
  let negotiator = Neg.Default.make ~compare:String.compare in
  let open Lwt.Infix in
  Neg.tips lwt access store negotiator |> Lwt_scheduler.prj >>= fun () ->
  Neg.run lwt lwt_fail lwt_io flow (prelude ctx) |> Lwt_scheduler.prj
  >>= fun uid ->
  Neg.find_common lwt lwt_io flow cfg access store negotiator ctx [ uid ]
  |> Lwt_scheduler.prj
  >>= fun _res ->
  Neg.run lwt lwt_fail lwt_io flow (pack ctx) |> Lwt_scheduler.prj

let pp_error ppf = function
  | `Protocol err -> Smart.pp_error ppf err
  | `Reference_not_found reference ->
      Fmt.pf ppf "Reference <%s> not found" reference

let notzen =
  let x = Array.make 256 `None in
  for i = 0 to 31 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xd7, 0xff))
  done ;
  for i = 48 to 57 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xdf, 0x77))
  done ;
  for i = 65 to 90 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0x5f))
  done ;
  for i = 97 to 122 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0xd7))
  done ;
  Hxd.O.colorscheme_of_array x

let () = Hxd.Fmt.set_style_renderer Fmt.stdout `Ansi

let hxd = Hxd.O.xxd notzen

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let do_clone_v1 path ~git ~resolvers store domain_name =
  let open Lwt.Infix in
  Conduit_lwt_unix.flow resolvers domain_name >>= function
  | Error err -> failwithf "%a" Conduit_lwt_unix.pp_error err
  | Ok flow -> (
      Fmt.pr ">>> start to clone.\n%!" ;
      clone_v1 ~host:domain_name path flow git store >>= fun () ->
      Conduit_lwt.close flow >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failwithf "%a" Conduit_lwt.pp_error err)

let resolvers =
  Conduit_lwt.register_resolver ~key:Conduit_lwt_unix_tcp.endpoint
    (Conduit_lwt_unix_tcp.resolv_conf ~port:9418)
    Conduit.empty

type 'uid hashtbl = ('uid, 'uid * int ref * int64) Hashtbl.t

type ('a, 's) Neg.Sigs.store +=
  | Hashtbl : 'a hashtbl -> ('a, 'a hashtbl) Neg.Sigs.store

let git ~capabilities path =
  let exists uid store =
    let store = match store with Hashtbl store -> store | _ -> assert false in
    match Hashtbl.find store uid with
    | v -> Lwt.return_some v
    | exception Not_found -> (
        let res =
          let open Rresult in
          let open Bos in
          OS.Dir.set_current path >>= fun () ->
          OS.Cmd.run_out Cmd.(v "git" % "show" % "-s" % "--pretty=%ct" % uid)
          |> OS.Cmd.out_string ~trim:true
          >>= function
          | timestamp, (_, `Exited 0) ->
              let ts = Int64.of_string timestamp in
              let p = ref 0 in
              Hashtbl.replace store uid (uid, p, ts) ;
              R.ok (Some (uid, p, ts))
          | _ -> R.ok None in
        match res with
        | Ok v -> Lwt.return v
        | Error _err ->
            Fmt.epr "[!] git cat-file -e %s\n%!" uid ;
            Lwt.return_none) in
  let parents uid store =
    let store = match store with Hashtbl store -> store | _ -> assert false in
    let res =
      let open Rresult in
      let open Bos in
      OS.Dir.set_current path >>= fun () ->
      OS.Cmd.run_out Cmd.(v "git" % "show" % "-s" % "--pretty=%P" % uid)
      |> OS.Cmd.out_lines ~trim:true
      >>| fst in
    match res with
    | Ok lst ->
        let f uid =
          try Hashtbl.find store uid
          with Not_found -> (
            let res =
              let open Rresult in
              let open Bos in
              OS.Dir.set_current path >>= fun () ->
              OS.Cmd.run_out
                Cmd.(v "git" % "show" % "-s" % "--pretty=%ct" % uid)
              |> OS.Cmd.out_string ~trim:true
              >>| fst
              >>= fun timestamp ->
              let ts = Int64.of_string timestamp in
              let p = ref 0 in
              Hashtbl.replace store uid (uid, p, ts) ;
              R.ok (uid, p, ts) in
            match res with
            | Ok obj ->
                Hashtbl.replace store uid obj ;
                obj
            | Error err -> Fmt.failwith "%a" Rresult.R.pp_msg err) in
        Lwt.return (List.map f lst)
    | Error _err ->
        Fmt.epr "[!] git show -s --pretty=%%P %s\n%!" uid ;
        Lwt.return [] in
  let deref reference _ =
    let res =
      let open Rresult in
      let open Bos in
      OS.Dir.set_current path >>= fun () ->
      OS.Cmd.run_out Cmd.(v "git" % "show-ref" % "--hash" % reference)
      |> OS.Cmd.out_string ~trim:true
      >>| fst in
    match res with
    | Ok uid -> Lwt.return_some uid
    | Error _err ->
        Fmt.epr "[!] git show-ref --hash %s\n%!" reference ;
        Lwt.return_none in
  let locals _ =
    let res =
      let open Rresult in
      let open Bos in
      OS.Dir.set_current path >>= fun () ->
      OS.Cmd.run_out Cmd.(v "git" % "show-ref")
      |> OS.Cmd.out_lines ~trim:true
      >>| fst in
    match res with
    | Ok refs ->
        let f line =
          match Astring.String.cut ~sep:" " line with
          | Some (_, reference) -> reference
          | None -> line in
        Lwt.return (List.map f refs)
    | Error _err -> Lwt.return [] in
  let access =
    let open Lwt_scheduler in
    {
      Neg.Find_common.exists = (fun uid store -> inj (exists uid store));
      Neg.Find_common.parents = (fun uid store -> inj (parents uid store));
      Neg.Find_common.deref =
        (fun reference store -> inj (deref reference store));
      Neg.Find_common.locals = (fun store -> inj (locals store));
    } in
  let cfg =
    {
      Neg.Find_common.stateless = false;
      Neg.Find_common.multi_ack =
        (match
           ( List.find_opt (( = ) `Multi_ack) capabilities,
             List.find_opt (( = ) `Multi_ack_detailed) capabilities )
         with
        | Some _, Some _ | None, Some _ -> `Detailed
        | Some _, None -> `Some
        | None, None -> `None);
      Neg.Find_common.no_done = false;
    } in
  { access; cfg; capabilities }

let main ~capabilities repository =
  let open Rresult in
  let open Bos in
  OS.Dir.set_current repository >>= fun () ->
  OS.Cmd.run_out Cmd.(v "git" % "remote" % "get-url" % "origin")
  |> OS.Cmd.out_string ~trim:true
  >>| fst
  >>| Uri.of_string
  >>= fun uri ->
  match (Uri.scheme uri, Uri.host uri, Uri.path uri) with
  | Some "git", Some domain_name, path ->
      let git = git ~capabilities repository in
      let store = Hashtbl (Hashtbl.create 0x100) in
      let domain_name = Domain_name.(host_exn (of_string_exn domain_name)) in
      let fiber =
        let open Lwt.Infix in
        Lwt.catch
          (fun () ->
            do_clone_v1 path ~git ~resolvers store domain_name >>= Lwt.return_ok)
          (function
            | Failure err -> Lwt.return_error (R.msgf "%s" err)
            | exn -> Lwt.return_error (R.msgf "%s" (Printexc.to_string exn)))
      in
      Lwt_main.run fiber
  | _ -> R.error_msgf "Invalid uri: %a" Uri.pp uri

let () =
  match Sys.argv with
  | [| _; path |] -> (
      let path = Fpath.v path in
      match
        main
          ~capabilities:
            [
              `Thin_pack;
              `Ofs_delta;
              `Multi_ack;
              `Multi_ack_detailed;
              `Side_band_64k;
            ]
          path
      with
      | Ok () -> ()
      | Error (`Msg err) -> Fmt.epr "%s" err)
  | _ -> Fmt.epr "%s <uri>\n%!" Sys.argv.(0)
