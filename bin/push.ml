open Rresult
open Lwt_backend
open Store_backend

let src = Logs.Src.create "push"

module Log = (val Logs.src_log src : Logs.LOG)

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let ( <.> ) f g = fun x -> f (g x)

module Append = struct
  type t = Lwt_unix.file_descr
  type uid = Fpath.t
  type error = [ `Msg of string ]

  type +'a fiber = 'a Lwt.t

  open Lwt.Infix

  let pp_error = Rresult.R.pp_msg

  let create fpath =
    Lwt_unix.openfile (Fpath.to_string fpath) Unix.[ O_CREAT; O_RDWR ] 0o644 >>= Lwt.return_ok

  let map fd ~pos len =
    let res = Mmap.V1.map_file (Lwt_unix.unix_file_descr fd)
        ~pos Bigarray.char Bigarray.c_layout false [| len |] in
    let res = Bigarray.array1_of_genarray res in
    Lwt.return res

  let append fd str =
    let rec go off len =
      Lwt_unix.write_string fd str off len >>= fun len' ->
      if len - len' <= 0 then Lwt.return_unit
      else go (off + len') (len - len') in
    go 0 (String.length str)

  let move ~src ~dst =
    Lwt_unix.rename (Fpath.to_string src) (Fpath.to_string dst) >>= fun () ->
    Lwt.return_ok ()

  let close fd =
    Lwt_unix.close fd >>= fun () ->
    Lwt.return_ok ()

  let delete fpath =
    Lwt_unix.unlink (Fpath.to_string fpath) >>= fun () ->
    Lwt.return_ok ()
end

module Git = Git.Make (Scheduler) (Append) (Uid) (Ref)

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
  let store = store_inj (Hashtbl.create 0x100) in
  Git.push ~resolvers (access, light_load, heavy_load) store uri ~version ~capabilities
    cmds

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
