open Rresult
open Lwt_backend
open Store_backend

let ( <.> ) f g x = f (g x)

let src = Logs.Src.create "not-so-smart.fetch"

module Log = (val Logs.src_log src : Logs.LOG)

let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

module G = Git.Make (Scheduler) (Append) (HTTP) (Uid) (Ref)

let ( >>? ) = Lwt_result.bind

let fpathf fmt = Fmt.kstrf Fpath.v fmt

let fetch edn ~resolvers ?(version = `V1) ?(capabilities = []) want path =
  let light_load uid = lightly_load lwt path uid |> Scheduler.prj in
  let heavy_load uid = heavily_load lwt path uid |> Scheduler.prj in
  let access = access lwt in
  let store = store_inj ({ path; tbl= Hashtbl.create 0x100; }) in
  Bos.OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
  Bos.OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
  Bos.OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
  G.fetch ~resolvers
    (access, light_load, heavy_load)
    store edn ~version ~capabilities want ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  >>? fun (uid, _refs) ->
  let pck = fpathf "pack-%a.pack" Uid.pp uid in
  let idx = fpathf "pack-%a.idx" Uid.pp uid in
  Bos.OS.Path.move tmp1 pck |> Lwt.return >>? fun () ->
  Bos.OS.Path.move tmp2 idx |> Lwt.return >>? fun () -> Lwt.return_ok ()

let resolvers =
  Conduit_lwt.add Conduit_lwt_unix_tcp.protocol
    (Conduit_lwt_unix_tcp.resolv_conf ~port:9418)
    Conduit.empty

module SSH = Awa_conduit.Make(Lwt)(Conduit_lwt)(Mclock)

let ssh_protocol = SSH.protocol_with_ssh Conduit_lwt_unix_tcp.protocol

let fetch all thin _depth no_done no_progress level style_renderer repository
    authenticator seed want path =
  let capabilities =
    let ( $ ) x f = f x in
    [ `Multi_ack_detailed; `Side_band_64k; `Ofs_delta ]
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
  let resolvers = match repository, seed with
    | { Git.scheme= `SSH user; path; _ }, Some seed ->
      let req = Awa.Ssh.Exec (Fmt.strf "git-upload-pack '%s'" path) in
      let ssh = { Awa_conduit.user; key= seed; req; authenticator; } in
      let resolve domain_name =
        let open Lwt.Infix in
        Conduit_lwt_unix_tcp.resolv_conf ~port:22 domain_name >>= function
        | Some edn -> Lwt.return (Some (edn, ssh))
        | None -> Lwt.return_none in
      R.ok (Conduit_lwt.add ~priority:0 ssh_protocol resolve resolvers)
    | { Git.scheme= `SSH _; _ }, None ->
      R.error_msgf "An ssh fetch requires a seed argument"
    | _ -> R.ok resolvers in
  resolvers >>= fun resolvers ->
  Lwt_main.run (fetch repository ~resolvers ~capabilities want path)

open Cmdliner

let edn =
  let parser = Git.endpoint_of_string in
  let pp = Git.pp_endpoint in
  Arg.conv (parser, pp)

let authenticator =
  let parser = R.reword_error R.msg <.> Awa.Keys.authenticator_of_string in
  let pp ppf _ = Fmt.pf ppf "#authenticator" in
  Arg.conv (parser, pp)

let seed =
  let parser = R.ok <.> Awa.Keys.of_seed in
  let pp ppf _ = Fmt.pf ppf "#ssh-key" in
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
  Arg.(required & pos 0 (some edn) None & info [] ~docv:"<repository>" ~doc)

let authenticator =
  let doc = "SSH authenticator." in
  Arg.(value & opt (some authenticator) None & info [ "a"; "authenticator" ] ~doc)

let seed =
  let doc = "SSH seed." in
  Arg.(value & opt (some seed) None & info [ "s"; "seed" ] ~doc)

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
      $ authenticator
      $ seed
      $ references
      $ local),
    Term.info "fetch" ~doc ~exits ~man )
