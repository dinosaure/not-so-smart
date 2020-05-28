open Cmdliner

let () = Mirage_crypto_rng_unix.initialize ()
let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let cmds = [ Fetch.fetch; Push.push ]

let main = `Help (`Pager, None)

let cmd =
  let doc = "Not so smart implementation of Git protocol." in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P "Not so smart implemention of $(i,git fetch) and $(i,git push).";
    ] in
  (Term.(ret (const main)), Term.info "nss" ~doc ~exits ~man)

let () = Term.(exit @@ eval_choice cmd cmds)
