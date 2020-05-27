type t = Lwt_unix.file_descr

type uid = Fpath.t

type error = [ `Msg of string ]

type +'a fiber = 'a Lwt.t

open Lwt.Infix

let pp_error = Rresult.R.pp_msg

let create fpath =
  Lwt_unix.openfile (Fpath.to_string fpath) Unix.[ O_CREAT; O_RDWR ] 0o644
  >>= Lwt.return_ok

let map fd ~pos len =
  let res =
    Mmap.V1.map_file
      (Lwt_unix.unix_file_descr fd)
      ~pos Bigarray.char Bigarray.c_layout false [| len |] in
  Bigarray.array1_of_genarray res

let map fd ~pos len =
  let res = ref None in
  (Lwt_unix.auto_yield 0.0 () >>= fun () ->
   res := Some (map fd ~pos len) ; Lwt.return_unit ) >>= fun () ->
  Lwt.return (Option.get !res)

let append fd str =
  let rec go off len =
    Lwt_unix.write_string fd str off len >>= fun len' ->
    if len - len' <= 0 then Lwt.return_unit else go (off + len') (len - len')
  in
  go 0 (String.length str)

let move ~src ~dst =
  Lwt_unix.rename (Fpath.to_string src) (Fpath.to_string dst) >>= fun () ->
  Lwt.return_ok ()

let close fd = Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
