open Neg.Sigs

type lwt

type error =
  [ Conduit_lwt.error
  | `Protocol of Smart.error ]

val lwt_prj : ('a, lwt) io -> 'a Lwt.t

val lwt_inj : 'a Lwt.t -> ('a, lwt) io

val lwt : lwt scheduler

val lwt_io : (Conduit_lwt.flow, error, lwt) flow

val lwt_fail : exn -> ('a, lwt) io
