open Neg.Sigs

type 'uid hashtbl = ('uid, 'uid * int ref * int64) Hashtbl.t

type git

val store_prj : ('uid, git) store -> 'uid hashtbl

val store_inj : 'uid hashtbl -> ('uid, git) store

val parents :
  's scheduler ->
  Fpath.t ->
  string ->
  (string, git) store ->
  ((string * int ref * int64) list, 's) io

val deref :
  's scheduler ->
  Fpath.t ->
  string ->
  (string, git) store ->
  (string option, 's) io

val locals :
  's scheduler -> Fpath.t -> (string, git) store -> (string list, 's) io

val exists :
  's scheduler ->
  Fpath.t ->
  string ->
  (string, git) store ->
  ((string * int ref * int64) option, 's) io

val access : 's scheduler -> Fpath.t -> (string, string, git, 's) access
