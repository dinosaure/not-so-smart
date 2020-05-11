open Neg.Sigs

type git

type hex = private string
type reference = private string

val store_prj : ('uid, 'v, git) store -> ('uid, 'v) Hashtbl.t
val store_inj : ('uid, 'v) Hashtbl.t -> ('uid, 'v, git) store
val to_hex : hex -> string
val of_hex : string -> hex

val parents :
  's scheduler ->
  Fpath.t ->
  hex ->
  (hex, (hex * int ref * int64), git) store ->
  ((hex * int ref * int64) list, 's) io

val deref :
  's scheduler ->
  Fpath.t ->
  reference ->
  (hex, (hex * int ref * int64), git) store ->
  (hex option, 's) io

val locals :
  's scheduler -> Fpath.t -> (hex, (hex * int ref * int64), git) store -> (reference list, 's) io

val exists :
  's scheduler ->
  Fpath.t ->
  hex ->
  (hex, (hex * int ref * int64), git) store ->
  ((hex * int ref * int64) option, 's) io

val access : 's scheduler -> Fpath.t -> (hex, reference, (hex * int ref * int64), git, 's) access
