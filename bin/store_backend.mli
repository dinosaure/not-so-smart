open Neg.Sigs

type git

type reference = private string

val store_prj : ('uid, 'v, git) store -> ('uid, 'v) Hashtbl.t

val store_inj : ('uid, 'v) Hashtbl.t -> ('uid, 'v, git) store

val parents :
  's scheduler ->
  Fpath.t ->
  Uid.t ->
  (Uid.t, Uid.t * int ref * int64, git) store ->
  ((Uid.t * int ref * int64) list, 's) io

val deref :
  's scheduler ->
  Fpath.t ->
  reference ->
  (Uid.t, Uid.t * int ref * int64, git) store ->
  (Uid.t option, 's) io

val locals :
  's scheduler ->
  Fpath.t ->
  (Uid.t, Uid.t * int ref * int64, git) store ->
  (reference list, 's) io

val get_object_for_packer :
  's scheduler ->
  Fpath.t ->
  Uid.t ->
  (Uid.t, Uid.t Pck.t, git) store ->
  (Uid.t Pck.t option, 's) io

val get_commit_for_negotiation :
  's scheduler ->
  Fpath.t ->
  Uid.t ->
  (Uid.t, Uid.t * int ref * int64, git) store ->
  ((Uid.t * int ref * int64) option, 's) io

val heavily_load :
  's Carton.scheduler -> Fpath.t -> Uid.t -> (Carton.Dec.v, 's) Carton.io

val lightly_load :
  's Carton.scheduler -> Fpath.t -> Uid.t -> (Carton.kind * int, 's) Carton.io

val access :
  's scheduler ->
  Fpath.t ->
  (Uid.t, reference, Uid.t * int ref * int64, git, 's) access
