open Sigs

type git

val store_prj : ('uid, 'v, git) store -> ('uid, 'v) Hashtbl.t

val store_inj : ('uid, 'v) Hashtbl.t -> ('uid, 'v, git) store

val parents :
  's scheduler ->
  Fpath.t ->
  Uid.t ->
  (Uid.t, Uid.t * int ref * int64, git) store ->
  ((Uid.t * int ref * int64) list, 's) io

val deref : 's scheduler -> Fpath.t -> 'store -> Ref.t -> (Uid.t option, 's) io

val locals :
  's scheduler ->
  Fpath.t ->
  (Uid.t, Uid.t * int ref * int64, git) store ->
  (Ref.t list, 's) io

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

val heavily_load : 's scheduler -> Fpath.t -> Uid.t -> (Carton.Dec.v, 's) io

val lightly_load :
  's scheduler -> Fpath.t -> Uid.t -> (Carton.kind * int, 's) io

val access :
  's scheduler ->
  Fpath.t ->
  (Uid.t, Ref.t, Uid.t * int ref * int64, git, 's) access
