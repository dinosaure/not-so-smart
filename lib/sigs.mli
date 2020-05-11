type ('a, 's) io

type ('a, 's) store

type 's scheduler = {
  bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io;
  return : 'a. 'a -> ('a, 's) io;
}

type ('flow, 'error, 's) flow = {
  recv :
    'flow ->
    Cstruct.t ->
    (([ `End_of_input | `Input of int ], 'error) result, 's) io;
  send : 'flow -> Cstruct.t -> ((int, 'error) result, 's) io;
  pp_error : Format.formatter -> 'error -> unit;
}

type ('uid, 'ref, 'g, 's) access = {
  exists : 'uid -> ('uid, 'g) store -> (('uid * int ref * int64) option, 's) io;
  parents : 'uid -> ('uid, 'g) store -> (('uid * int ref * int64) list, 's) io;
  deref : 'ref -> ('uid, 'g) store -> ('uid option, 's) io;
  locals : ('uid, 'g) store -> ('ref list, 's) io;
}

module type SCHED = sig
  type +'a s

  type t

  external inj : 'a s -> ('a, t) io = "%identity"

  external prj : ('a, t) io -> 'a s = "%identity"
end

module type STORE = sig
  type 'a s

  type t

  external inj : 'a s -> ('a, t) store = "%identity"

  external prj : ('a, t) store -> 'a s = "%identity"
end

module Make_sched (T : sig
  type +'a t
end) : SCHED with type +'a s = 'a T.t

module Make_store (T : sig
  type 'a t
end) : STORE with type 'a s = 'a T.t
