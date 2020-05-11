type ('a, 's) io

type ('k, 'v, 's) store

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

type ('uid, 'ref, 'v, 'g, 's) access = {
  exists : 'uid -> ('uid, 'v, 'g) store -> ('v option, 's) io;
  parents : 'uid -> ('uid, 'v, 'g) store -> ('v list, 's) io;
  deref : 'ref -> ('uid, 'v, 'g) store -> ('uid option, 's) io;
  locals : ('uid, 'v, 'g) store -> ('ref list, 's) io;
}

module type SCHED = sig
  type +'a s

  type t

  external inj : 'a s -> ('a, t) io = "%identity"

  external prj : ('a, t) io -> 'a s = "%identity"
end

module type STORE = sig
  type ('a, 'b) s

  type t

  external inj : ('a, 'b) s -> ('a, 'b, t) store = "%identity"

  external prj : ('a, 'b, t) store -> ('a, 'b) s = "%identity"
end

module Make_sched (T : sig
  type +'a t
end) : SCHED with type +'a s = 'a T.t

module Make_store (T : sig
  type ('k, 'v) t
end) : STORE with type ('k, 'v) s = ('k, 'v) T.t
