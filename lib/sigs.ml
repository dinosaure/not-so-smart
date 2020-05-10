module type FUNCTOR = sig
  type +'a t
end

type ('a, 's) io

type ('a, 's) store = ..

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

module type X = sig
  type +'a s

  type t

  external inj : 'a s -> ('a, t) io = "%identity"

  external prj : ('a, t) io -> 'a s = "%identity"
end

module Common = struct
  type t

  external inj : 'a -> 'b = "%identity"

  external prj : 'a -> 'b = "%identity"
end

module Make (T : FUNCTOR) = struct
  type 'a s = 'a T.t

  include Common
end
