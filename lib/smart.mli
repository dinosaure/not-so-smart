module Capability = Capability

module Advertised_refs : sig
  type ('hash, 'reference) t

  val pp : (string, string) t Fmt.t

  val head : ('a, string) t -> 'a option

  val capabilities : ('hash, 'reference) t -> Capability.t list

  val reference :
    equal:('ref -> 'ref -> bool) ->
    ?peeled:bool ->
    'ref ->
    ('hash, 'ref) t ->
    'hash option
end

module Proto_request : sig
  type t

  val pp : t Fmt.t

  val upload_pack :
    host:[ `host ] Domain_name.t -> ?port:int -> ?version:int -> string -> t
end

module Want : sig
  type ('hash, 'reference) t

  val want :
    ?deepen:[ `Depth of int | `Timestamp of int64 | `Not of 'reference ] ->
    ?filter:Filter.t ->
    ?shallows:'hash list ->
    ?others:'hash list ->
    'hash ->
    ('hash, 'reference) t
end

module Result : sig
  type 'hash t = private NAK | ACK of 'hash

  val pp : string t Fmt.t
end

module Negotiation : sig
  type 'hash t = private
    | ACK of 'hash
    | ACK_continue of 'hash
    | ACK_ready of 'hash
    | ACK_common of 'hash
    | NAK

  val is_common : 'hash t -> bool

  val is_ready : 'hash t -> bool

  val is_nak : 'hash t -> bool

  val pp : string t Fmt.t
end

module Shallow : sig
  type 'hash t = private Shallow of 'hash | Unshallow of 'hash
end

type ('a, 'err) t =
  | Read of { buffer : bytes; off : int; len : int; k : int -> ('a, 'err) t }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

type error =
  [ `End_of_input
  | `Expected_char of char
  | `Unexpected_char of char
  | `Expected_string of string
  | `Expected_eol
  | `Expected_eol_or_space
  | `Unexpected_end_of_input
  | `No_enough_space
  | `Assert_predicate of char -> bool
  | `Invalid_advertised_ref of string
  | `Invalid_shallow of string
  | `Invalid_negotiation_result of string
  | `Invalid_side_band of string
  | `Invalid_ack of string
  | `No_enough_space ]

val pp_error : error Fmt.t

type context

val make : Capability.t list -> context

val update : context -> Capability.t list -> unit

type 'a send

val proto_request : Proto_request.t send

val want : (string, string) Want.t send

val negotiation_done : unit send

val flush : unit send

type 'a recv

val advertised_refs : (string, string) Advertised_refs.t recv

val negotiation_result : string Result.t recv

val pack :
  ?push_stdout:(string -> unit) ->
  ?push_stderr:(string -> unit) ->
  push_pack:(string -> unit) ->
  unit recv

val ack : string Negotiation.t recv

val shallows : string Shallow.t list recv

val bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t

val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

val encode :
  context ->
  'a send ->
  'a ->
  (context -> ('b, ([> `Protocol of error ] as 'err)) t) ->
  ('b, 'err) t

val decode :
  context ->
  'a recv ->
  (context -> 'a -> ('b, ([> `Protocol of error ] as 'err)) t) ->
  ('b, 'err) t

val send : context -> 'a send -> 'a -> (unit, [> `Protocol of error ]) t

val recv : context -> 'a recv -> ('a, [> `Protocol of error ]) t

val return : 'v -> ('v, 'err) t

val fail : 'err -> ('v, 'err) t

val reword_error : ('err0 -> 'err1) -> ('v, 'err0) t -> ('v, 'err1) t

val error_msgf :
  ('a, Format.formatter, unit, ('b, [> `Msg of string ]) t) format4 -> 'a

(**/*)

module Unsafe : sig
  val write : context -> string -> unit
end