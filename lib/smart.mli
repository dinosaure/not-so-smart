module Capability = Capability

module Advertised_refs : sig
  type ('uid, 'reference) t

  val pp : (string, string) t Fmt.t

  val head : ('a, string) t -> 'a option

  val capabilities : ('uid, 'reference) t -> Capability.t list

  val refs : ('uid, 'reference) t -> ('uid * 'reference * bool) list

  val reference :
    equal:('ref -> 'ref -> bool) ->
    ?peeled:bool ->
    'ref ->
    ('uid, 'ref) t ->
    'uid option

  val references :
    equal:('ref -> 'ref -> bool) ->
    ?peeled:bool ->
    'ref list ->
    ('uid, 'ref) t ->
    'uid list
end

module Proto_request : sig
  type t

  val pp : t Fmt.t

  val upload_pack :
    host:[ `host ] Domain_name.t -> ?port:int -> ?version:int -> string -> t

  val receive_pack :
    host:[ `host ] Domain_name.t -> ?port:int -> ?version:int -> string -> t
end

module Want : sig
  type ('uid, 'reference) t

  val want :
    capabilities:Capability.t list ->
    ?deepen:[ `Depth of int | `Timestamp of int64 | `Not of 'reference ] ->
    ?filter:Filter.t ->
    ?shallows:'uid list ->
    ?others:'uid list ->
    'uid ->
    ('uid, 'reference) t
end

module Result : sig
  type 'uid t = private NAK | ACK of 'uid

  val pp : string t Fmt.t
end

module Negotiation : sig
  type 'uid t = private
    | ACK          of 'uid
    | ACK_continue of 'uid
    | ACK_ready    of 'uid
    | ACK_common   of 'uid
    | NAK

  val is_common : 'uid t -> bool

  val is_ready : 'uid t -> bool

  val is_nak : 'uid t -> bool

  val pp : string t Fmt.t

  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module Commands : sig
  type ('uid, 'ref) command = private
    | Create of 'uid * 'ref
    | Delete of 'uid * 'ref
    | Update of 'uid * 'uid * 'ref

  type ('uid, 'ref) t

  val create : 'uid -> 'ref -> ('uid, 'ref) command

  val delete : 'uid -> 'ref -> ('uid, 'ref) command

  val update : 'uid -> 'uid -> 'ref -> ('uid, 'ref) command

  val v :
    capabilities:Capability.t list ->
    ?others:('uid, 'ref) command list ->
    ('uid, 'ref) command ->
    ('uid, 'ref) t

  val commands : ('uid, 'ref) t -> ('uid, 'ref) command list

  val map :
    fuid:('uid0 -> 'uid1) ->
    fref:('ref0 -> 'ref1) ->
    ('uid0, 'ref0) t ->
    ('uid1, 'ref1) t
end

module Status : sig
  type 'ref t = private {
    result : (unit, string) result;
    commands : ('ref, 'ref * string) result list;
  }

  val pp : string t Fmt.t
  val to_result : 'ref t -> (unit, string) result
  val v : ?err:string -> (('uid, 'ref) Commands.command, ('uid, 'ref) Commands.command * string) result list ->
    'ref t
end

module Shallow : sig
  type 'uid t = private Shallow of 'uid | Unshallow of 'uid
end

type ('a, 'err) t =
  | Read   of { buffer : bytes; off : int; len : int; k : int -> ('a, 'err) t }
  | Write  of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error  of 'err

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
  | `Invalid_result of string
  | `Invalid_command_result of string
  | `No_enough_space ]

val pp_error : error Fmt.t

type context

val make : Capability.t list -> context

val update : context -> Capability.t list -> unit

val shared : Capability.t -> context -> bool

val capabilities : context -> Capability.t list * Capability.t list

type 'a send

val proto_request : Proto_request.t send

val want : (string, string) Want.t send

val negotiation_done : unit send

val flush : unit send

val commands : (string, string) Commands.t send

val send_pack : ?stateless:bool -> bool -> string send

type 'a recv

val advertised_refs : (string, string) Advertised_refs.t recv

val negotiation_result : string Result.t recv

val recv_pack :
  ?side_band:bool ->
  ?push_stdout:(string -> unit) ->
  ?push_stderr:(string -> unit) ->
  push_pack:(string -> unit) ->
  unit recv

val ack : string Negotiation.t recv

val shallows : string Shallow.t list recv

val status : string Status.t recv

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

(**/**)

module Unsafe : sig
  val write : context -> string -> unit
end
