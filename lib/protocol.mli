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

module Decoder : sig
  open Decoder

  type error =
    [ Decoder.error
    | `Invalid_advertised_ref of string
    | `Invalid_shallow of string
    | `Invalid_negotiation_result of string
    | `Invalid_side_band of string
    | `Invalid_ack of string ]

  val pp_error : error Fmt.t

  val decode_advertised_refs :
    decoder -> ((string, string) Advertised_refs.t, [> error ]) state

  val decode_result : decoder -> (string Result.t, [> error ]) state

  val decode_pack :
    capabilities:Capability.t list ->
    push_pack:(string -> unit) ->
    push_stdout:(string -> unit) ->
    push_stderr:(string -> unit) ->
    decoder ->
    (unit, [> error ]) state

  val decode_negotiation : decoder -> (string Negotiation.t, [> error ]) state

  val decode_shallows : decoder -> (string Shallow.t list, [> error ]) state
end

module Encoder : sig
  open Encoder

  type error = Encoder.error

  val pp_error : error Fmt.t

  val encode_proto_request : encoder -> Proto_request.t -> error state

  val encode_want :
    capabilities:Capability.t list ->
    encoder ->
    (string, string) Want.t ->
    error state

  val encode_done : encoder -> error state

  val encode_flush : encoder -> error state

  val unsafe_encode_packet : encoder -> packet:string -> unit
end
