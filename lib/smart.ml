let ( <.> ) f g x = f (g x)

module Capability = Capability
module Proto_request = Protocol.Proto_request
module Advertised_refs = Protocol.Advertised_refs
module Want = Protocol.Want
module Result = Protocol.Result
module Negotiation = Protocol.Negotiation
module Shallow = Protocol.Shallow

module Witness = struct
  type 'a send =
    | Proto_request : Proto_request.t send
    | Want : (string, string) Want.t send
    | Done : unit send
    | Flush : unit send

  type 'a recv =
    | Advertised_refs : (string, string) Advertised_refs.t recv
    | Result : string Result.t recv
    | Pack : {
        push_pack : string -> unit;
        push_stdout : string -> unit;
        push_stderr : string -> unit;
      }
        -> unit recv
    | Ack : string Negotiation.t recv
    | Shallows : string Shallow.t list recv
end

module Value = struct
  type encoder = Encoder.encoder

  type decoder = Decoder.decoder

  include Witness

  type error = [ Protocol.Encoder.error | Protocol.Decoder.error ]

  let encode :
      type a.
      capabilities:Capability.t list ->
      encoder ->
      a send ->
      a ->
      (unit, [> Encoder.error ]) State.t =
   fun ~capabilities encoder w v ->
    let fiber : a send -> [> Encoder.error ] Encoder.state = function
      | Proto_request -> Protocol.Encoder.encode_proto_request encoder v
      | Want -> Protocol.Encoder.encode_want ~capabilities encoder v
      | Done -> Protocol.Encoder.encode_done encoder
      | Flush -> Protocol.Encoder.encode_flush encoder in
    let rec go = function
      | Encoder.Done -> State.Return ()
      | Encoder.Write { continue; buffer; off; len } ->
          State.Write { k = go <.> continue; buffer; off; len }
      | Encoder.Error err -> State.Error (err :> error) in
    (go <.> fiber) w

  let decode :
      type a.
      capabilities:Capability.t list ->
      decoder ->
      a recv ->
      (a, [> Decoder.error ]) State.t =
   fun ~capabilities decoder w ->
    let rec go = function
      | Decoder.Done v -> State.Return v
      | Decoder.Read { buffer; off; len; continue } ->
          State.Read { k = go <.> continue; buffer; off; len }
      | Decoder.Error { error; _ } -> State.Error error in
    match w with
    | Advertised_refs -> go (Protocol.Decoder.decode_advertised_refs decoder)
    | Result -> go (Protocol.Decoder.decode_result decoder)
    | Pack { push_pack; push_stdout; push_stderr } ->
        go
          (Protocol.Decoder.decode_pack ~capabilities ~push_pack ~push_stdout
             ~push_stderr decoder)
    | Ack -> go (Protocol.Decoder.decode_negotiation decoder)
    | Shallows -> go (Protocol.Decoder.decode_shallows decoder)
end

type ('a, 'err) t = ('a, 'err) State.t =
  | Read of { buffer : bytes; off : int; len : int; k : int -> ('a, 'err) t }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

type context = State.Context.t

let make capabilities = State.Context.make capabilities

let update ctx capabilities = State.Context.update ctx capabilities

include Witness

let proto_request = Proto_request

let advertised_refs = Advertised_refs

let want = Want

let negotiation_done = Done

let negotiation_result = Result

let pack ?(push_stdout = ignore) ?(push_stderr = ignore) ~push_pack =
  Pack { push_pack; push_stdout; push_stderr }

let flush = Flush

let ack = Ack

let shallows = Shallows

include State.Scheduler (State.Context) (Value)

let pp_error ppf = function
  | #Protocol.Encoder.error as err -> Protocol.Encoder.pp_error ppf err
  | #Protocol.Decoder.error as err -> Protocol.Decoder.pp_error ppf err

module Unsafe = struct
  let write context packet =
    let encoder = State.Context.encoder context in
    Protocol.Encoder.unsafe_encode_packet encoder ~packet
end