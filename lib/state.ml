let ( <.> ) f g x = f (g x)

type ('a, 'err) t =
  | Read of { buffer : bytes; off : int; len : int; k : int -> ('a, 'err) t }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

module type CONTEXT = sig
  type t

  type encoder

  type decoder

  val pp : t Fmt.t

  val encoder : t -> encoder

  val decoder : t -> decoder

  val capabilities : t -> Capability.t list
end

module type S = sig
  type 'a send

  type 'a recv

  type error

  type encoder

  type decoder

  val encode :
    capabilities:Capability.t list ->
    encoder ->
    'a send ->
    'a ->
    (unit, error) t

  val decode :
    capabilities:Capability.t list -> decoder -> 'a recv -> ('a, error) t
end

module Context = struct
  type t = {
    encoder : Encoder.encoder;
    decoder : Decoder.decoder;
    mutable capabilities : Capability.t list;
  }

  type encoder = Encoder.encoder

  type decoder = Decoder.decoder

  let pp _ppf _t = ()

  let make capabilities =
    { encoder = Encoder.encoder (); decoder = Decoder.decoder (); capabilities }

  let encoder { encoder; _ } = encoder

  let decoder { decoder; _ } = decoder

  let capabilities { capabilities; _ } = capabilities

  let update ({ capabilities = client_side; _ } as t) server_side =
    let module Set = Set.Make (Capability) in
    let server_side = Set.of_list server_side in
    let client_side = Set.of_list client_side in
    t.capabilities <- Set.elements (Set.inter server_side client_side)
end

module Scheduler
    (Context : CONTEXT)
    (Value : S
               with type encoder = Context.encoder
                and type decoder = Context.decoder) =
struct
  type error = Value.error

  let rec go ~f m len =
    match m len with
    | Return v -> f v
    | Read { k; off; len; buffer } -> Read { k = go ~f k; off; len; buffer }
    | Write { k; off; len; buffer } -> Write { k = go ~f k; off; len; buffer }
    | Error err -> Error err

  let bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t =
   fun m ~f ->
    match m with
    | Return v -> f v
    | Error err -> Error err
    | Read { k; off; len; buffer } -> Read { k = go ~f k; off; len; buffer }
    | Write { k; off; len; buffer } -> Write { k = go ~f k; off; len; buffer }

  let ( let* ) m f = bind m ~f

  let ( >>= ) m f = bind m ~f

  let encode :
      type a.
      Context.t ->
      a Value.send ->
      a ->
      (Context.t -> ('b, [> `Protocol of error ]) t) ->
      ('b, [> `Protocol of error ]) t =
   fun ctx w v k ->
    let rec go = function
      | Return () -> k ctx
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Error err -> Error (`Protocol err) in
    go
      (Value.encode ~capabilities:(Context.capabilities ctx)
         (Context.encoder ctx) w v)

  let send :
      type a.
      Context.t -> a Value.send -> a -> (unit, [> `Protocol of error ]) t =
   fun ctx w x -> encode ctx w x (fun _ctx -> Return ())

  let decode :
      type a.
      Context.t ->
      a Value.recv ->
      (Context.t -> a -> ('b, [> `Protocol of error ]) t) ->
      ('b, [> `Protocol of error ]) t =
   fun ctx w k ->
    let rec go : (a, 'err) t -> ('b, [> `Protocol of error ]) t = function
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> k ctx v
      | Error err -> Error (`Protocol err) in
    go
      (Value.decode ~capabilities:(Context.capabilities ctx)
         (Context.decoder ctx) w)

  let recv : type a. Context.t -> a Value.recv -> (a, [> `Protocol of error ]) t
      =
   fun ctx w -> decode ctx w (fun _ctx v -> Return v)

  let reword_error f x =
    let rec go = function
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> Return v
      | Error err -> Error (f err) in
    go x

  let return v = Return v

  let fail error = Error error

  let error_msgf fmt = Fmt.kstrf (fun err -> Error (`Msg err)) fmt
end
