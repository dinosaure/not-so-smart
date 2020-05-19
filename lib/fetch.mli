open Sigs

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val fail : exn -> 'a t

  val yield : unit -> unit t
end

module type UID = sig
  type t

  val of_hex : string -> t

  val to_hex : t -> string

  val compare : t -> t -> int
end

module type REF = sig
  type t

  val v : string -> t

  val equal : t -> t -> bool
end

module type FLOW = sig
  type +'a fiber

  type t

  type error

  val recv :
    t -> Cstruct.t -> ([ `End_of_input | `Input of int ], error) result fiber

  val send : t -> Cstruct.t -> (int, error) result fiber

  val pp_error : error Fmt.t
end

type configuration = Neg.configuration

val configuration : ?stateless:bool -> Smart.Capability.t list -> configuration

module Make
    (Scheduler : SCHED)
    (IO : IO with type +'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : UID)
    (Ref : REF) : sig
  val fetch_v1 :
    capabilities:Smart.Capability.t list ->
    ?want:[ `All | `Some of Ref.t list | `None ] ->
    host:[ `host ] Domain_name.t ->
    string ->
    Flow.t ->
    (Uid.t, Uid.t * int ref * int64, 'g) store ->
    (Uid.t, _, Uid.t * int ref * int64, 'g, Scheduler.t) access ->
    configuration ->
    (string * int * int -> unit) ->
    unit IO.t
end
