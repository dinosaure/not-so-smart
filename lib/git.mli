module type APPEND = sig
  type t
  type uid
  type error

  type +'a fiber

  val pp_error : error Fmt.t
  val create : uid -> (t, error) result fiber
  val map : t -> pos:int64 -> int -> Bigstringaf.t fiber
  val append : t -> string -> unit fiber
  val move : src:uid -> dst:uid -> (unit, error) result fiber
  val close : t -> (unit, error) result fiber
end

module type UID = sig
  include Carton.UID
  include Sigs.UID with type t := t
  val hash : t -> int
end

module Make
    (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
    (Append : APPEND with type +'a fiber = 'a Lwt.t)
    (Uid : UID)
    (Ref : Sigs.REF) : sig
  val fetch
    :  resolvers:Conduit.resolvers
    -> (Uid.t, _, Uid.t * int ref * int64, 'g, Scheduler.t) Sigs.access
       * Uid.t Carton_lwt.Thin.light_load
       * Uid.t Carton_lwt.Thin.heavy_load
    -> (Uid.t, Uid.t * int ref * int64, 'g) Sigs.store
    -> Uri.t
    -> ?version:[> `V1 ]
    -> ?capabilities:Smart.Capability.t list
    -> [ `All | `Some of Ref.t list | `None ]
    -> src:Append.uid
    -> dst:Append.uid
    -> idx:Append.uid
    -> (Uid.t * Ref.t list, [> `Msg of string
                            | `Exn of exn
                            | `Invalid_key
                            | `Not_found
                            | `Unresolved ]) result Lwt.t

  val push
    :  resolvers:Conduit.resolvers
    -> (Uid.t, Ref.t, Uid.t Pck.t, 'g, Scheduler.t) Sigs.access
       * Uid.t Carton_lwt.Thin.light_load
       * Uid.t Carton_lwt.Thin.heavy_load
    -> (Uid.t, Uid.t Pck.t, 'g) Sigs.store
    -> Uri.t
    -> ?version:[> `V1 ]
    -> ?capabilities:Smart.Capability.t list
    -> [ `Create of Ref.t | `Delete of Ref.t | `Update of Ref.t * Ref.t ] list
    -> (unit, [> `Msg of string
              | `Exn of exn
              | `Invalid_key
              | `Not_found
              | `Unresolved ]) result Lwt.t
end
