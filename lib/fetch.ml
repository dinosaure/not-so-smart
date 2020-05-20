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

let multi_ack capabilities =
  match
    ( List.exists (( = ) `Multi_ack) capabilities,
      List.exists (( = ) `Multi_ack_detailed) capabilities )
  with
  | true, true | false, true -> `Detailed
  | true, false -> `Some
  | false, false -> `None

let no_done = List.exists (( = ) `No_done)

let configuration ?(stateless = false) capabilities =
  {
    Neg.stateless;
    Neg.no_done = no_done capabilities;
    Neg.multi_ack = multi_ack capabilities;
  }

module Make
    (Scheduler : SCHED)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : UID)
    (Ref : REF) =
struct
  open Scheduler

  let src = Logs.Src.create "fetch"

  module Log = (val Logs.src_log src : Logs.LOG)

  let ( >>= ) x f = IO.bind x f

  let return x = IO.return x

  let sched =
    {
      Sigs.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      Sigs.return = (fun x -> inj (return x));
    }

  let fail exn = inj (IO.fail exn)

  let io =
    {
      Sigs.recv = (fun flow raw -> inj (Flow.recv flow raw));
      Sigs.send = (fun flow raw -> inj (Flow.send flow raw));
      Sigs.pp_error = Flow.pp_error;
    }

  let references want have =
    match want with
    | `None -> []
    | `All ->
        List.fold_left
          (fun acc -> function uid, _, false -> uid :: acc | _ -> acc)
          [] have
    | `Some refs ->
        let fold acc (uid, refname, peeled) =
          if List.exists Ref.(equal refname) refs && not peeled
          then uid :: acc
          else acc in
        List.fold_left fold [] have

  let fetch_v1 ~capabilities ?want:(refs = `None) ~host path flow store access
      fetch_cfg pack =
    let prelude ctx =
      let open Smart in
      let* () =
        send ctx proto_request (Proto_request.upload_pack ~host ~version:1 path)
      in
      let* v = recv ctx advertised_refs in
      let v = Smart.Advertised_refs.map ~fuid:Uid.of_hex ~fref:Ref.v v in
      let uids = references refs (Smart.Advertised_refs.refs v) in
      update ctx (Smart.Advertised_refs.capabilities v) ;
      return uids in
    let pack ctx =
      let open Smart in
      let side_band =
        Smart.shared `Side_band ctx || Smart.shared `Side_band_64k ctx in
      recv ctx
        (recv_pack ~side_band ~push_stdout:ignore ~push_stderr:ignore
           ~push_pack:pack) in
    let ctx = Smart.make capabilities in
    let negotiator = Neg.negotiator ~compare:Uid.compare in
    Neg.tips sched access store negotiator |> prj >>= fun () ->
    Neg.run sched fail io flow (prelude ctx) |> prj >>= fun uids ->
    let hex = { Neg.to_hex = Uid.to_hex; Neg.of_hex = Uid.of_hex } in
    Neg.find_common sched io flow fetch_cfg hex access store negotiator ctx uids
    |> prj
    >>= fun res ->
    if res < 0 then Log.warn (fun m -> m "No common commits") ;
    let rec go () =
      Neg.run sched fail io flow (pack ctx) |> prj >>= fun continue ->
      if continue
      then IO.yield () >>= go
      else return () in
    Log.debug (fun m -> m "Start to download PACK file.") ;
    go ()
end
