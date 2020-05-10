open Sigs

let _initial_flush = 16

let _max_in_vain = 256

let _large_flush = 16384

let _pipe_safe_flush = 32

type ('uid, 'g, 's) exists =
  ('uid, 'g) store -> 'uid -> (('uid * int ref) option, 's) io

type ('a, 's) raise = exn -> ('a, 's) io

let run :
    type f s.
    s scheduler ->
    ('a, s) raise ->
    (f, ([> ] as 'error), s) flow ->
    f ->
    ('res, 'error) Smart.t ->
    ('res, s) io =
 fun { bind; return } raise { recv; send; pp_error } flow fiber ->
  let ( >>= ) = bind in
  let tmp = Cstruct.create 0x1000 in
  let failwithf fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt in
  let rec go = function
    | Smart.Read { k; buffer; off; len } -> (
        let max = min (Cstruct.len tmp) len in
        recv flow (Cstruct.sub tmp 0 max) >>= function
        | Ok `End_of_input -> failwithf "End of input"
        | Ok (`Input len) ->
            Cstruct.blit_to_bytes tmp 0 buffer off len ;
            go (k len)
        | Error err -> failwithf "%a" pp_error err)
    | Smart.Write { k; buffer; off; len } ->
        let rec loop tmp =
          if Cstruct.len tmp = 0
          then go (k len)
          else
            send flow tmp >>= function
            | Ok shift -> loop (Cstruct.shift tmp shift)
            | Error err -> failwithf "%a" pp_error err in
        loop (Cstruct.of_string buffer ~off ~len)
    | Smart.Return v -> return v
    | Smart.Error err -> failwithf "%a" pp_error err in
  go fiber

(* XXX(dinosaure): this part is really **ugly**! But we must follow the same
   behaviour of [git]. Instead to understand the synchronisation process of [git]
   with Smart.v1 and implement a state of the art synchronisation algorithm, I
   translated as is [fetch-pack.c:find_common] in OCaml. *)

let unsafe_write_have ctx uid =
  let packet = Fmt.strf "have %s\n" uid in
  Smart.Unsafe.write ctx packet

let unsafe_write_done ctx = Smart.Unsafe.write ctx "done\n"

let next_flush stateless count =
  if stateless
  then if count < _large_flush then count lsl 1 else count * 11 / 10
  else if count < _pipe_safe_flush
  then count lsl 1
  else count + _pipe_safe_flush

type configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
}

type ('uid, 'ref, 'g, 's) access = {
  exists : 'uid -> ('uid, 'g) store -> (('uid * int ref * int64) option, 's) io;
  parents : 'uid -> ('uid, 'g) store -> (('uid * int ref * int64) list, 's) io;
  deref : 'ref -> ('uid, 'g) store -> ('uid option, 's) io;
  locals : ('uid, 'g) store -> ('ref list, 's) io;
}

let tips { bind; return } { exists; deref; locals; _ } store negotiator =
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in

  let rec go = function
    | [] -> return ()
    | reference :: others ->
        deref reference store
        >>= Option.fold ~none:(return None) ~some:(fun uid -> exists uid store)
        >>| Option.iter (fun obj -> Default.tip negotiator obj)
        >>= fun () -> go others in
  locals store >>= go

let find_common ({ bind; return } as scheduler) io flow
    ({ stateless; no_done; _ } as cfg) access store negotiator ctx refs =
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in
  let fold_left_s ~f a l =
    let rec go a = function
      | [] -> return a
      | x :: r -> f a x >>= fun a -> go a r in
    go a l in
  let fold acc remote_uid =
    access.exists remote_uid store >>= function
    | Some _ -> return acc
    | None -> return ((remote_uid, ref 0) :: acc) in
  fold_left_s ~f:fold [] refs >>| List.rev >>= function
  | [] ->
      run scheduler raise io flow Smart.(send ctx flush ()) >>= fun () ->
      assert false
  | uid :: others ->
      run scheduler raise io flow
        Smart.(
          let uid, _ = uid in
          let others = List.map fst others in
          send ctx want (Want.want uid ~others))
      >>= fun () ->
      let in_vain = ref 0 in
      let count = ref 0 in
      let flush_at = ref _initial_flush in
      let flushes = ref 0 in
      let got_continue = ref false in
      let got_ready = ref false in
      let retval = ref (-1) in
      (* TODO(dinosaure): handle [shallow] and [unshallow]. *)
      let rec go negotiator =
        Default.next scheduler ~parents:access.parents store negotiator
        >>= function
        | None -> return ()
        | Some uid ->
            unsafe_write_have ctx uid ;
            (* completely unsafe! *)
            incr in_vain ;
            incr count ;
            if !flush_at <= !count
            then (
              run scheduler raise io flow Smart.(send ctx flush ())
              >>= fun () ->
              incr flushes ;
              flush_at := next_flush stateless !count ;
              if (not stateless) && !count = _initial_flush
              then go negotiator
              else
                run scheduler raise io flow Smart.(recv ctx shallows)
                >>= fun _shallows ->
                let rec loop () =
                  run scheduler raise io flow Smart.(recv ctx ack)
                  >>= fun ack ->
                  match ack with
                  | Smart.Negotiation.NAK -> return `Continue
                  | Smart.Negotiation.ACK _ ->
                      flushes := 0 ;
                      cfg.multi_ack <- `None ;
                      (* XXX(dinosaure): [multi_ack] supported by the client but it
                         is not supported by the server. *)
                      retval := 0 ;
                      return `Done
                  | Smart.Negotiation.ACK_common uid
                  | Smart.Negotiation.ACK_ready uid
                  | Smart.Negotiation.ACK_continue uid -> (
                      access.exists uid store >>= function
                      | None -> assert false
                      | Some obj ->
                          Default.ack scheduler ~parents:access.parents store
                            negotiator obj
                          >>= fun was_common ->
                          if stateless
                             && Smart.Negotiation.is_common ack
                             && not was_common
                          then (
                            (* we need to replay the have for this object on the next RPC request so
                               the peer kows it is in common with us. *)
                            unsafe_write_have ctx uid ;
                            (* reset [in_vain] because an ack for this commit has not been seen. *)
                            in_vain := 0 ;
                            retval := 0 ;
                            got_continue := true ;
                            loop ())
                          else if (not stateless)
                                  || not (Smart.Negotiation.is_common ack)
                          then (
                            in_vain := 0 ;
                            retval := 0 ;
                            got_continue := true ;
                            if Smart.Negotiation.is_ready ack
                            then got_ready := true ;
                            loop ())
                          else (
                            retval := 0 ;
                            got_continue := true ;
                            if Smart.Negotiation.is_ready ack
                            then got_ready := true ;
                            loop ())) in
                loop () >>= function
                | `Done -> return ()
                | `Continue ->
                    decr flushes ;
                    if !got_continue && _max_in_vain < !in_vain
                    then return ()
                    else if !got_ready
                    then return ()
                    else go negotiator)
            else go negotiator in
      go negotiator >>= fun () ->
      (if (not !got_ready) || not no_done
      then (
        unsafe_write_done ctx ;
        run scheduler raise io flow Smart.(send ctx flush ()))
      else return ())
      >>= fun () ->
      if !retval <> 0
      then (
        cfg.multi_ack <- `None ;
        incr flushes) ;
      (if (not !got_ready) || not no_done
      then
        run scheduler raise io flow Smart.(recv ctx shallows)
        >>= fun _shallows -> return ()
      else return ())
      >>= fun () ->
      let rec go () =
        if !flushes > 0 || cfg.multi_ack = `Some || cfg.multi_ack = `Detailed
        then (
          run scheduler raise io flow Smart.(recv ctx ack) >>= fun ack ->
          match ack with
          | Smart.Negotiation.ACK _ -> return 0
          | Smart.Negotiation.ACK_common _ | Smart.Negotiation.ACK_continue _
          | Smart.Negotiation.ACK_ready _ ->
              cfg.multi_ack <- `Some ;
              go ()
          | Smart.Negotiation.NAK ->
              decr flushes ;
              go ())
        else if !count > 0
        then return !retval
        else return 0 in
      go ()