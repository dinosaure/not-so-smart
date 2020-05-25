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

module Verbose = struct
  type 'a fiber = 'a Lwt.t

  let succ () = Lwt.return_unit

  let print () = Lwt.return_unit
end

let ( <.> ) f g x = f (g x)

module Make
    (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
    (Append : APPEND with type +'a fiber = 'a Lwt.t)
    (Uid : UID)
    (Ref : Sigs.REF) =
struct
  module Thin = Carton_lwt.Thin.Make (Uid)

  let fs =
    let open Rresult in
    let open Lwt.Infix in
    {
      Thin.create =
        (fun path ->
          Append.create path >|= R.reword_error (R.msgf "%a" Append.pp_error));
      Thin.append = Append.append;
      Thin.map = Append.map;
      Thin.close =
        (fun fd ->
          Append.close fd >|= R.reword_error (R.msgf "%a" Append.pp_error));
    }

  (* XXX(dinosaure): abstract it? *)
  let digest :
      kind:[ `A | `B | `C | `D ] ->
      ?off:int ->
      ?len:int ->
      Bigstringaf.t ->
      Uid.t =
   fun ~kind ?(off = 0) ?len buf ->
    let len =
      match len with Some len -> len | None -> Bigstringaf.length buf - off
    in
    let ctx = Uid.empty in
    let feed_string ctx str =
      let off = 0 and len = String.length str in
      Uid.feed ctx (Bigstringaf.of_string ~off ~len str) in

    let ctx =
      match kind with
      | `A -> feed_string ctx (Fmt.strf "commit %d\000" len)
      | `B -> feed_string ctx (Fmt.strf "tree %d\000" len)
      | `C -> feed_string ctx (Fmt.strf "blob %d\000" len)
      | `D -> feed_string ctx (Fmt.strf "tag %d\000" len) in
    let ctx = Uid.feed ctx ~off ~len buf in
    Uid.get ctx

  let ( >>? ) = Lwt_result.bind

  module CartonSched = Carton.Make (Lwt)

  let sched =
    let open Lwt.Infix in
    let open CartonSched in
    {
      Carton.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      Carton.return = (fun x -> inj (Lwt.return x));
    }

  let finish_it ~pack ~weight ~where offsets =
    let open Lwt.Infix in
    Append.create pack >>? fun fd ->
    let zl_buffer = De.bigstring_create De.io_buffer_size in
    let allocate bits = De.make_window ~bits in
    let pack =
      Carton.Dec.make fd ~allocate ~z:zl_buffer ~uid_ln:Uid.length
        ~uid_rw:Uid.of_raw_string (fun uid -> Hashtbl.find where uid) in
    let map fd ~pos len =
      let max = Int64.sub weight pos in
      let len = min max (Int64.of_int len) in
      let len = Int64.to_int len in
      CartonSched.inj (Append.map fd ~pos len) in
    let rec go entries = function
      | [] -> Lwt.return entries
      | (offset, crc) :: offsets ->
          Lwt.catch
            (fun () ->
              Carton.Dec.weight_of_offset sched ~map pack
                ~weight:Carton.Dec.null ~cursor:offset
              |> CartonSched.prj
              >>= fun weight ->
              let raw = Carton.Dec.make_raw ~weight in
              Carton.Dec.of_offset sched ~map pack raw ~cursor:offset
              |> CartonSched.prj
              >>= fun v ->
              let kind = Carton.Dec.kind v in
              let raw = Carton.Dec.raw v in
              let len = Carton.Dec.len v in
              let uid = digest ~kind ~off:0 ~len raw in
              go ({ Carton.Dec.Idx.offset; crc; uid } :: entries) offsets)
            (fun exn ->
              Printexc.print_backtrace stdout ;
              Lwt.fail exn) in
    go [] offsets >>= fun entries ->
    Append.close fd >>? fun () -> Lwt.return_ok entries

  let run_pck ~light_load ~heavy_load stream ~src ~dst =
    let open Rresult in
    let open Lwt.Infix in
    Lwt.catch
      (fun () -> Thin.verify ~digest src fs stream)
      (function
        | Failure err -> Lwt.return_error (R.msg err)
        | Invalid_argument err -> Lwt.return_error (R.msg err)
        | exn -> Lwt.return_error (`Exn exn))
    >>= function
    | Error _ as err -> Lwt.return err
    | Ok (_, [], [], entries, _weight, uid) ->
        Append.move ~src ~dst >|= R.reword_error (R.msgf "%a" Append.pp_error)
        >>? fun () -> Lwt.return_ok (uid, Array.of_list entries)
    | Ok (n, uids, unresolveds, entries, weight, _uid) ->
        Thin.canonicalize ~light_load ~heavy_load ~src ~dst fs n uids weight
        >>? fun (shift, weight, uid, entries') ->
        let where = Hashtbl.create 0x100 in
        let entries =
          let fold ({ Carton.Dec.Idx.offset; uid; _ } as entry) =
            let offset = Int64.add offset shift in
            Hashtbl.add where uid offset ;
            { entry with Carton.Dec.Idx.offset } in
          List.map fold entries in
        List.iter
          (fun { Carton.Dec.Idx.offset; uid; _ } ->
            Hashtbl.add where uid offset)
          entries' ;
        let unresolveds =
          let fold (offset, crc) = (Int64.add offset shift, crc) in
          List.map fold unresolveds in
        finish_it ~pack:dst ~weight ~where unresolveds
        >|= R.reword_error (R.msgf "%a" Append.pp_error)
        >>? fun entries'' ->
        let entries = List.rev_append entries' entries in
        let entries = List.rev_append entries'' entries in
        Lwt.return_ok (uid, Array.of_list entries)

  module Enc = Carton.Dec.Idx.N (Uid)

  let run_idx ~dst ~pack entries =
    let open Lwt.Infix in
    let encoder = Enc.encoder `Manual ~pack entries in
    let buf = Bigstringaf.create De.io_buffer_size in
    Enc.dst encoder buf 0 (Bigstringaf.length buf) ;
    Append.create dst >>? fun fd ->
    let rec go = function
      | `Partial ->
          let len = Bigstringaf.length buf - Enc.dst_rem encoder in
          Append.append fd (Bigstringaf.substring buf ~off:0 ~len) >>= fun () ->
          Enc.dst encoder buf 0 (Bigstringaf.length buf) ;
          go (Enc.encode encoder `Await)
      | `Ok -> Lwt.return_ok () in
    go (Enc.encode encoder `Await) >>? fun () -> Append.close fd

  let run ~light_load ~heavy_load stream ~src ~dst ~idx =
    let open Rresult in
    let open Lwt.Infix in
    run_pck ~light_load ~heavy_load stream ~src ~dst >>? fun (pack, entries) ->
    run_idx ~dst:idx ~pack entries
    >|= R.reword_error (R.msgf "%a" Append.pp_error)
    >>? fun () -> Lwt.return_ok pack

  module Flow = struct
    type +'a fiber = 'a Lwt.t

    type t = Conduit_lwt.flow

    include Conduit_lwt
  end

  module Fetch = Nss.Fetch.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)
  module Push = Nss.Push.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)

  let fetch_v1 ~capabilities path ~resolvers ?want domain_name store access
      fetch_cfg pack =
    let open Lwt.Infix in
    Conduit_lwt.flow resolvers domain_name >>? fun flow ->
    Fetch.fetch_v1 ~capabilities ?want ~host:domain_name path flow store access
      fetch_cfg (fun (payload, off, len) ->
        let v = String.sub payload off len in
        pack (Some (v, 0, len)))
    >>= fun refs ->
    pack None ;
    Conduit_lwt.close flow >>? fun () -> Lwt.return_ok refs

  let fetch ~resolvers (access, light_load, heavy_load) store uri
      ?(version = `V1) ?(capabilities = []) want ~src ~dst ~idx =
    let open Rresult in
    let open Lwt.Infix in
    match (version, Uri.scheme uri, Uri.host uri, Uri.path uri) with
    | `V1, Some "git", Some domain_name, path ->
        let fetch_cfg = Nss.Fetch.configuration capabilities in
        let domain_name = Domain_name.(host_exn (of_string_exn domain_name)) in
        let stream, pusher = Lwt_stream.create () in
        let stream () = Lwt_stream.get stream in
        let run () =
          Fmt.epr ">>> START TO RUN FIBER.\n%!" ;
          Lwt.both
            (fetch_v1 ~capabilities path ~resolvers ~want domain_name store
               access fetch_cfg pusher)
            (run ~light_load ~heavy_load stream ~src ~dst ~idx)
          >>= fun (refs, idx) ->
          match (refs, idx) with
          | Ok refs, Ok uid -> Lwt.return_ok (uid, refs)
          | (Error _ as err), _ -> Lwt.return err
          | Ok _refs, (Error _ as err) -> Lwt.return err in
        Lwt.catch run (function
          | Failure err -> Lwt.return_error (R.msg err)
          | exn -> Lwt.return_error (`Exn exn))
    | _ -> Lwt.return_error (R.msgf "Invalid uri: %a" Uri.pp uri)

  module Delta = Carton_lwt.Enc.Delta (Uid) (Verbose)

  let deltify ~light_load ~heavy_load ?(threads = 4) (uids : Uid.t list) =
    let open Lwt.Infix in
    let fold (uid : Uid.t) =
      light_load uid >|= fun (kind, length) ->
      Carton_lwt.Enc.make_entry ~kind ~length uid in
    Lwt_list.map_p fold uids >|= Array.of_list >>= fun entries ->
    Delta.delta
      ~threads:(List.init threads (fun _thread -> heavy_load))
      ~weight:10 ~uid_ln:Uid.length entries
    >>= fun targets -> Lwt.return (entries, targets)

  let header = Bigstringaf.create 12

  let pack ~(heavy_load : Uid.t Carton_lwt.Enc.load) stream targets =
    let open Lwt.Infix in
    let offsets = Hashtbl.create (Array.length targets) in
    let find uid =
      match Hashtbl.find offsets uid with
      | v -> Lwt.return_some v
      | exception Not_found -> Lwt.return_none in
    let uid =
      { Carton.Enc.uid_ln = Uid.length; Carton.Enc.uid_rw = Uid.to_raw_string }
    in
    let b =
      {
        Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.q = De.Queue.create 0x10000;
        Carton.Enc.w = De.make_window ~bits:15;
      } in
    let ctx = ref Uid.empty in
    let cursor = ref 0 in
    Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12 ;
    stream (Some (Bigstringaf.to_string header)) ;
    ctx := Uid.feed !ctx header ~off:0 ~len:12 ;
    cursor := !cursor + 12 ;
    let encode_targets targets =
      let encode_target idx =
        Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor ;
        Carton_lwt.Enc.encode_target ~b ~find ~load:heavy_load ~uid
          targets.(idx) ~cursor:!cursor
        >>= fun (len, encoder) ->
        let rec go encoder =
          match Carton.Enc.N.encode ~o:b.o encoder with
          | `Flush (encoder, len) ->
              let payload = Bigstringaf.substring b.o ~off:0 ~len in
              stream (Some payload) ;
              ctx := Uid.feed !ctx b.o ~off:0 ~len ;
              cursor := !cursor + len ;
              let encoder =
                Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
              go encoder
          | `End -> Lwt.return () in
        let payload = Bigstringaf.substring b.o ~off:0 ~len in
        stream (Some payload) ;
        ctx := Uid.feed !ctx b.o ~off:0 ~len ;
        cursor := !cursor + len ;
        let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
        go encoder in
      let rec go idx =
        if idx < Array.length targets
        then encode_target idx >>= fun () -> go (succ idx)
        else Lwt.return () in
      go 0 in
    encode_targets targets >>= fun () ->
    let uid = Uid.((to_raw_string <.> get) !ctx) in
    stream (Some uid) ;
    stream None ;
    Lwt.return_unit

  let pack ~light_load ~heavy_load uids =
    let open Lwt.Infix in
    let stream, pusher = Lwt_stream.create () in
    let fiber () =
      deltify ~light_load ~heavy_load uids >>= fun (_, targets) ->
      pack ~heavy_load pusher targets in
    let stream () = Lwt_stream.get stream in
    Lwt.async fiber ;
    stream

  let push ~resolvers ~capabilities path cmds domain_name store access push_cfg
      pack =
    let open Lwt.Infix in
    Conduit_lwt.flow resolvers domain_name >>? fun flow ->
    Push.push ~capabilities cmds ~host:domain_name path flow store access
      push_cfg pack
    >>= fun () -> Conduit_lwt.close flow

  let push ~resolvers (access, light_load, heavy_load) store uri
      ?(version = `V1) ?(capabilities = []) cmds =
    let open Rresult in
    match (version, Uri.scheme uri, Uri.host uri, Uri.path uri) with
    | `V1, Some "git", Some domain_name, path ->
        let push_cfg = Nss.Push.configuration () in
        let domain_name = Domain_name.(host_exn <.> of_string_exn) domain_name in
        let run () =
          push ~resolvers ~capabilities path cmds domain_name store access
            push_cfg
            (pack ~light_load ~heavy_load) in
        Lwt.catch run (function
          | Failure err -> Lwt.return_error (R.msgf "%s" err)
          | exn -> Lwt.return_error (`Exn exn))
    | _ -> Lwt.return_error (R.msgf "Invalid uri: %a" Uri.pp uri)
end
