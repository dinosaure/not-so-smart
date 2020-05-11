let ( <.> ) f g x = f (g x)

module Advertised_refs = struct
  type ('hash, 'reference) t = {
    shallows : 'hash list;
    refs : ('hash * 'reference * bool) list;
    capabilities : Capability.t list;
    version : int;
  }

  let head { refs; _ } =
    try
      let uid, _, _ =
        List.find (function _, "HEAD", false -> true | _ -> false) refs in
      Some uid
    with _exn -> None

  let reference ~equal ?(peeled = false) refname { refs; _ } =
    try
      let uid, _, _ =
        List.find
          (fun (_, refname', peeled') ->
            equal refname refname' && peeled = peeled')
          refs in
      Some uid
    with _exn -> None

  let references ~equal ?(peeled = false) refnames { refs; _ } =
    let fold acc (uid, refname', peeled') =
      if List.exists (equal refname') refnames && peeled = peeled'
      then uid :: acc
      else acc in
    List.fold_left fold [] refs

  let refs { refs; _ } = refs
  let capabilities { capabilities; _ } = capabilities

  let pp ppf { shallows; refs; capabilities; version } =
    Fmt.pf ppf "version %d@ " version ;
    match refs with
    | [] ->
        Fmt.pf ppf "0 capabilities^{}@ " ;
        Fmt.pf ppf "%a@," Fmt.(Dump.list Capability.pp) capabilities ;
        List.iter (Fmt.pf ppf "shallow %s@ ") shallows
    | head :: refs ->
        let pp_ref ppf (uid, refname, peeled) =
          if peeled
          then Fmt.pf ppf "%s %s^{}" uid refname
          else Fmt.pf ppf "%s %s" uid refname in
        Fmt.pf ppf "%a@ " pp_ref head ;
        Fmt.pf ppf "%a@ " Fmt.(Dump.list Capability.pp) capabilities ;
        List.iter (Fmt.pf ppf "%a@ " pp_ref) refs ;
        List.iter (Fmt.pf ppf "shallow %s@ ") shallows
end

module Proto_request = struct
  type t = {
    path : string;
    host : [ `host ] Domain_name.t * int option;
    version : int;
    request_command : [ `Upload_pack | `Receive_pack | `Upload_archive ];
  }

  let upload_pack ~host ?port ?(version = 2) path =
    let host = (host, port) in
    { request_command = `Upload_pack; host; version; path }

  let pp ppf { path; host; request_command; version } =
    let pp_request_command ppf = function
      | `Upload_pack -> Fmt.pf ppf "git-upload-pack"
      | `Receive_pack -> Fmt.pf ppf "git-receive-pack"
      | `Upload_archive -> Fmt.pf ppf "git-upload-archive" in
    let pp_host ppf = function
      | host, Some port -> Fmt.pf ppf "%a:%d" Domain_name.pp host port
      | host, None -> Fmt.pf ppf "%a" Domain_name.pp host in
    Fmt.pf ppf "%a %s %a %a" pp_request_command request_command path
      Fmt.(prefix (const string " host=") pp_host)
      host
      Fmt.(prefix (const string " version=") int)
      version
end

module Want = struct
  type ('hash, 'reference) t = {
    wants : 'hash * 'hash list;
    shallows : 'hash list;
    deepen :
      [ `Depth of int | `Timestamp of int64 | `Not of 'reference ] option;
    filter : Filter.t option;
  }

  let want ?deepen ?filter ?(shallows = []) ?(others = []) hash =
    { wants = (hash, others); shallows; deepen; filter }
end

module Result = struct
  type 'hash t = NAK | ACK of 'hash

  let pp ppf = function
    | NAK -> Fmt.pf ppf "NAK"
    | ACK common -> Fmt.pf ppf "ACK %s" common
end

module Negotiation = struct
  type 'hash t =
    | ACK of 'hash
    | ACK_continue of 'hash
    | ACK_ready of 'hash
    | ACK_common of 'hash
    | NAK

  let is_common = function ACK_common _ -> true | _ -> false

  let is_ready = function ACK_ready _ -> true | _ -> false

  let is_nak = function NAK -> true | _ -> false

  let pp ppf = function
    | ACK uid -> Fmt.pf ppf "ACK %s" uid
    | ACK_continue uid -> Fmt.pf ppf "ACK %s continue" uid
    | ACK_ready uid -> Fmt.pf ppf "ACK %s ready" uid
    | ACK_common uid -> Fmt.pf ppf "ACK %s common" uid
    | NAK -> Fmt.pf ppf "NAK"

  let map ~f = function
    | ACK uid -> ACK (f uid)
    | ACK_continue uid -> ACK_continue (f uid)
    | ACK_ready uid -> ACK_ready (f uid)
    | ACK_common uid -> ACK_common (f uid)
    | NAK -> NAK
end

module Shallow = struct
  type 'hash t = Shallow of 'hash | Unshallow of 'hash
end

module Decoder = struct
  open Astring
  open Decoder

  type error =
    [ Decoder.error
    | `Invalid_advertised_ref of string
    | `Invalid_shallow of string
    | `Invalid_negotiation_result of string
    | `Invalid_side_band of string
    | `Invalid_ack of string ]

  let pp_error ppf = function
    | #Decoder.error as err -> Decoder.pp_error ppf err
    | `Invalid_advertised_ref raw ->
        Fmt.pf ppf "Invalid advertised refererence (%S)" raw
    | `Invalid_shallow raw -> Fmt.pf ppf "Invalid shallow (%S)" raw
    | `Invalid_negotiation_result raw ->
        Fmt.pf ppf "Invalid negotiation result (%S)" raw
    | `Invalid_side_band raw -> Fmt.pf ppf "Invalid side-band (%S)" raw
    | `Invalid_ack raw -> Fmt.pf ppf "Invalid ack (%S)" raw

  let rec prompt_pkt k decoder =
    if at_least_one_pkt decoder
    then k decoder
    else prompt (prompt_pkt k) decoder

  let is_new_line = function '\n' -> true | _ -> false

  let peek_pkt decoder =
    let buf, off, len = peek_pkt decoder in
    let buf = Bytes.unsafe_to_string buf in
    let res = String.Sub.v buf ~start:off ~stop:(off + len) in
    let res = String.Sub.trim ~drop:is_new_line res in
    res

  let is_zero = function '\000' -> true | _ -> false

  let v_zero = String.Sub.of_string "\000"

  let v_space = String.Sub.of_string " "

  let v_peeled = String.Sub.of_string "^{}"

  let v_shallow = String.Sub.of_string "shallow"

  let v_unshallow = String.Sub.of_string "unshallow"

  let v_version = String.Sub.of_string "version"

  let v_nak = String.Sub.of_string "NAK"

  let v_ack = String.Sub.of_string "ACK"

  let decode_advertised_refs decoder =
    let decode_shallows advertised_refs decoder =
      let rec go shallows decoder =
        let v = peek_pkt decoder in
        if String.Sub.is_empty v
        then (
          junk_pkt decoder ;
          return { advertised_refs with Advertised_refs.shallows } decoder)
        else
          match String.Sub.cut ~sep:v_space v with
          | Some (_, uid) ->
              let uid = String.Sub.to_string uid in
              junk_pkt decoder ;
              let k decoder = go (uid :: shallows) decoder in
              prompt_pkt k decoder
          | None -> fail decoder (`Invalid_shallow (String.Sub.to_string v))
      in
      go [] decoder in
    (* obj-id refname *)
    let decode_others_refs ~version ~head ~capabilities decoder =
      let rec go refs decoder =
        let v = peek_pkt decoder in
        if String.Sub.is_empty v
        then (
          junk_pkt decoder ;
          return
            {
              Advertised_refs.capabilities;
              refs = List.rev refs;
              version;
              shallows = [];
            }
            decoder)
        else if String.Sub.is_prefix ~affix:v_shallow v
        then
          decode_shallows
            {
              Advertised_refs.capabilities;
              refs = List.rev refs;
              version;
              shallows = [];
            }
            decoder
        else
          match String.Sub.cut ~sep:v_space v with
          | Some (uid, reference) ->
              let uid = String.Sub.to_string uid in
              let reference, peeled =
                match String.Sub.cut ~rev:true ~sep:v_peeled reference with
                | Some (reference, _) -> (String.Sub.to_string reference, true)
                | None -> (String.Sub.to_string reference, false) in
              let k decoder = go ((uid, reference, peeled) :: refs) decoder in
              junk_pkt decoder ;
              prompt_pkt k decoder
          | None ->
              fail decoder (`Invalid_advertised_ref (String.Sub.to_string v))
      in
      go [ head ] decoder in

    (* zero-id capabilities^{}\000capabilities *)
    let decode_no_ref ~version v decoder =
      let _, rest = Option.get (String.Sub.cut ~sep:v_space v) in
      match String.Sub.cut ~sep:v_zero rest with
      | Some (_, capabilities) ->
          let capabilities = String.Sub.fields capabilities in
          let capabilities =
            List.map
              (Capability.of_string <.> String.Sub.to_string)
              capabilities in
          junk_pkt decoder ;
          decode_shallows
            { Advertised_refs.capabilities; refs = []; version; shallows = [] }
            decoder
      | None -> fail decoder (`Invalid_advertised_ref (String.Sub.to_string v))
    in

    (* obj-id HEAD\000capabilities *)
    let decode_first_ref ~version v decoder =
      let uid, rest = Option.get (String.Sub.cut ~sep:v_space v) in
      match String.Sub.cut ~sep:v_zero rest with
      | Some (head, capabilities) ->
          let uid = String.Sub.to_string uid in
          let capabilities = String.Sub.fields capabilities in
          let capabilities =
            List.map
              (Capability.of_string <.> String.Sub.to_string)
              capabilities in
          let head = String.Sub.to_string head in
          junk_pkt decoder ;
          let k decoder =
            decode_others_refs ~version ~head:(uid, head, false) ~capabilities
              decoder in
          prompt_pkt k decoder
      | None -> fail decoder (`Invalid_advertised_ref (String.Sub.to_string v))
    in

    (* zero-id capabilities^{}\000capabilities
       | obj-id HEAD\000capabilities *)
    let decode_refs ?(version = 1) decoder =
      let v = peek_pkt decoder in
      match String.Sub.cut ~sep:v_space v with
      | Some (uid, _) ->
          if String.Sub.for_all is_zero uid
          then decode_no_ref ~version v decoder
          else decode_first_ref ~version v decoder
      | None -> fail decoder (`Invalid_advertised_ref (String.Sub.to_string v))
    in

    (* version (1|2) *)
    let decode_version decoder =
      let v = peek_pkt decoder in
      if String.Sub.is_prefix ~affix:v_version v
      then
        match String.Sub.cut ~sep:v_space v with
        | Some (_, version) ->
            let version = int_of_string (String.Sub.to_string version) in
            junk_pkt decoder ;
            prompt_pkt (decode_refs ~version) decoder
        | None -> prompt_pkt (decode_refs ~version:1) decoder
      else decode_refs decoder in
    prompt_pkt decode_version decoder

  let decode_result decoder =
    let k decoder =
      let v = peek_pkt decoder in
      if String.Sub.equal_bytes v v_nak
      then (
        junk_pkt decoder ;
        return Result.NAK decoder)
      else
        match String.Sub.cut ~sep:v_space v with
        | Some (_, common) ->
            let common = String.Sub.to_string common in
            junk_pkt decoder ;
            return (Result.ACK common) decoder
        | None ->
            fail decoder (`Invalid_negotiation_result (String.Sub.to_string v))
    in
    prompt_pkt k decoder

  let decode_pack ~capabilities ~push_pack ~push_stdout ~push_stderr decoder =
    let side_band =
      List.exists (( = ) `Side_band) capabilities
      || List.exists (( = ) `Side_band_64k) capabilities in
    let rec with_side_band decoder =
      let v = peek_pkt decoder in
      match String.Sub.head v with
      | Some '\001' ->
          let tail = String.Sub.to_string (String.Sub.tail v) (* copy *) in
          push_pack tail ;
          junk_pkt decoder ;
          prompt_pkt with_side_band decoder
      | Some '\002' ->
          let tail = String.Sub.to_string (String.Sub.tail v) (* copy *) in
          push_stdout tail ;
          junk_pkt decoder ;
          prompt_pkt with_side_band decoder
      | Some '\003' ->
          let tail = String.Sub.to_string (String.Sub.tail v) (* copy *) in
          push_stderr tail ;
          junk_pkt decoder ;
          prompt_pkt with_side_band decoder
      | Some _ -> fail decoder (`Invalid_side_band (String.Sub.to_string v))
      | None -> return () decoder in
    let rec without_side_band decoder =
      let v = peek_pkt decoder in
      if String.Sub.is_empty v
      then return () decoder
      else
        let v = String.Sub.to_string v in
        push_pack v ;
        junk_pkt decoder ;
        prompt_pkt without_side_band decoder in
    if side_band
    then prompt_pkt with_side_band decoder
    else prompt_pkt without_side_band decoder

  let decode_shallows decoder =
    let rec go acc decoder =
      let v = peek_pkt decoder in
      if String.Sub.length v = 0
      then (
        junk_pkt decoder ;
        return (List.rev acc) decoder)
      else if String.Sub.is_prefix ~affix:v_shallow v
              || String.Sub.is_prefix ~affix:v_unshallow v
      then
        match String.Sub.cut ~sep:v_space v with
        | Some (v, uid) ->
            let uid = String.Sub.to_string uid in
            if String.Sub.equal_bytes v v_shallow
            then (
              junk_pkt decoder ;
              prompt_pkt (go (Shallow.Shallow uid :: acc)) decoder)
            else (
              junk_pkt decoder ;
              prompt_pkt (go (Shallow.Unshallow uid :: acc)) decoder)
        | _ -> return (List.rev acc) decoder
      else return (List.rev acc) decoder in
    prompt_pkt (go []) decoder

  let decode_negotiation decoder =
    let k decoder =
      let pkt = peek_pkt decoder in
      if String.Sub.equal_bytes pkt v_nak
      then (
        junk_pkt decoder ;
        return Negotiation.NAK decoder)
      else if String.Sub.is_prefix ~affix:v_ack pkt
      then
        match String.Sub.cuts ~sep:v_space pkt with
        | [ _; uid ] ->
            let uid = String.Sub.to_string uid in
            junk_pkt decoder ;
            return (Negotiation.ACK uid) decoder
        | [ _; uid; v ] -> (
            let uid = String.Sub.to_string uid in
            match
              let v = String.Sub.to_string v in
              junk_pkt decoder ;
              v
            with
            | "continue" -> return (Negotiation.ACK_continue uid) decoder
            | "ready" -> return (Negotiation.ACK_ready uid) decoder
            | "common" -> return (Negotiation.ACK_common uid) decoder
            | _ -> fail decoder (`Invalid_ack (String.Sub.to_string pkt)))
        | _ -> fail decoder (`Invalid_ack (String.Sub.to_string pkt))
      else assert false in
    prompt_pkt k decoder
end

module Encoder = struct
  open Encoder

  type error = Encoder.error

  let pp_error = Encoder.pp_error

  let write_space encoder = write encoder " "

  let write_zero encoder = write encoder "\000"

  let write_new_line encoder = write encoder "\n"

  let delayed_write_pkt k0 k1 encoder =
    let pos = encoder.pos in
    encoder.pos <- encoder.pos + 4 ;
    k0 encoder ;
    (* XXX(dinosaure): or [encoder.pos <- encoder.pos + 4]? *)
    let len = encoder.pos - pos in
    Bytes.blit_string (Fmt.strf "%04X" len) 0 encoder.payload pos 4 ;
    flush k1 encoder

  let kdone _encoder = Done

  let kflush encoder =
    write encoder "0000" ;
    flush kdone encoder

  let encode_flush encoder = kflush encoder

  let encode_proto_request encoder
      { Proto_request.path; host; version; request_command } =
    let write_request_command encoder = function
      | `Upload_pack -> write encoder "git-upload-pack"
      | `Receive_pack -> write encoder "git-receive-pack"
      | `Upload_archive -> write encoder "git-upload-archive" in
    let write_version encoder version =
      let version = Fmt.strf "version=%d" version in
      write encoder version in
    let write_host encoder = function
      | host, Some port ->
          let host = Fmt.strf "host=%s:%d" (Domain_name.to_string host) port in
          write encoder host
      | host, None ->
          let host = Fmt.strf "host=%s" (Domain_name.to_string host) in
          write encoder host in
    let k encoder =
      write_request_command encoder request_command ;
      write_space encoder ;
      write encoder path ;
      write_zero encoder ;
      write_host encoder host ;
      write_zero encoder ;
      if version > 1
      then (
        write_zero encoder ;
        write_version encoder version ;
        write_zero encoder) in
    delayed_write_pkt k kdone encoder

  let encode_want ~capabilities encoder
      { Want.wants = first, others; shallows; deepen; filter } =
    let filter encoder =
      match filter with Some _ -> . | None -> kflush encoder in

    let deepen encoder =
      match deepen with
      | None -> filter encoder
      | Some (`Depth depth) ->
          let depth encoder =
            write encoder "deepen" ;
            write_space encoder ;
            write encoder (string_of_int depth) in
          delayed_write_pkt depth filter encoder
      | Some (`Timestamp timestamp) ->
          let timestamp encoder =
            write encoder "deepen-since" ;
            write_space encoder ;
            write encoder (Int64.to_string timestamp) in
          delayed_write_pkt timestamp filter encoder
      | Some (`Not reference) ->
          let not encoder =
            write encoder "deepen-not" ;
            write_space encoder ;
            write encoder reference in
          delayed_write_pkt not filter encoder in

    let shallows encoder =
      let shallow hash encoder =
        write encoder "shallow" ;
        write_space encoder ;
        write encoder hash in
      let rec go shallows encoder =
        match shallows with
        | [] -> deepen encoder
        | head :: tail -> delayed_write_pkt (shallow head) (go tail) encoder
      in
      go shallows encoder in

    let others encoder =
      let want hash encoder =
        write encoder "want" ;
        write_space encoder ;
        write encoder hash in
      let rec go others encoder =
        match others with
        | [] -> shallows encoder
        | head :: tail -> delayed_write_pkt (want head) (go tail) encoder in
      go others encoder in

    let first encoder =
      write encoder "want" ;
      write_space encoder ;
      write encoder first ;
      let rec go = function
        | [] -> ()
        | [ capability ] -> write encoder (Capability.to_string capability)
        | head :: tail ->
            write encoder (Capability.to_string head) ;
            write_space encoder ;
            go tail in
      if List.length capabilities > 0
      then (
        write_space encoder ;
        go capabilities) ;
      write_new_line encoder in

    delayed_write_pkt first others encoder

  let encode_done encoder =
    let k encoder =
      write encoder "done" ;
      write_new_line encoder in
    delayed_write_pkt k kdone encoder

  let unsafe_encode_packet encoder ~packet =
    let pos = encoder.pos in
    encoder.pos <- encoder.pos + 4 ;
    Encoder.write encoder packet ;
    let len = encoder.pos - pos in
    Bytes.blit_string (Fmt.strf "%04X" len) 0 encoder.payload pos 4
end
