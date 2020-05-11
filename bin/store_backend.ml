open Neg.Sigs
open Rresult

let src = Logs.Src.create "store"

module Log = (val Logs.src_log src : Logs.LOG)

module Store = Neg.Sigs.Make_store (struct
  type ('k, 'v) t = ('k, 'v) Hashtbl.t
end)

type git = Store.t
type hex = string
type reference = string

let store_prj = Store.prj

let store_inj = Store.inj

let to_hex x = x
let of_hex x = x

let failwithf fmt = Fmt.kstrf (fun err -> raise (Failure err)) fmt

let exists path uid =
  let open Bos in
  OS.Dir.set_current path >>= fun () ->
  OS.Cmd.run_out ~err:OS.Cmd.err_null
    Cmd.(v "git" % "show" % "-s" % "--pretty=%ct" % uid)
  |> OS.Cmd.out_string ~trim:true
  >>= function
  | ts, (_, `Exited 0) ->
      let ts = Int64.of_string ts in
      let p = ref 0 in
      R.ok (Some (uid, p, ts))
  | _ -> R.ok None

let parents :
    type s.
    s scheduler ->
    Fpath.t ->
    hex ->
    (hex, (hex * int ref * int64), git) store ->
    ((hex * int ref * int64) list, s) io =
 fun { Neg.Sigs.return; _ } path uid store ->
  let store = Store.prj store in
  let fiber =
    let open Bos in
    OS.Dir.set_current path >>= fun () ->
    OS.Cmd.run_out ~err:OS.Cmd.err_null
      Cmd.(v "git" % "show" % "-s" % "--pretty=%P" % uid)
    |> OS.Cmd.out_lines ~trim:true in
  match fiber with
  | Ok (uids, (_, `Exited 0)) -> (
      let map uid =
        match Hashtbl.find store uid with
        | obj -> obj
        | exception Not_found ->
        match exists path uid with
        | Ok (Some obj) ->
            Hashtbl.add store uid obj ;
            obj
        | Ok None -> assert false
        | Error err -> Stdlib.raise (Failure (Fmt.strf "%a" R.pp_msg err)) in
      try
        let objs = List.map map uids in
        return objs
      with Failure err -> failwithf "%s" err)
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info)) ;
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m -> m "Got an error [parents]: %a" R.pp_msg err) ;
      failwithf "%a" R.pp_msg err

let deref :
    type s.
    s scheduler ->
    Fpath.t ->
    reference ->
    (hex, (hex * int ref * int64), git) store ->
    (hex option, s) io =
 fun { Neg.Sigs.return; _ } path reference _ ->
  let fiber =
    let open Bos in
    OS.Dir.set_current path >>= fun () ->
    OS.Cmd.run_out ~err:OS.Cmd.err_null
      Cmd.(v "git" % "show-ref" % "--hash" % reference)
    |> OS.Cmd.out_string ~trim:true in
  match fiber with
  | Ok (uid, (_, `Exited 0)) -> return (Some uid)
  | Ok _ -> return None
  | Error err ->
      Log.err (fun m -> m "Got an error [deref]: %a" R.pp_msg err) ;
      failwithf "%a" R.pp_msg err

let locals :
    type s. s scheduler -> Fpath.t -> (hex, (hex * int ref * int64), git) store -> (reference list, s) io
    =
 fun { Neg.Sigs.return; _ } path _ ->
  let fiber =
    let open Bos in
    OS.Dir.set_current path >>= fun () ->
    OS.Cmd.run_out ~err:OS.Cmd.err_null Cmd.(v "git" % "show-ref")
    |> OS.Cmd.out_lines ~trim:true in
  match fiber with
  | Ok (refs, (_, `Exited 0)) ->
      let map line =
        match Astring.String.cut ~sep:" " line with
        | Some (_, reference) -> reference
        | None -> line in
      return (List.map map refs)
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info)) ;
      failwithf "Impossible to get local references"
  | Error err ->
      Log.err (fun m -> m "Got an error [local]: %a" R.pp_msg err) ;
      failwithf "%a" R.pp_msg err

let exists { Neg.Sigs.return; _ } path uid store =
  let store = Store.prj store in
  match Hashtbl.find store uid with
  | v -> return (Some v)
  | exception Not_found ->
  match exists path uid with
  | Ok (Some obj) ->
      Hashtbl.replace store uid obj ;
      return (Some obj)
  | Ok None -> return None
  | Error err ->
      Log.warn (fun m -> m "Got an error [exists]: %a" R.pp_msg err) ;
      return None

let access : type s. s scheduler -> Fpath.t -> (hex, reference, (hex * int ref * int64), git, s) access =
 fun scheduler path ->
  let exists uid store = exists scheduler path uid store in
  {
    exists;
    parents = parents scheduler path;
    deref = deref scheduler path;
    locals = locals scheduler path;
  }
