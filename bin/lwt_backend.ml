module Lwt_scheduler = Neg.Sigs.Make_sched (struct
  type +'a t = 'a Lwt.t
end)

let lwt_prj = Lwt_scheduler.prj

let lwt_inj = Lwt_scheduler.inj

type lwt = Lwt_scheduler.t

type error = [ Conduit_lwt.error | `Protocol of Smart.error ]

let lwt =
  let open Lwt_scheduler in
  let open Lwt.Infix in
  {
    Neg.Sigs.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
    Neg.Sigs.return = (fun x -> inj (Lwt.return x));
  }

let pp_error ppf = function
  | `Protocol err -> Smart.pp_error ppf err
  | #Conduit_lwt.error as err -> Conduit_lwt.pp_error ppf err

let lwt_io =
  let open Lwt_scheduler in
  {
    Neg.Sigs.recv = (fun flow raw -> inj (Conduit_lwt.recv flow raw));
    Neg.Sigs.send = (fun flow raw -> inj (Conduit_lwt.send flow raw));
    Neg.Sigs.pp_error;
  }

let lwt_fail exn = Lwt_scheduler.inj (Lwt.fail exn)
