module Sigs = Sigs
module Find_common = Find_common
module Default = Default
open Find_common

type nonrec ('a, 's) raise = ('a, 's) raise

type nonrec 'uid configuration = 'uid configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
  of_hex : string -> 'uid;
  to_hex : 'uid -> string;
}

type 'uid negotiator = 'uid Default.t

let negotiator ~compare = Default.make ~compare

let run = run

let find_common = find_common

let tips = tips
