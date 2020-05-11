module Sigs = Sigs
module Find_common = Find_common
module Default = Default

type configuration = Find_common.configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
}

let run = Find_common.run

let find_common = Find_common.find_common

let tips = Find_common.tips
