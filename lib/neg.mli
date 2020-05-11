module Sigs = Sigs
open Sigs

type 'uid configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
  of_hex : string -> 'uid;
  to_hex : 'uid -> string;
}

type ('a, 's) raise = exn -> ('a, 's) io

type 'uid negotiator

val negotiator : compare:('uid -> 'uid -> int) -> 'uid negotiator

val run : 's scheduler ->
    ('res, 's) raise ->
    ('flow, ([> ] as 'error), 's) flow ->
    'flow ->
    ('res, 'error) Smart.t ->
    ('res, 's) io

val find_common : 's scheduler ->
  ('flow, ([> `Protocol of Smart.error ] as 'error), 's) flow ->
  'flow ->
  'uid configuration ->
  ('uid, 'ref, ('uid * int ref * int64), 'g, 's) access ->
  ('uid, ('uid * int ref * int64), 'g) store ->
  'uid negotiator ->
  Smart.context ->
  'uid list ->
  (int, 's) io

val tips : 's scheduler -> ('uid, 'ref, ('uid * int ref * int64), 'g, 's) access -> ('uid, ('uid * int ref * int64), 'g) store -> 'uid negotiator -> (unit, 's) io
