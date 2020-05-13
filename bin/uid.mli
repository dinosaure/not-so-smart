type t = private string

val length : int

val compare : t -> t -> int

val of_hex : string -> t

val to_hex : t -> string

val equal : t -> t -> bool

val hash : t -> int
