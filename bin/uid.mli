type t = private string

type ctx

val length : int

val compare : t -> t -> int

val of_hex : string -> t

val to_hex : t -> string

val equal : t -> t -> bool

val hash : t -> int

val to_raw_string : t -> string

val of_raw_string : string -> t

val null : t

val pp : t Fmt.t

val feed : ctx -> ?off:int -> ?len:int -> Bigstringaf.t -> ctx

val get : ctx -> t

val empty : ctx
