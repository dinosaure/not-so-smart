type t = string

let length = 20

let compare a b = String.compare a b

let to_hex x = x

let of_hex x = x

let hash x = Hashtbl.hash x

let equal a b = String.equal a b
