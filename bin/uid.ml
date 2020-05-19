type t = string

type ctx = Digestif.SHA1.ctx

let ( <.> ) f g x = f (g x)

let length = Digestif.SHA1.digest_size

let compare a b = String.compare a b

let to_hex x = x

let of_hex x = x

let hash x = Hashtbl.hash x

let equal a b = String.equal a b

let to_raw_string = Digestif.SHA1.(to_raw_string <.> of_hex)

let of_raw_string = Digestif.SHA1.(to_hex <.> of_raw_string)

let null = Digestif.SHA1.(to_hex (digest_string ""))

let pp ppf t = Digestif.SHA1.(pp ppf (of_hex t))

let feed = Digestif.SHA1.feed_bigstring

let get ctx = Digestif.SHA1.(to_hex (get ctx))

let empty = Digestif.SHA1.empty
