(** [square x] returns x * x. *)
let square (_x : int) : int =
  (* EXERCISE: return the square of _x *)
  _x * _x

(** [is_empty s] returns true if the string [s] has length 0. *)
let is_empty (_s : string) : bool =
  (* EXERCISE: check if the string is empty *)
  String.length _s = 0

(** [greet name] returns the string "Hello, <name>!". *)
let greet (_name : string) : string =
  (* EXERCISE: use string concatenation (^) or Printf.sprintf *)
"Hello, " ^ _name ^ "!"
