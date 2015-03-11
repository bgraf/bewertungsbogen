
(** Task module *)

(** {2 Tasks} *)

type t = {
  name: string;
  kind: kind;
}

and kind =
  | Group of t list
  | Piece of int


val make_group : name:string -> tasks:t list -> t

val make_piece : name:string -> points:int -> t

val points : t -> int

val is_group : t -> bool

val is_piece : t -> bool

(** {2 Sheets} *)

type info = string * string

type header = info list

type sheet = header * t list

