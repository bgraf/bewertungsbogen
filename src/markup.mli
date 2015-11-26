type t =
  | Bold of t list
  | Italic of t list
  | Text of string
  | Tt of t list


val to_string : t -> string
