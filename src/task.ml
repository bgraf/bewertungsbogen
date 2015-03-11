
type t = {
  name: string;
  kind: kind;
}

and kind =
  | Group of t list
  | Piece of int

let make_group ~name ~tasks =
  { name; kind = Group tasks }

let make_piece ~name ~points =
  { name; kind = Piece points }

let rec points { kind; } = match kind with
  | Group l -> List.fold_left (fun acc task -> acc + points task) 0 l
  | Piece n -> n

let is_group { kind; } = match kind with
  | Group _ -> true
  | _       -> false

let is_piece t = not (is_group t)

type info = string * string

type header = info list

type sheet = header * t list


