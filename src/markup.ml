type t =
  | Bold of t list
  | Italic of t list
  | Text of string
  | Tt of t list

let rec named_list_to_string name l =
  List.map to_string l
  |> String.concat ","
  |> Printf.sprintf "%s[%s]" name 

and to_string = function
  | Bold l -> named_list_to_string "bold" l
  | Italic l -> named_list_to_string "italic" l
  | Text t -> t
  | Tt l -> named_list_to_string "tt" l
