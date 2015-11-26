(*===========================================================================*.
|*                                                                           *|
|*   Bewertungsbogen                                                         *|
|*                                                                           *|
|*   Author:                                                                 *|
|*      Benjamin Graf (bgraf at uos de)                                      *|
|*   Version:                                                                *|
|*      2015-11-27 0.1.0                                                     *|
|*                                                                           *|
`*===========================================================================*)


open Printf
open Batteries

(*---------------------------------------------------------------------------*)

let parse_input inp =
  let lexfun =
    let lexstate = Lexer.State.make () in
    Lexer.lex lexstate
  and lexbuf = Lexing.from_input inp in
  try
    Ok (Parser.specification lexfun lexbuf)
  with
  | Parser.Error -> Bad("some error")
  | Failure s -> begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let msg = Printf.sprintf "%s at line %d:%d, token: '%s'" s line cnum tok in
      Bad(msg)
    end

(*---------------------------------------------------------------------------*)

module Header = struct
  
  module Transformers = struct
    let positive_int s =
      try
        let i = int_of_string s in
        if i > 0 then Ok i else Bad "not a positive integer"
      with
      | _ -> Bad "not an integer"
  end

  let get ~key ~f h =
    try
      List.assoc key h |> f
    with
    | Not_found -> Bad "not found."

  let get_int_positive = get ~f:Transformers.positive_int

  let get_string = get ~f:Result.Monad.return

end

(*---------------------------------------------------------------------------*)

let exit_error msg =
  sprintf "Error: %s\n" msg |> failwith


let parse_file ~file =
  let header, tasks =
    let tasks_res = File.with_file_in file parse_input in
    match tasks_res with
    | Bad msg            -> exit_error msg
    | Ok (header, tasks) -> header, tasks
  in
  let groups =
    match Header.get_int_positive ~key:"group" header with
    | Ok i -> i
    | Bad msg -> exit_error ("header 'group' " ^ msg)
  and title =
    match Header.get_string ~key:"title" header with
    | Ok t -> t
    | Bad msg -> exit_error ("header 'title' " ^ msg)
  in
  (tasks, title, groups)


(** [output_filename path] extracts the filename from a path and
    appends ".pdf" to it. *)
let output_filename name =
  let prefix =
    try
      let index = String.rindex name '/' in
      let len = String.length name in
      String.sub name (index + 1) (len - index - 1)
    with _ -> name
  in
  prefix ^ ".pdf"


let main file =
  try
    let tasks, title, groups = parse_file ~file in

    let module H =
      Output.Show(struct
        let group_column_count = groups
        let title = title
      end)
    in

    let output_filename = output_filename file in

    let wk_stdin =
      Unix.open_process_out
        ("wkhtmltopdf --encoding utf8 - " ^ output_filename) in

    H.show ~f:(fun s -> Printf.fprintf wk_stdin "%s" s) tasks;

    match Unix.close_process_out wk_stdin with
    | Unix.WEXITED i when i == 127 ->
      exit_error "Error while running `wkhtmltopdf`."
    | Unix.WEXITED i when i == 0 ->
      `Ok (Printf.printf "Created file '%s'\n" output_filename)
    | _ ->
      exit_error "Error while running `wkhtmltopdf`."
  with
  | Failure s -> `Error (true, s)

(*---------------------------------------------------------------------------*)

open Cmdliner

let make_cmdline () =
  let file_arg = 
    let doc = "Input source file." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"SOURCE" ~doc)
  in
  let cmd =
    let doc = "Bewertungsbogen generator" in
    let man = [] in
    Term.(ret (pure main $ file_arg)),
    Term.info "bewertungsbogen" ~man ~doc ~version:"0.1.0"
  in
  cmd

let () =
  let cmdline = make_cmdline () in
  match Term.eval cmdline with `Error _ -> exit 1 | _ -> exit 0 

