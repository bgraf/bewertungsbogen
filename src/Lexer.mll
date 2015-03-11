
{

(*---------------------------------------------------------------------------*)

module State = struct

  type t = {
    mutable indents : int;
    mutable current : int;
    mutable look    : bool;
  }

  let make () = { indents = 0; current = 0; look = true }
  let current_increase t = t.current <- t.current + 1
  let current_reset t = t.current <- 0
  let is_indent t = t.current > t.indents
  let is_dedent t = t.current < t.indents
  let indents_increase t = t.indents <- t.indents + 1
  let indents_decrease t = t.indents <- t.indents - 1
  let look_on t = t.look <- true
  let look_off t = t.look <- false
end

(*---------------------------------------------------------------------------*)

type info = {
  key: string;
  value: string
}

type token =
  | EOF
  | DATA of string
  | NUMBER of int
  | INDENT
  | DEDENT
  | INFO of info

(*---------------------------------------------------------------------------*)

(** [decrease_current_position lexbuf] will decrease [lexbuf]'s position
    by one.                                                                  *)
let decrease_current_position lb =
  Lexing.(lb.lex_curr_pos <- lb.lex_curr_pos - 1)

}

(*---------------------------------------------------------------------------*)

let digit = ['0'-'9']
let white    = [' ' '\t']
let alpha    = ['A'-'Z' 'a'-'z']
let newline  = ['\n']
let number   = ['1'-'9'] digit*

rule lex s = parse
  | white* newline  { State.look_on s; lex s lexbuf }
  | white white
    { (* leading whitespace pair, handle possible indent *)
      State.current_increase s;
      
      if State.is_indent s then (
        State.indents_increase s;
        INDENT
      ) else (
        lex s lexbuf
      )

    }
  | eof
    { (* EOF, handle possible dedents *)
      
      if s.State.indents > 0 then (
        State.indents_decrease s;
        decrease_current_position lexbuf;
        DEDENT
      ) else
        EOF
    }
  | _
    { (* possible dedent *)
      decrease_current_position lexbuf;

      if not s.State.look then 
        lex_content s lexbuf
      else if State.is_dedent s then
        let () = State.indents_decrease s in
        DEDENT
      else 
        let () = State.current_reset s in
        lex_content s lexbuf
    }


and lex_content s = parse
  | white* newline { State.look_on s; lex s lexbuf }
  | white { lex_content s lexbuf }
  | '#' (['a'-'z']+ as key) white* ':' white* ([^'\n']+ as value)
  { INFO { key; value } }
  | number as numstr white*
    { State.look_off s; NUMBER (int_of_string numstr) }
  | ([^'0'-'9'] [^'\n']*) as data
    { DATA data }


