{
  open Smarkup_parser
}

rule token = parse
  | "**"                      { DSTAR }
  | '*'                       { STAR }
  | '`'                       { BACKTICK }
  | ([^'*' '`']+ as content)  { CONTENT content }
  | eof                       { EOF}
