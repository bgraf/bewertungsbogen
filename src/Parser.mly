%token EOF
%token INDENT
%token DEDENT
%token <int> NUMBER
%token <Lexer.info> INFO
%token <string> DATA

%{
%}

%start <Task.sheet> specification

%%

specification:
| h = header; t = tasks; EOF { h, t }
;

tasks:
| t = nonempty_list(task) { t }
| error { failwith "Expected list of tasks" }
;

task:
| points = NUMBER; name = DATA 
  { Task.make_piece ~name ~points }
| name = DATA; INDENT; tasks = tasks; DEDENT
  { Task.make_group ~name ~tasks }
;

header:
| l = list(info) { l }
;

info:
| info = INFO
  { let { Lexer.key; value } = info in key, value }
;

