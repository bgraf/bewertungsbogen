%token EOF
%token STAR
%token DSTAR
%token BACKTICK
%token<string> CONTENT

%start <Markup.t list> main

%%

main:
| ms = nonempty_list(markup); EOF { ms }
;

markup:
| DSTAR; l = nonempty_list(dstar_rest); DSTAR { Markup.Bold l }
| STAR; l = nonempty_list(star_rest); STAR { Markup.Italic l }
| BACKTICK; l = nonempty_list(backtick_rest); BACKTICK { Markup.Tt l }
| t = text { t }
;

dstar_rest:
| STAR; l = nonempty_list(dstar_star_rest); STAR { Markup.Italic l }
| BACKTICK; l = nonempty_list(dstar_backtick_rest); BACKTICK { Markup.Tt l }
| t = text { t }
;

star_rest:
| DSTAR; l = nonempty_list(dstar_star_rest); DSTAR { Markup.Bold l }
| BACKTICK; l = nonempty_list(star_backtick_rest); BACKTICK { Markup.Tt l }
| t = text { t }
;

backtick_rest:
| STAR; l = nonempty_list(star_backtick_rest); STAR { Markup.Italic l }
| DSTAR; l = nonempty_list(dstar_backtick_rest); DSTAR { Markup.Bold l }
| t = text { t }
;

dstar_star_rest:
| BACKTICK; c = text; BACKTICK { Markup.Tt [c] }
| t = text { t }
; 

dstar_backtick_rest:
| STAR; c = text; STAR { Markup.Italic [c] }
| t = text { t }
; 

star_backtick_rest:
| DSTAR; c = text; DSTAR { Markup.Bold [c] }
| t = text { t }
; 

%inline text:
| c = CONTENT { Markup.Text c }
;
