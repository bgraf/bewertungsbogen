
main:
	ocamlbuild -menhir "menhir --external-tokens Lexer" -use-ocamlfind main.native
clean:
	ocamlbuild -clean

run:
	./main.native
