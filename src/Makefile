
all: main.native

main.native:
	ocamlbuild -menhir "menhir --external-tokens Lexer" -use-ocamlfind main.native
clean:
	ocamlbuild -clean
	rm -fR bewertungsbogen bewertungsbogen.zip

run:
	./main.native

package: bewertungsbogen.zip

bewertungsbogen.zip: main.native
	cp main.native bewertungsbogen
	zip -r bewertungsbogen.zip bewertungsbogen
