FIND=ocamlfind query

all: async game generators pi

async: async_await.ml
	ocamlopt unix.cmxa async_await.ml -o async_await

game: guess_the_number.ml
	ocamlopt guess_the_number.ml -o game

generators: generators.ml
	ocamlopt generators.ml -o generators

pi: pi.ml
	ocamlopt pi.ml -o pi

clean:
	rm -f *.cmo *.cmx *.cmi *.cma *.o
	rm -f async_await game generators pi
