CC=ocamlc -g -a
FIND=ocamlfind query

all: async pi

async: async_await.ml
	$(CC) -I $(shell $(FIND) unix) unix.cma async_await.ml -o async_await.cma

pi: pi.ml
	$(CC) -I $(shell $(FIND) unix) unix.cma pi.ml -o pi.cma

clean:
	rm -f *.cmo *.cmx *.cmi *.cma
	rm -f async_await
