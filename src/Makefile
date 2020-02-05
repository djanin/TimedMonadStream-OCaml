
all : count MonadStream MonadStreamRef Monad MonadRef MVar Main
	ocamlopt -O3 -o test -I +threads unix.cmxa  threads.cmxa Monad.cmx  MVar.cmx   MonadRef.cmx  MonadStream.cmx MonadStreamRef.cmx main.cmx

# ocamlc -I +threads unix.cma  threads.cma monad.cmo monadRef.cmo mVar.cmo monadStream.ml 

count : count.c
	gcc -o count count.c

doc : all
	cd ../doc; ocamldoc  -html -keep-code -colorize-code -I +threads -I ../_build/src ../src/*.ml

Main : MonadStreamRef MonadStream Monad MonadRef MVar main.ml
	ocamlopt -O3  -ccopt O3   -I +threads unix.cmxa threads.cmxa -c main.ml


MonadStreamRef : MonadStreamRef.ml MonadStream Monad MonadRef MVar
	ocamlopt -O3  -ccopt O3   -I +threads unix.cmxa threads.cmxa -c MonadStreamRef.ml

MonadStream : MonadStream.ml Monad MonadRef MVar
	ocamlopt -O3  -ccopt O3   -I +threads unix.cmxa threads.cmxa -c MonadStream.ml

Monad : Monad.ml
	ocamlopt -O3  -ccopt O3   -I +threads unix.cmxa threads.cmxa -c Monad.ml

MonadRef : MonadRef.ml Monad MVar
	ocamlopt -O3  -ccopt O3   -I +threads unix.cmxa threads.cmxa -c  MonadRef.ml

MVar : MVar.ml
	ocamlopt -O3  -ccopt O3   -I +threads unix.cmxa threads.cmxa -c  -c MVar.mli
	ocamlopt -O3  -ccopt O3   -I +threads unix.cmxa threads.cmxa -c  -c MVar.ml

clean :
	rm -f *.cmo *.cmi test *.cmx *.o *.cmxa
