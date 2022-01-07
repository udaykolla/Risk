MODULES=authors board card command dispatcher gui player state  
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte

OCAMLBUILD=ocamlbuild -use-ocamlfind -pkgs camlimages.all

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

clean:
	ocamlbuild -clean
	rm -rf _doc finalproject.zip *.cm* *.o *.byte* _doc.public
zip:
	zip finalproject.zip *.ml *.mli *.txt *.json *.sh *.jpg _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile
loc:
	cloc --by-file --include-lang=OCaml .
install:
	opam update
	opam upgrade
	opam install ounit2
	opam install yojson
	opam install ANSITerminal
	opam install graphics
	opam install camlimages
	opam install lablgtk

docs: docs-public

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,graphics,camlimages.all \
		-html -stars -d _doc.public $(MLIS)
