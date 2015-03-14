all:
	ocamlbuild -syntax camlp4o -use-ocamlfind -pkgs core_kernel,bap,bap.plugins,cmdliner,sexplib.syntax,ocamlgraph callstrings.native

rcallstrings:
	ocamlbuild -use-ocamlfind -pkgs core_kernel,bap,bap.plugins,cmdliner,ocamlgraph rcallstrings.native

visitor:
	ocamlbuild -syntax camlp4o -use-ocamlfind -pkgs core_kernel,bap,bap.plugins,cmdliner,sexplib.syntax,ocamlgraph visitor.native

test:
	bapbuild -pkg cmdliner,ocamlgraph test.native
	./test.native -runner sequential


clean:
	rm -r *.native _build *.callstr *.dot *.tbl *.png
