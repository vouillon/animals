
all: hypertree.js

hypertree.byte: hypertree.ml
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax \
		-syntax camlp4o -linkpkg -o hypertree.byte hypertree.ml

hypertree.js: hypertree.byte
	js_of_ocaml hypertree.byte
