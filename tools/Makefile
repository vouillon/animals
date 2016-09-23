
NAME=fetch_image_information

$(NAME): $(NAME).ml
	ocamlfind ocamlopt $<  -package xmlm,str,unix -linkpkg -o $@
