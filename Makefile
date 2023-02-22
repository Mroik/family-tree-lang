compile_deps:
	ocamlopt -c parser_combinator/Combinator.ml
	mv parser_combinator/Combinator.o parser_combinator/Combinator.cm* .
