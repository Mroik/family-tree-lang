build:
	cp parser_combinator/combinator.ml .
	ocamlc -o parser combinator.ml parser.ml
	rm combinator.ml
build_debug:
	cp parser_combinator/combinator.ml .
	ocamlc -g -o parser combinator.ml carser.ml
	rm combinator.ml
clean:
	rm combinator.cm*
	rm parser.cm*
	rm parser
compile_deps:
	ocamlopt -c parser_combinator/combinator.ml
	mv parser_combinator/combinator.o parser_combinator/combinator.cm* .
clean_deps:
	rm combinator.o combinator.cm*
