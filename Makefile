build:
	cp parser_combinator/Combinator.ml .
	ocamlc -o parser Combinator.ml Parser.ml
	rm Combinator.ml
build_debug:
	cp parser_combinator/Combinator.ml .
	ocamlc -g -o parser Combinator.ml Parser.ml
	rm Combinator.ml
clean:
	rm Combinator.cm*
	rm Parser.cm*
	rm parser
compile_deps:
	ocamlopt -c parser_combinator/Combinator.ml
	mv parser_combinator/Combinator.o parser_combinator/Combinator.cm* .
clean_deps:
	rm Combinator.o Combinator.cm*
