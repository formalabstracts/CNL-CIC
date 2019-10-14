This directory contains experimental files related to a LaTeX to Text conversion
for machine learning purposes.  Many things in the source file are ignored.

Cloned from TeX2CNL on October 13, 2019.

The files use ocaml, sedlex.

Lexer
 * [sedlex](https://github.com/ocaml-community/sedlex) lexer is used.
   The file can be run interactively in a top loop.


Using  sedlex,


Old Build
 * 	ocamlfind ocamlc -c -package sedlex -package batteries lexer_tex.ml 
 * ocamlfind ocamlc -o lexer_tex.byte -linkpkg -thread -package sedlex -package batteries lexer_tex.cmo
 * ocamlrun lexer_tex.byte

 

Build
 * dune clean
 * dune build
 * dune runtest
