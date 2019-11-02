This directory contains files related to a TeX to CNL converter.

The files use ocaml, sedlex.

Lexer
 * [sedlex](https://github.com/ocaml-community/sedlex) lexer is used.
   The file can be run interactively in a top loop.


Using  sedlex,


 

Build and Run

 * dune clean
 * dune build
 * dune runtest
 * dune build main.exe --verbose
 * _build/default/main.exe tex/sample_sylow.tex
