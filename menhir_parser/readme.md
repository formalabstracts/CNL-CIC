This directory contains experimental files related to a CNL-CIC parser.
The files are here for preliminary design purposes only.  
The files use ocaml, dune, sedlex, menhir. 

Lexer
 * [sedlex](https://github.com/ocaml-community/sedlex) lexer is used.
   The file can be run interactively in a top loop.

 * [Math symbols in unicode](https://www.fileformat.info/info/unicode/category/Sm/list.htm)
   We can easily expand our symbol list as needed.

Using ppx_deriving.std, sedlex, menhir,


Parser
 * [menhir](https://github.com/pippijn/menhir) parser is used.

 * compilation, the first creates _build/token.ml containing Token type definition 

```
menhir --only-tokens --infer --explain --base "_build/token" parser_cnl.mly
menhir --infer --explain parser_cnl.mly
```

OCaml notes

 * [Ocaml lexical conventions](https://caml.inria.fr/pub/docs/manual-ocaml/lex.html)


Build
 * dune clean
 * dune build
 * dune runtest
 

References:
 * Paskevich "The syntax and semantics of the ForTheL language" 
   (Dec 2007)
 * Avigad, Ebner, Ullrich "The Lean Reference Manual Release 3.3.0" 
   (Oct 24, 2017)