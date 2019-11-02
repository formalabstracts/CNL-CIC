(*
open Lexer_tex;;
open Tex2cnl;;
 *)


let convert_toks s = 
  let toks = Tex2cnl__Lexer_tex.lex_string s in
  toks;;



Tex2cnl.process_doc convert_toks  "../tex/sample_sylow.tex";;
