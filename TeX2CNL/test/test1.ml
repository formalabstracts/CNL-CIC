
let convert_toks s = 
(*  let _ = print_endline s in  *)
  let toks = Tex2cnl__Lexer_tex.lex_string s in
(*  let _ = print_endline ("->"^(Tex2cnl.tokens_to_string ". " toks)) in *)
  toks;;



Tex2cnl.process_doc convert_toks  "../tex/sample_sylow.tex";;
