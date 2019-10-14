
let convert_toks s = 
(*  let _ = print_endline s in  *)
  let toks = Latextual__Lexer_tex.lex_string s in
(*  let _ = print_endline ("->"^(Latextual.tokens_to_string ". " toks)) in *)
  toks;;



Latextual.process_doc convert_toks  "../tex/sample_sylow.tex";;
