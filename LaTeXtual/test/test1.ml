
let convert_toks s = 
(*  let _ = print_endline s in   *)
  let toks = Latextual__Lexer.lex_string s in
(*  let _ = print_endline ("->"^(Latextual.tokens_to_string ". " toks)) in  *)
  toks;;

Latextual.process_doc convert_toks  "../tex/reinhardt-optimal-control.tex";;
