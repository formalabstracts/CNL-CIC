
let convert_toks s = 
 let _ = print_endline s in 
  let toks = Cnl_parse.Parser_cnl.lex_string s in
 let _ = print_endline ("->"^(Cnl_parse.Parser_cnl.tokens_to_string ". " toks)) in 
  toks;;



(* Tex2cnl.process_doc convert_toks  "../tex/sample_sylow.tex";; *)
