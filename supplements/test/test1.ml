
let rec print_tok = function
  | [] -> ()
  | t :: ts -> 
      print_endline(Cnl_parse.Lexer_cnl.lex_token_to_string t) ; print_tok ts

let convert_toks s = 
 let _ = print_endline "*" in
 let toks = Cnl_parse.Lexer_cnl.lex_string s in
 let _ = print_tok toks in
()


let _ = convert_toks "ABC DEF\"abc\"\\abc \\@#3.4 5+4.5-6$#\\qed()[]{}|-> .\\mid\\tmid , := -> _\\alt|\\sub/ /-\\^ \\\\ \\lam\\lambder\\Pity\\forall\\exists44 X Y x y33 x__rad75'_ hello her_22 ab.cd.ef 1.rt.. ww._" 



