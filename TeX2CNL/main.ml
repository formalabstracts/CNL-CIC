
let convert_toks s = 
  let toks = Tex2cnl__Lexer_tex.lex_string s in
  toks

let _ = 
  let args = Sys.argv in 
  if Array.length args < 2 then 
    print_endline "usage: main.exe filename"
  else ignore(Tex2cnl.process_doc convert_toks  (args.(1)))
