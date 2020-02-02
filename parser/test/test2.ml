open Cnl_parse__Lexer_cnl
open Cnl_parse__Type
open Cnl_parse__Parser

let take = Cnl_parse__Lib.take

let convert_node = Cnl_parse.Lexer_cnl.lex_string 

let token_to_string = 
function
| STRING s ->  "\""^s^"\""
| CONTROLSEQ s -> "\\"^s
| DECIMAL s -> s
| INTEGER s ->  s
| SYMBOL s -> s
| SYMBOL_QED -> "[QED]"
| L_PAREN -> "("
| R_PAREN -> ")"
| L_BRACE -> "{"
| R_BRACE -> "}"
| MAPSTO -> "\\mapsto"
| PERIOD -> "."
| MID -> "|"
| TMID -> "//"
| COMMA -> ","
| SEMI -> ";"
| COLON -> ":"
| ASSIGN -> ":="
| ARROW -> "\to"
| BLANK -> "_"
| ALT -> "|"
| APPLYSUB -> "\\sb"
| SLASH -> "/"
| SLASHDASH -> "/-"
| COERCION -> "^"
| LAMBDA -> "\\"
| PITY -> "[PI]"
| QUANTIFIER s -> s
| ATOMIC_IDENTIFIER s -> s
| FIELD_ACCESSOR s -> s
| EOF -> "EOF"
| UNKNOWN s -> s^"?"
| _ -> failwith "token_to_string: not found"



let myt = (convert_node "test2.ml" "++0--9;3/-")

let test1()  = List.map print_string (List.map show_token (List.map tok myt));;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

let mk_nodes infile  = 
  let ss = read_lines infile in 
  let toks = List.map (convert_node (Filename.basename infile)) ss in 
  List.flatten toks 

let ns = mk_nodes   "../scripts/sylow.cnl";;
let ns100 = take 70 ns;;


let test2 = List.map print_endline
             (List.map show_token (List.map tok (ns100)))

let rec test_it input  = 
  if input = [] then () else
  let input' = 
    try 
      let (t,(_,input')) = text input in
      let _ = print_endline (show_text_node t) in 
      input' 
    with Noparse tr 
       | Nocatch tr ->
           (
             print_endline (show_trace (clean_tr tr)); 
             print_endline ("\n\nA\n\n");
             ignore(List.map print_endline (List.map show_token 
                                              (List.map tok (take 40 input))));
             failwith "my-error"
           ) in
  test_it input';;


let _ = 
  try (test_it ns) with _ -> ();;

