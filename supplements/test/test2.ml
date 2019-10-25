open Cnl_parse__Lexer_cnl
open Cnl_parse__Parser

let rec print_tok = function
  | [] -> ()
  | t :: ts -> 
      print_string(Cnl_parse.Lexer_cnl.lex_token_to_string t ^ " ") ; print_tok ts

let convert_node s = 
(* let _ = print_endline "*" in *)
 let toks = Cnl_parse.Lexer_cnl.lex_string s in
(* let _ = print_tok toks in *)
 toks

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

let rec take k =
  function 
  | t :: ts -> if (k <=0) then [] else t :: take (k - 1) ts
  | _ -> []


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
let ns100 = take 50 ns;;


let test2 = List.map print_endline
             (List.map show_token (List.map tok (ns100)))

let (t,x) = text ns100 ;;
  print_endline (show_text_node t)

let (t,x) = text x;;
  print_endline (show_text_node t)

let (t,x) = text x;;
  print_endline (show_text_node t);;

let (t,x) = text x;;
  print_endline (show_text_node t);;

let (t,x) = text x;;
  print_endline (show_text_node t);;

print_nodes x;;



let (t,x) = (definition_preamble x);;
(*   print_endline (show_text_node t);; *)

print_nodes x;;
print_endline "A";;
let (_,x) = (assumption x);;

print_nodes x;;
(*

 (List.map (fun a -> print_endline (show_wordpattern (fst a))) t);;

 *)





