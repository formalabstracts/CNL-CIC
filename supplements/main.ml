(* command line file *)

open Cnl_parse__Lexer_cnl
open Cnl_parse__Type
open Cnl_parse__Parser

let take = Cnl_parse__Lib.take

let convert_node = Cnl_parse.Lexer_cnl.lex_string 

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

let rec test_it input  = 
  if input = [] then (print_endline "no errors") else
  let input' = 
    try 
      let (_,(_,input')) = text input in
(*      let _ = print_endline (show_text_node t) in  *)
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


(* main *)

(* Uses JaneStret Command.Param *)

let command = 
  let open Core_kernel in 
  let open Command.Let_syntax in 
    Command.basic ~summary:"Coarse parsing of a  Colada file"
    [%map_open
    let filename = anon("filename" %: string) in 
        fun () -> 
              let () = print_endline ("Reading "^filename) in 
              let ns = mk_nodes filename in 
              let () = print_endline ("Tokenization of "^filename^" is complete.") in 
              try (test_it ns) with _ -> ()
    ];;
   

Core.Command.run ~version:"0.1" command
(*
let usage_help_string = "main.exe filename"               

let _ = 
  let args = Sys.argv in 
  if Array.length args < 2 then 
    print_endline ("usage: "^usage_help_string)
  else 
    let filename = args.(1) in 
    let _ = print_endline ("reading "^filename) in 
    let ns = mk_nodes filename in 
    try (test_it ns) with _ -> ();;

 *)



