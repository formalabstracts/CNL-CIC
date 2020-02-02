(* command line file *)

open Cnl_parse__Lexer_cnl
(* open Cnl_parse__Type *)
open Cnl_parse__Parser

let take = Cnl_parse__Lib.take

let convert_node = Cnl_parse.Lexer_cnl.lex_string 

(* deprecate 
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;
*)

let read_all = 
  Core.In_channel.read_all 

let mk_nodes infile  = 
  let ss = read_all infile in 
  let toks = convert_node (Filename.basename infile) ss in 
  toks 

let rec test_it input  = 
  if input = [] then (print_endline "no errors") else
  let input' = 
    try 
      let (_,(_,input')) = text input in
      input' 
    with Noparse tr 
       | Nocatch tr ->
           (
             
             let ((_,ns),_) = Cnl_parse.Parser.text_to_period input in 
             let s = Cnl_parse.Lexer_cnl.string_of_toks_plain (List.map tok ns) in 
             let trpos = line_col_string (line_col (List.hd ns)) in             
             print_endline (
                 "Error found at "^trpos^".  Unable to parse the text item starting with the sentence.");
             print_endline s;             
             print_endline "\nThe context in which the error appeared is the following.";
             print_endline (show_trace (clean_tr tr)); 
             print_endline "\nThe best match is the following.";
             print_endline (show_trace (clean_tr (snd (get_best_trace tr))));              
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



