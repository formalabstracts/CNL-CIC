(* reference, unicode math version 3.1 16-Nov-16 *)

(* #include "parser_cab.ml" [@@deriving show] *)

module Sedlexing = Lexbuffer

exception LexError of (string * int * int * string * string)

let raise_lex (p: Lexing.position) tok msg =                
  let open Lexing in
  let line = p.pos_lnum in
  let col = p.pos_cnum - p.pos_bol in
  raise @@ LexError (p.pos_fname, line, col, tok, msg)


             (* sedlex format *)

(* start interactive section *)
(* #require "Batteries"
#require "sedlex"


open BatSet.String
open BatList 
 *)

(* -- Lexical structure -- *)

let numeral10 =
  [%sedlex.regexp? Plus('0'..'9')]
let natural_number =
  [%sedlex.regexp? numeral10]
let decimal =
  [%sedlex.regexp? Plus('0'..'9') , '.', Plus('0'..'9')]
let numeric =
  [%sedlex.regexp? Opt('+' | '-'), (natural_number | decimal)]
 
let eol =   [%sedlex.regexp?  '\n']

let white =
  [%sedlex.regexp? ' ' | '\t' | '\012' | '~' | '@' | '\r' ] 

let alphabet = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']

let alphanum = [%sedlex.regexp? alphabet | numeral10 | '_' | "'"]
             
let controlseq = [%sedlex.regexp? '\\', Plus(alphabet)]

let controlchar = [%sedlex.regexp? '\\', Compl(alphabet)]

let comment = [%sedlex.regexp? '%', Compl(eol)]

let arg = [%sedlex.regexp? '#', numeral10 ]

let lparen = [%sedlex.regexp? '(' ]  
let rparen = [%sedlex.regexp? ')' ]
let lbrack = [%sedlex.regexp? '[' ]  
let rbrack = [%sedlex.regexp? ']' ]
let lbrace = [%sedlex.regexp? '{' ]
let rbrace = [%sedlex.regexp? '}' ]
let ldisplay = [%sedlex.regexp? '\\', '['  ]
let rdisplay = [%sedlex.regexp? '\\', ']'  ]
let format_eol = [%sedlex.regexp? '\\']
let format_col = [%sedlex.regexp? '&']


let period = [%sedlex.regexp? '.']           
let comma = [%sedlex.regexp? ',']
let colon = [%sedlex.regexp? ':']
let semi = [%sedlex.regexp? ';']           
let punct = [%sedlex.regexp? period | comma | semi | colon ]

let dollar = [%sedlex.regexp? '$']
let sub = [%sedlex.regexp? '_']

let symbol = [%sedlex.regexp? punct | '|' | '<' | '>' | '^' | '+' | '-' | '=' | '/' | '*']


let unmarked_id_more = [%sedlex.regexp? alphanum | '.' ]
let unmarked_id = [%sedlex.regexp? alphabet, Star(unmarked_id_more), Plus(alphanum) ]
let id = [%sedlex.regexp? '!', unmarked_id, '!' ]
let tok = [%sedlex.regexp?  alphabet | unmarked_id | id ]

           
open Parser_tex

let lexeme = Sedlexing.lexeme

let rec lex_token buf =
  match%sedlex buf with
  | Plus(white) -> (lex_token buf)
    | natural_number -> NATURAL(int_of_string(lexeme buf))
    | numeric -> NUMERIC(lexeme buf)
    | eol -> EOL
    | controlseq -> CONTROLSEQ(lexeme buf)
    | controlchar -> CONTROLCHAR(lexeme buf)
    | arg -> ARG(int_of_string(lexeme buf))


    | rparen -> R_PAREN
    | lparen -> L_PAREN
    | lbrack -> L_BRACK
    | rbrack -> R_BRACK
    | lbrace -> L_BRACE
    | rbrace -> R_BRACE
    | ldisplay -> L_DISPLAY
    | rdisplay -> R_DISPLAY
    | dollar -> DOLLAR
    | format_eol -> FORMAT_EOL
    | format_col -> FORMAT_COL
    | tok -> TOK(lexeme buf) 
    | symbol -> TOK(lexeme buf)
    | eof -> EOF
    | any -> failwith (lexeme buf)
    | _ -> failwith (lexeme buf)

              
(* testing stuff *)


let lexing_positions _ =
  (Lexing.dummy_pos,Lexing.dummy_pos)

(* git version efcc changed name of this *)
let with_tokenizer (lexer' : Sedlexing.lexbuf -> token)  (buf : Sedlexing.lexbuf) :
      (unit -> token * Lexing.position * Lexing.position) =
  fun () ->
        let token = lexer' buf in
        let (start_p, curr_p) = lexing_positions buf in
    (token, start_p, curr_p)

(* test *)
let buf_example1 = Sedlexing.Utf8.from_string
               "hello there xx $ yy $"

(* both work on the same mutable buffer, *)
let ff = fun () -> lex_token buf_example1
BatList.map ff (BatList.init 5 (fun _ -> ()))

let gg = with_tokenizer lex_token buf_example1
gg()
 *)

(*
#require "MenhirLib"        
MenhirLib.Convert.traditional2revised
 *)
