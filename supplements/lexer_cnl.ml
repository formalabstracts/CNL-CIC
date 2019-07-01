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
let number =
  [%sedlex.regexp? numeral10]
let decimal =
  [%sedlex.regexp? Plus('0'..'9') , '.', Plus('0'..'9')]
let numeric =
  [%sedlex.regexp? Opt('+' | '-'), (number | decimal)]

  
let white =
  [%sedlex.regexp? ' ' | '\r' | '\n' | '\t' | '\012' ] 

let string_escape = [%sedlex.regexp?
'\\' , ('\\' | '"' | "'" | "n" | "t" )]

let string_char = [%sedlex.regexp? Compl('\\' | '\"')]

let string_item = [%sedlex.regexp? string_char | string_escape  ]

let string = [%sedlex.regexp? '"', Star(string_item), '"']

let alphabet = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let alphanum = [%sedlex.regexp? alphabet | numeral10 | '_' | "'"]
             
let controlseq = [%sedlex.regexp? '\\', Plus(alphabet)]
let var = [%sedlex.regexp? alphabet, Star(numeral10 | "'" | "_")]
let token = [%sedlex.regexp? Plus(alphabet) ]
let word = [%sedlex.regexp? alphabet, Star(alphanum) ]          

(* numerical literals *)

  

let lparen = [%sedlex.regexp? '(' ]  
let rparen = [%sedlex.regexp? ')' ]
let lbrack = [%sedlex.regexp? '[' ]  
let rbrack = [%sedlex.regexp? ']' ]
let lbrace = [%sedlex.regexp? '{' ]
let rbrace = [%sedlex.regexp? '}' ]

let period = [%sedlex.regexp? '.']           
let comma = [%sedlex.regexp? ',']
let semi = [%sedlex.regexp? ';']           
let assign = [%sedlex.regexp? ":="]
let alt = [%sedlex.regexp? "|"]
let slash = [%sedlex.regexp? "/"]
let slashdash = [%sedlex.regexp? "/-"]
        
let blank = [%sedlex.regexp? '_']

let symbol = [%sedlex.regexp? '*' | '+' | '^' | '=' | '<' | '>' | '/']
                    
let op_char_no_dot =   [%sedlex.regexp?
                  '+'
                | '&'
                       ]

let unicode = [%sedlex.regexp?
                  Compl(' ')]

(* tilde starts the suffix, in which more general character sequences are allowed: *)

           
open Parser_cnl

let lexeme = Sedlexing.lexeme

let rec lex_token buf =
  match%sedlex buf with
  | Plus(white) -> (lex_token buf)
    | string -> STRING (lexeme buf)
    | controlseq -> CONTROLSEQ(lexeme buf)
    | number -> NUMBER(lexeme buf)
    | decimal -> DECIMAL(lexeme buf)
    | numeric -> NUMERIC(lexeme buf)
    | symbol -> SYMBOL(lexeme buf)
    | rparen -> R_PAREN (lexeme buf)
    | lparen -> L_PAREN (lexeme buf)
    | lbrack -> L_BRACK (lexeme buf)
    | rbrack -> R_BRACK (lexeme buf)
    | lbrace -> L_BRACE (lexeme buf)
    | rbrace -> R_BRACE (lexeme buf)
    | period -> PERIOD (lexeme buf)
    | comma -> COMMA (lexeme buf)
    | semi -> SEMI (lexeme buf)
    | assign -> ASSIGN (lexeme buf)
    | alt -> ALT (lexeme buf)
    | slash -> SLASH (lexeme buf)
    | slashdash -> SLASHDASH (lexeme buf)
    | blank -> BLANK (lexeme buf)
    | var -> VAR (lexeme buf)
    | token -> TOKEN (lexeme buf)
    | word -> WORD (lexeme buf)
    | eof -> EOF
    | any -> failwith (lexeme buf)
    | _  -> failwith (lexeme buf)



              
(* testing stuff *)

(*
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
               "'' ``~ ' ` ~ 'a '+ (**) X (* hi *) Y (***) X  (* U )) *) V with | thm theorem then structure α \"string\x65\x65\" Sort Type , ; ) ] } post postulate . package := == # #. #.# *. 123 notation match ( [ { let inductive in import if hi.hi ~mod hi~# #print #eval # end else def 33.33 , : _ _33 _# _*_ := hello #reduce ##hi := structure let in with match :: // #print this.that by universe/-  -/ :+: :: (:= == :) {|} *a *.*_ ... ++ +- +0 +012 my oh.( + ) ohm.≪ - ≫ hello /--//- -//- hi --/'a'\"hello\"1234 0x33AF 0b0101 0o44 /-- abc -/ /-! bye -/'z'/--/ abc print a.b.c a.( + ) a.0.44"

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
