(* reference, unicode math version 3.1 16-Nov-16 *)



             (* sedlex format *)

(* start interactive section *)
(* #require "Batteries"
#require "sedlex"


open BatSet.String
open BatList 
 *)

type token = 
   | Natural of int
   | Numeric of string
   | Eol
   | ControlSeq of string
   | ControlChar of string
   | Arg of int
   | LParen
   | RParen
   | LBrack
   | RBrack
   | LBrace
   | RBrace
   | LDisplay
   | RDisplay
   | Dollar
   | Sub
   | FormatEol 
   | FormatCol 
   | Tok of string
   | Symbol of string
   | Eof
   | NotImplemented;;

let to_string = function
  | Natural i -> (string_of_int i)
  | Numeric s -> s
  | ControlSeq s -> s
  | ControlChar s -> s
  | Arg i -> "#"^(string_of_int i)
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Dollar -> "$"
  | Sub -> "\\sb"
  | FormatEol -> "&"
  | FormatCol -> "\\"
  | Tok s -> s
  | Symbol s -> s
  | Eof -> "EOF"
  | NotImplemented -> "NotImplemented"
  | _ -> "";;


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

           
(* open Parser_tex *)

let lexeme = Sedlexing.lexeme;;

let implode l = List.fold_right (^) l "";;

let string_of_ints js =
 let cs =  List.map (fun j -> Char.escaped (Char.chr j)) (Array.to_list js) in
  implode cs;;

let string_lexeme buf = string_of_ints(lexeme buf);;

let rec lex_token buf = 
 match%sedlex buf with 
 | Plus(white) -> (lex_token buf)
  | natural_number -> Natural(int_of_string(string_lexeme buf)) 
    | numeric -> Numeric(string_lexeme buf)
    | eol -> Eol
    | controlseq -> ControlSeq(string_lexeme buf)
    | controlchar -> ControlChar(string_lexeme buf)
    | arg -> Arg(int_of_string(string_lexeme buf))
    | rparen -> RParen
    | lparen -> LParen
    | lbrack -> LBrack
    | rbrack -> RBrack
    | lbrace -> LBrace
    | rbrace -> RBrace
    | ldisplay -> LDisplay
    | rdisplay -> RDisplay
    | dollar -> Dollar
    | format_eol -> FormatEol
    | format_col -> FormatCol
    | tok -> Tok(string_lexeme buf) 
    | symbol -> Tok(string_lexeme buf)
    | eof -> Eof
    | any -> failwith (string_lexeme buf)
    | _ -> failwith (string_lexeme buf)


              
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
let buf_example1 = Sedlexing.Latin1.from_string
               "hello\\alpha33[1]there !ready! (xx) [$] {yy} $"



(* both work on the same mutable buffer, *)
let ff = fun () -> print_endline(to_string(lex_token buf_example1));;


BatList.map ff (BatList.init 18 (fun _ -> ()));;


