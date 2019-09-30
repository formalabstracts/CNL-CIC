(* ocamlfind ocamlc -c -package sedlex my_file.ml *)

(*

Lexical structure of Colada language in Sedlex. 

The longest match rule holds (following sedlex).

  PERIOD is used to demarcate statements (and sections, etc.)
  Most parsing can be done on a statement by statement basis.
  (That is, to parse a statement, it is never necessary to look further ahead.)

  Delimiters, COMMA, and SEMI cannot be combined into larger lexemes. 
  Delimiters must alway occur in properly nested matching pairs. 

  The only characters that may appear in identifiers are A-Z a-z 0-9 ' _ PERIOD
  The initial character must be alphabetic [a-zA-Z].
  The final character must not be a PERIOD.

  According to the internal structure of the identifier, the identifier is
  classified as
  a variable, an atomic identifier, a hierarchical identifier, or a word. 
  A word is case insensitive, but other types of identifiers are case sensitive. 
  There are no keywords, but many words (if, then, else, section, etc.)
  have special meaning in particular
  contexts. 

  A field accessor begins with a PERIOD and otherwise has the same structure
  as a hierarchical identifier. 

  A control sequence is sequence of characters starting with backslash \
  and continuing with alphabetic characters [a-z][A-Z].  
  Control sequences are case sensitive.

  A second form of control sequence consists of a single backslash followed
  by a single character. 
  The second character cannot be alphabetic, a delimiter, or (most) separators. 

  

 *)


(* module Sedlexing = Lexbuffer *)

(*
exception LexError of (string * int * int * string * string)

let raise_lex (p: Lexing.position) tok msg =                
  let open Lexing in
  let line = p.pos_lnum in
  let col = p.pos_cnum - p.pos_bol in
  raise @@ LexError (p.pos_fname, line, col, tok, msg)
 *)

(* type *)

type token = 
  | STRING of string
  | CONTROLSEQ of string
  | DECIMAL of string
  | INTEGER of string
  | SYMBOL of string
  | SYMBOL_QED of string
  | L_PAREN of string
  | R_PAREN of string
  | L_BRACK of string
  | R_BRACK of string
  | L_BRACE of string
  | R_BRACE of string
  | MAPSTO of string
  | PERIOD of string
  | MID of string
  | TMID of string
  | COMMA of string
  | SEMI of string
  | COLON of string
  | ASSIGN of string
  | ARROW of string
  | BLANK of string
  | ALT of string
  | APPLYSUB of string
  | SLASH of string
  | SLASHDASH of string
  | COERCION of string
  | LAMBDA of string
  | PITY of string
  | QUANTIFIER of string
  | VAR of string
  | WORD of string
  | ATOMIC_IDENTIFIER of string
  | HIERARCHICAL_IDENTIFIER of string
  | FIELD_ACCESSOR of string
  | EOF
  | UNKNOWN of string 


(* -- Lexical structure -- *)

let digit =
  [%sedlex.regexp? '0'..'9']
let number =
  [%sedlex.regexp? Plus(digit)]
let integer = 
  [%sedlex.regexp? ('+' | '-'), number]
let decimal =
  [%sedlex.regexp? Opt('+' | '-'), number , '.', number]

let eol =   [%sedlex.regexp?  '\n']

let comment = [%sedlex.regexp? '%', Star(Compl(eol))]
  
let white =
  [%sedlex.regexp? ' ' | '\r' | '\n' | '\t' | '\012' ] 

let string_escape = [%sedlex.regexp?
'\\' , ('\\' | '"' | "'" | "n" | "t" )]

let string_char = [%sedlex.regexp? Compl('\\' | '\"')]

let string_item = [%sedlex.regexp? string_char | string_escape  ]

let string = [%sedlex.regexp? '"', Star(string_item), '"']

let alphabet = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']

let alphanum = [%sedlex.regexp? alphabet | digit | '_' | "'"]
             
let controlseq = [%sedlex.regexp? '\\', Plus(alphabet)]

let varlong = [%sedlex.regexp? alphabet, '_', '_', Star(alphanum)] (* mangling *)

let atomic_identifier = [%sedlex.regexp? alphabet, Star(alphanum) ]          

let hierarchical_identifier = [%sedlex.regexp? atomic_identifier, Plus('.', atomic_identifier) ]  

let field_accessor = [%sedlex.regexp? Plus('.', atomic_identifier)]


let lparen = [%sedlex.regexp? '(' ]  
let rparen = [%sedlex.regexp? ')' ]
let lbrack = [%sedlex.regexp? '[' ]  
let rbrack = [%sedlex.regexp? ']' ]
let lbrace = [%sedlex.regexp? '{' ]
let rbrace = [%sedlex.regexp? '}' ]
let comma = [%sedlex.regexp? ',']
let semi = [%sedlex.regexp? ';']           

(* symbol excludes '\' *)
          
let symbol = [%sedlex.regexp? '*' | '+' | '^' | '=' | '<' | '>' | '/' | '!' | 
              '@' | '#' | '$' | '&' | '_' | '-' | '|' | ':' | "'" | '.' |
              '?' | '~' | '`'] 

let symbolseq = [%sedlex.regexp? Plus(symbol)]

let controllable = [%sedlex.regexp? '.' | ',' | ':' | ';' | '|' | '\\' ]

let controlchar = [%sedlex.regexp? '\\', (symbol | controllable) ]

let controlkey s = match s with
| "\\qed" -> SYMBOL_QED s
| "\\mid" -> MID s
| "\\tmid" -> TMID s
| "\\alt" -> ALT s 
| "\\sub" -> APPLYSUB s
| "\\^" -> COERCION s
| "\\to" -> ARROW s
| "\\mapsto" -> MAPSTO s
| "\\blank" -> BLANK s
| "\\\\" -> LAMBDA s
| "\\lam" -> LAMBDA s
| "\\lambder" -> LAMBDA s
| "\\Pity" -> PITY s
| "\\forall" -> QUANTIFIER s
| "\\exists" -> QUANTIFIER s
| "\\existsunique" -> QUANTIFIER s
| _ -> CONTROLSEQ s;;

let symbolkey s = match s with
  | "." -> PERIOD s
  | ":" -> COLON s
  | ":=" -> ASSIGN s
  | "->" -> ARROW s
  | "|->" -> MAPSTO s
  | "|" -> ALT s
  | "/" -> SLASH s
  | "/-" -> SLASHDASH s
  | "_" -> BLANK s 
  | _ -> SYMBOL s


(* We avoid lookahead by doing a post-tokenization regexp test. *)

let var = [%sedlex.regexp? alphabet, Star(digit | "'" | "_")]
let word = [%sedlex.regexp? Plus(alphabet) ]

let identkey = 
  let is_var s = 
    Str.string_match (Str.regexp "[a-zA-Z][0-9'_]*$") s 0 in
  let is_word s = 
    Str.string_match (Str.regexp "[a-zA-Z]+$") s 0 in
  fun s -> 
        if is_var s then VAR s
        else if is_word s then WORD s
        else ATOMIC_IDENTIFIER s
           
(* open Parser_cnl *)

let lexeme = Sedlexing.lexeme

let implode l = String.init (List.length l) (List.nth l)

let string_of_ints js =
 let cs =  List.map (fun j ->  (Uchar.to_char j)) (Array.to_list js) in
  implode cs;;

let string_lexeme buf = string_of_ints(lexeme buf);;

(* 
  The order matters here.  The first match is applied.

  Note the folowing important particular orderings:
  * (+/-) DECIMAL - INTEGER - symbolseq 
  * (periods) field_accessor - symbolseq (other periods are internal to a lexeme)
  * varlong - word - atomic_identifier (after parsing as atomic)

  Some words should actually be treated as 
  case sensitive atomic_identifiers (such as sin, cos, tan).
  This is left to the parser. 
 *)

let rec lex_token buf =
  match%sedlex buf with
  | Plus(white) -> (lex_token buf)
  | comment -> (lex_token buf)
    | string -> STRING (string_lexeme buf)
    | controlseq -> controlkey(string_lexeme buf)
    | controlchar -> controlkey(string_lexeme buf)
    | decimal -> DECIMAL(string_lexeme buf)
    | integer -> INTEGER(string_lexeme buf)
    | number -> INTEGER(string_lexeme buf)
    | rparen -> R_PAREN (string_lexeme buf)
    | lparen -> L_PAREN (string_lexeme buf)
    | lbrack -> L_BRACK (string_lexeme buf)
    | rbrack -> R_BRACK (string_lexeme buf)
    | lbrace -> L_BRACE (string_lexeme buf)
    | rbrace -> R_BRACE (string_lexeme buf)
    | comma -> COMMA (string_lexeme buf)
    | semi -> SEMI (string_lexeme buf)
    | field_accessor -> FIELD_ACCESSOR (string_lexeme buf)
    | hierarchical_identifier -> HIERARCHICAL_IDENTIFIER (string_lexeme buf)
    | symbolseq -> symbolkey (string_lexeme buf)
    | varlong -> VAR (string_lexeme buf) 
    | atomic_identifier -> identkey (string_lexeme buf)
    | eof -> EOF 
    | any -> UNKNOWN (string_lexeme buf)
    | _  -> failwith (string_lexeme buf)

(* testing *)

let lex_token_to_string = function
  | STRING s -> s ^ " (STRING)"
  | CONTROLSEQ s -> s ^ "(CONTROL)"
  | DECIMAL s -> s ^ " (DECIMAL)"
  | INTEGER s -> s ^ " (INTEGER)"
  | SYMBOL s -> s ^ " (SYMBOL)"
  | SYMBOL_QED _ -> "(QED)"
  | L_PAREN _ -> "(L_PAREN)"
  | R_PAREN _ -> "(R_PAREN)"
  | L_BRACK _ -> "(L_BRACK)"
  | R_BRACK _ -> "(R_BRACK)"
  | L_BRACE _ -> "(L_BRACE)"
  | R_BRACE _ -> "(R_BRACE)"
  | MAPSTO _ -> "(MAPSTO)"
  | PERIOD _ -> "(PERIOD)"
  | MID _ -> "(MID)"
  | TMID _ -> "(TMID)"
  | COMMA _ -> "(COMMA)"
  | SEMI _ -> "(SEMI)"
  | COLON _ -> "(COLON)"
  | ASSIGN _ -> "(ASSIGN)"
  | ARROW _ -> "(ARROW)"
  | BLANK _ -> "(BLANK)"
  | ALT _ -> "(ALT)"
  | APPLYSUB _ -> "(APPLYSUB)"
  | SLASH _ -> "(SLASH)"
  | SLASHDASH _ -> "(SLASHDASH)"
  | COERCION _ -> "(COERCION)"
  | LAMBDA _ -> "(LAMBDA)"
  | PITY _ -> "(PITY)"
  | QUANTIFIER s -> s ^ " (QUANTIFIER)"
  | VAR s -> s ^ " (VAR)"
  | WORD s -> s ^ " (WORD)"
  | ATOMIC_IDENTIFIER s -> s ^ " (ATOMIC)"
  | HIERARCHICAL_IDENTIFIER s -> s ^ " (HIERARCHICAL)"
  | FIELD_ACCESSOR s -> s ^ " (FIELD)"
  | EOF -> "(EOF)"
  | UNKNOWN s -> s ^ " ?"

let rec lex_tokens acc buf = 
  let t = lex_token buf in 
  if (t = EOF) then List.rev (EOF :: acc) 
  else lex_tokens (t:: acc) buf

let lex_string s : token list = 
  let buf = Sedlexing.Latin1.from_string s in 
  lex_tokens [] buf;;

(*
let test_lex_string() = List.map print_endline (List.map lex_token_to_string (lex_string "A B C hello\\alpha33[1]there !ready! \\begin \\\\  Riemann-Hilbert %comment \n more #4 # 5  $ ))))))))"));;
 *)
              


              
(* testing stuff *)

