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

(* symbol does not include '\' *)
          
let symbol = [%sedlex.regexp? '*' | '+' | '^' | '=' | '<' | '>' | '/' | '!' | 
              '@' | '#' | '$' | '%' | '&' | '_' | '-' | '|' | ':' | "'" | '.' |
              '?' | '~' | '`'] 

let symbolseq = [%sedlex.regexp? Plus(symbol)]

let separator = [%sedlex.regexp? '.' | ',' | ':' | ';' | '|' ]

let controlchar = [%sedlex.regexp? '\\', (symbol | separator ) ]

(* tilde starts the suffix, in which more general character sequences are allowed: *)

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

(* handled by symbolkey:
let period = [%sedlex.regexp? '.']           
let colon = [%sedlex.regexp? ':']
let assign = [%sedlex.regexp? ":="]
let arrow = [%sedlex.regexp? "->"]
let mapsto = [%sedlex.regexp? "|->"]
let alt = [%sedlex.regexp? "|"]
let slash = [%sedlex.regexp? "/"]
let slashdash = [%sedlex.regexp? "/-"]     
let blank = [%sedlex.regexp? '_']
 *)

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


(* the rule for var and word is that they cannot be directly followed by an alphanum,
   but we don't have lookahead.  *)

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

(* 
  The order matters here.  The first match is applied.

  Note the folowing important particular orderings:
  * (+/-) DECIMAL - INTEGER - symbolseq 
  * (periods) field_accessor - symbolseq (other periods are internal to a lexeme)
  * varlong - word - atomic_identifier (after parsing as atomic)

  Some words should actually be treated as 
  case sensitive atomic_identifiers (such as sin, cos, tan).
 *)

let rec lex_token buf =
  match%sedlex buf with
  | Plus(white) -> (lex_token buf)
  | comment -> (lex_token buf)
    | string -> STRING (lexeme buf)
    | controlseq -> controlkey(lexeme buf)
    | controlchar -> controlkey(lexeme buf)
    | decimal -> DECIMAL(lexeme buf)
    | integer -> INTEGER(lexeme buf)
    | number -> INTEGER(lexeme buf)
    | rparen -> R_PAREN (lexeme buf)
    | lparen -> L_PAREN (lexeme buf)
    | lbrack -> L_BRACK (lexeme buf)
    | rbrack -> R_BRACK (lexeme buf)
    | lbrace -> L_BRACE (lexeme buf)
    | rbrace -> R_BRACE (lexeme buf)
    | comma -> COMMA (lexeme buf)
    | semi -> SEMI (lexeme buf)
    | field_accessor -> FIELD_ACCESSOR (lexeme buf)
    | hierarchical_identifier -> HIERARCHICAL_IDENTIFIER (lexeme buf)
    | symbolseq -> symbolkey (lexeme buf)
    | varlong -> VAR (lexeme buf) 
(*    | var -> VAR (lexeme buf)
    | word -> WORD (lexeme buf) *)
    | atomic_identifier -> identkey s 
    | eof -> EOF
    | any -> failwith (lexeme buf)
    | _  -> failwith (lexeme buf)

let lex_token_to_string = function
  | STRING s -> "STRING["^s^"]"
  | CONTROLSEQ s -> "CS["^s^"]"
  | DECIMAL s -> s
  | INTEGER s -> s
  | SYMBOL s -> "S["^s^"]"
  | SYMBOL_QED _ -> "[QED]"
  | L_PAREN _ -> "("
  | R_PAREN _ -> ")"
  | L_BRACK _ -> "["
  | R_BRACK _ -> "]"
  | L_BRACE _ -> "{"
  | R_BRACE _ -> "}"
  | MAPSTO _ -> "->"
  | PERIOD _ -> "."
  | MID _ -> "[MID]"
  | TMID _ -> "[TMID]"
  | COMMA _ -> ","
  | SEMI _ -> ";"
  | COLON _ -> ":"
  | ASSIGN _ -> ":="
  | ARROW _ -> "->"
  | BLANK _ -> "_"
  | ALT _ -> "|"
  | APPLYSUB _ -> "[_]"
  | SLASH _ -> "/"
  | SLASHDASH _ -> "/-"
  | COERCION _ -> "\\^"
  | LAMBDA _ -> "[LAMBDA]"
  | PITY _ -> "[PI]"
  | QUANTIFIER s -> "Q["^s^"]"
  | VAR s -> "V["^s^"]"
  | WORD s -> "W["^s^"]"
  | ATOMIC_IDENTIFIER -> "A["^s^"]"
  | HIERARCHICAL_IDENTIFIER -> "H["^s^"]"
  | FIELD_ACCESSOR -> "F["^s^"]"
  | _ -> "?"

let rec lex_tokens acc buf = 
  let t = lex_token buf in 
  if (t = Eof) then List.rev (Eol :: acc) 
  else lex_tokens (t:: acc) buf

let lex_string s : token list = 
  let buf = Sedlexing.Latin1.from_string s in 
  lex_tokens [] buf;;

let test_lex_string() = List.map print_endline (List.map lex_token_to_string (lex_string "A B C hello\\alpha33[1]there !ready! \\begin \\\\  Riemann-Hilbert %comment \n more #4 # 5  $ ))))))))"));;
              


              
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
