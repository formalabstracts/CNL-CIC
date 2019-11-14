open Type;;
             (* sedlex format *)

(* start interactive section *)
(* #require "Batteries"
#require "sedlex"


open BatSet.String
open BatList 
 *)

(* moved to type.ml: 
 *)

let lex_token_to_string = function
  | Natural i -> (string_of_int i)
  | Numeric s -> s
  | Input s -> "\\input{"^s^"}"
  | ControlSeq s -> s
  | BeginSeq s -> "\\begin{"^s^"}"
  | EndSeq s -> "\\end{"^s^"}"
  | BeginCnl -> "\\begin{cnl}"
  | EndCnl -> "\\end{cnl}"
  | Arg i -> "#"^(string_of_int i)
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Display -> "$$"
  | Dollar -> "$"
  | Sub -> "\\sb"
  | Comma -> ","
  | Semi -> ";"
  | FormatEol -> "\\"
  | FormatCol -> "&"
  | Tok s -> s
  | Eof -> "EOF"
  | Eol -> "EOL"
  | NotImplemented _ -> "NotImplemented"
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

let alphanum = [%sedlex.regexp? alphabet | numeral10 | '_' | '\'' | "-" | "~" ] (* - for hyphen, ~ for connected space *)
             
let controlseq = [%sedlex.regexp? '\\', Plus(alphabet)]

let controlchar = [%sedlex.regexp? '\\', Compl(alphabet | eol)]

let comment = [%sedlex.regexp? '%', Star(Compl(eol))]

let arg = [%sedlex.regexp? '#', numeral10 ]

let lparen = [%sedlex.regexp? '(' ]  
let rparen = [%sedlex.regexp? ')' ]
let lbrack = [%sedlex.regexp? '[' ]  
let rbrack = [%sedlex.regexp? ']' ]
let lbrace = [%sedlex.regexp? '{' ]
let rbrace = [%sedlex.regexp? '}' ]
let ldisplay = [%sedlex.regexp? '\\', '['  ]
let rdisplay = [%sedlex.regexp? '\\', ']'  ]
let format_eol = [%sedlex.regexp? "\\\\", Opt('*') ]
let format_col = [%sedlex.regexp? '&']

let begincnl = [%sedlex.regexp? "\\begin", Star(white), "{", Star(white), 
                "cnl" ,  Star(white), "}"] 

let endcnl = [%sedlex.regexp? "\\end", Star(white), "{", Star(white), 
                "cnl" ,  Star(white), "}"] 

let beginseq = [%sedlex.regexp? "\\begin", Star(white), "{", Star(white), 
                Plus(alphabet), Opt("*"), Star(white), "}"] 

let endseq = [%sedlex.regexp? "\\end", Star(white), "{", Star(white), 
                Plus(alphabet), Opt("*"), Star(white), "}"] 

let inputseq = [%sedlex.regexp? "\\input", Star(white), "{", Star(white), 
                Plus(alphanum), Star(white), "}"] 

let alphafile = [%sedlex.regexp? alphabet | numeral10 | '_' | '-' | '.' | '/' ]

let cnlinputseq = [%sedlex.regexp? "\\Cnlinput", Star(white), "{", Star(white), 
                Plus(alphafile), Star(white), "}"] 

let cnlenvdel = [%sedlex.regexp? "\\CnlEnvirDelete", 
                 Star(white), "{", Star(white), 
                Plus(alphabet) ,  Opt("*"), Star(white), "}"] 


let period = [%sedlex.regexp? '.']           
let comma = [%sedlex.regexp? ',']
let colon = [%sedlex.regexp? ':']
let semi = [%sedlex.regexp? ';']           
let punct = [%sedlex.regexp? period | colon ]

let dollar = [%sedlex.regexp? '$']
let doubledollar = [%sedlex.regexp? dollar, dollar ]
let sub = [%sedlex.regexp? '_']

let symbol = [%sedlex.regexp? punct | '|' | '<' | '>' | '^' | '+' | '-' | '=' | '/' | '*']

(* handle foreign accents in words *)
let accent_char = [%sedlex.regexp?  '`' | '^' | '"' | '=' | '.' (* dash '\'' | '~' | *) ]

let accent_letter = [%sedlex.regexp? 'c' | 'v' | 'u' | 'H']

let accent_chars = [%sedlex.regexp? "oe" | "ae" | "ss" | "aa" | "AA" | "o" | "O" | "AE" | "OE" | "l" | "L" ]

let accent_cluster = [%sedlex.regexp? "\\", accent_char, alphabet 
 | "\\", accent_letter, '{', alphabet, '}'
 | '{', "\\", accent_chars, '}' 
]

let dash_char = [%sedlex.regexp? '-' | '~' | '\'' ]

let dash_cluster = [%sedlex.regexp? '\\', dash_char]

let word = [%sedlex.regexp? Plus(alphabet | accent_cluster | dash_cluster) ]

let wordbrace = [%sedlex.regexp? "{~", 
                 Plus(alphabet | accent_cluster | dash_cluster | white), '}'  ]


let dot_id = [%sedlex.regexp? '.', alphabet, Star(alphanum) ]
let unmarked_id = 
  [%sedlex.regexp? Opt('.'), alphabet, Star(alphanum), Star(dot_id)]
let id = [%sedlex.regexp? '!', unmarked_id, '!' ]

(*
let unmarked_id_more = [%sedlex.regexp? alphanum | '.' ]
let unmarked_id = [%sedlex.regexp? alphabet, Star(unmarked_id_more), Plus(alphanum) ]
 *)


           
(* open Parser_tex *)

let lexeme = Sedlexing.lexeme;;

let implode l = String.init (List.length l) (List.nth l)

let string_of_ints js =
 let cs =  List.map (fun j ->  (Uchar.to_char j)) (Array.to_list js) in
  implode cs;;

let string_lexeme buf = string_of_ints(lexeme buf);;

let trim_ends s = 
  if String.length s < 2 
  then s
  else String.sub s 1 (String.length s - 2);;

let drop s k = 
  if k > String.length s then ""
  else String.sub s k (String.length s - k);;


let convert_hyphen = String.map (function | '-' -> '_' | '~' -> '_' | c -> c);;

let strip_nonalpha s = 
 let s' = String.map (function | 'a'..'z' as c -> c | 'A'..'Z' as c -> c | _ -> ',') s in
 let ls = String.split_on_char ',' s' in
  String.concat "" ls;;

let _ = strip_nonalpha "abcd 234 efg\\={h}";;
let _ = strip_nonalpha "\\\'etale";;
let _ = strip_nonalpha "Erd\\H{o}s";;
let _ = strip_nonalpha "Poincar\\\'e";;

let strip_to_brace s = 
  let n1 = String.index s '{' in
  let n2 = String.index s '}' in
  let _ = ((n1 < n2) || raise Not_found) in
  String.trim (String.sub s (n1+1) (n2-(n1+1)));;

let test() = strip_to_brace "begin { hello } ";;

let rec lex_token buf = 
 match%sedlex buf with 
 | Plus(white) -> (lex_token buf)
 | comment -> Comment (string_lexeme buf)
  | natural_number -> Natural(int_of_string(string_lexeme buf)) 
    | numeric -> Numeric(string_lexeme buf)
    | eol -> Eol
    | begincnl -> BeginCnl
    | endcnl -> EndCnl
    | ldisplay -> LDisplay
    | rdisplay -> RDisplay
    | dollar -> Dollar
    | beginseq -> BeginSeq(strip_to_brace(string_lexeme buf))
    | endseq -> EndSeq(strip_to_brace(string_lexeme buf))
    | cnlenvdel -> Cnlenvdel(strip_to_brace(string_lexeme buf))
    | inputseq -> Input(strip_to_brace(string_lexeme buf))
    | cnlinputseq -> Input(strip_to_brace(string_lexeme buf))
    | format_eol -> FormatEol
    | format_col -> FormatCol
    | controlseq -> ControlSeq(drop (string_lexeme buf) 1) (* was 2 *)
    | controlchar -> ControlSeq(drop (string_lexeme buf) 1) (* was 2 *)
    | arg -> Arg(int_of_string(drop (string_lexeme buf) 1))
    | rparen -> RParen
    | lparen -> LParen
    | lbrack -> LBrack
    | rbrack -> RBrack
    | lbrace -> LBrace
    | rbrace -> RBrace
    | id -> Tok(trim_ends(convert_hyphen(string_lexeme buf)))
    | word | wordbrace -> Tok(strip_nonalpha(string_lexeme buf))
    | unmarked_id -> Tok(convert_hyphen(string_lexeme buf))
    | symbol -> Tok(string_lexeme buf)
    | comma -> Comma 
    | semi -> Semi
    | eof -> Eof
    | any -> NotImplemented (string_lexeme buf)
    | _ -> failwith (string_lexeme buf)

let rec lex_tokens acc buf = 
  let t = lex_token buf in 
  if (t = Eof) then List.rev (Eol :: acc) 
  else lex_tokens (t:: acc) buf

let lex_string s : token list = 
  let buf = Sedlexing.Latin1.from_string s in 
  lex_tokens [] buf;;

let test_lex_string() = List.map print_endline (List.map lex_token_to_string (lex_string "A B C hello\\alpha33[1]there !ready! \\begin{ cnl } Riemann-Hilbert Riemann\\-Hilbert Poincar\\\'e {\\ae} { \\ae} (xx) \\input{file} \\mathfrak{C}33 [$] {yy} %comment \n more #4 # 5  $ )))))))) {~less than or equal to} is \\df{well\\-founded} xx"));;
              

