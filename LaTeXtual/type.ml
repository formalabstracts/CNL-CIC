(* module Type = struct *)

type token = 
   | Natural of int
   | Numeric of string
   | Eol
   | Par 
   | Comment of string
   | Input of string
   | ControlSeq of string
   | BeginEnv of string
   | EndEnv of string
   | LBrack
   | RBrack
   | LBrace
   | RBrace
   | LDisplay
   | RDisplay
   | Display
   | Dollar
   | Period
   | Symbol
   | Tok of string
   | XmlCs of string*string (* name contents *)
   | XmlEnv of string*string
   | XmlDisplayMath
   | XmlMath
   | XmlError of string
   | XmlWarn of string 
   | XmlMessage of string 
   | Eof
   | EndDocument
   | Ignore
   | NotImplemented of string;;

let lex_token_to_string = function
  | Natural i -> (string_of_int i)
  | Numeric s -> s
  | Input s -> "\\input{"^s^"}"
  | ControlSeq s -> s
  | BeginEnv s -> "\\begin{"^s^"}"
  | EndEnv s -> "\\end{"^s^"}"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Display -> "$$"
  | Dollar -> "$"
  | Period -> "."
  | Symbol -> "-"
  | Tok s -> s
  | Eof -> "EOF"
  | Eol -> "EOL"
  | EndDocument -> "EDoc"
  | NotImplemented _ -> "NotImplemented"
  | _ -> "";;


let token_to_verbose_string = function
  | Natural i -> "Nat"^(string_of_int i)
  | Numeric s -> "Num"^s
  | Eol -> "Eol"
  | Par -> "Par"
  | Comment s -> "%Comment-"^s
  | Input s -> "Input-"^s
  | ControlSeq s -> "ControlSeq-"^s
  | BeginEnv s -> "BeginEnv-"^s
  | EndEnv s -> "EndEnv-"^s
  | LBrack -> "LBrack"
  | RBrack -> "RBrack"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | LDisplay -> "LDisplay"
  | RDisplay -> "RDisplay"
  | Dollar -> "Dollar"
  | Display -> "Display"
  | Period -> "Period"
  | Symbol -> "Symbol"
  | Tok s -> "Tok-"^s
  | Eof -> "Eof"
  | EndDocument -> "EndDocument"
  | Ignore -> "Ignore"
  | NotImplemented s -> "NotImplemented-"^s
  | _ -> "";;

type partrack =
  | TrackPar
  | NoTrackPar;;


type io_channels = 
  {
    infile : string;
    mutable intoks : token list ;
  };;

(*
  name of environment from \begin{name}
  begin_token insert these into output at beginning
  tr_token perform this token translation
  end_token insert these into output at ending
  drop_toks delete these from output 
  is_delete produce no output 
  stay_in_par environment must remain in paragraph 
 *)

type environ_item =
{
  name : string;
  begin_token : token;
  tr_token : (token*token) list;
  end_token : token;
  drop_toks : token list;
  is_delete : bool;
  stay_in_par : partrack;
};;

(* end;; *)
