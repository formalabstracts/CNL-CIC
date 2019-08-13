(* module Type = struct *)

type token = 
   | Natural of int
   | Numeric of string
   | Eol
   | Par 
   | Comment
   | Input of string
   | ControlSeq of string
   | BeginSeq of string
   | EndSeq of string
   | BeginCnl
   | EndCnl
   | Arg of int
   | LParen
   | RParen
   | LBrack
   | RBrack
   | LBrace
   | RBrace
   | LDisplay
   | RDisplay
   | Display
   | Dollar
   | Sub
   | Comma
   | Semi
   | FormatEol 
   | FormatCol 
   | Label of string
   | Tok of string
   | Symbol of string
   | Error of string
   | Warn of string 
   | Eof
   | Ignore
   | NotImplemented;;


type io_channels = 
  {
    infile : string;
    outfile : string;
    mutable intoks : token list ;
    outc : out_channel;
  };;

type environ_item =
{
  name : string;
  begin_token : token;
  tr_token : (token*token) list;
  end_token : token;
  drop_toks : token list;
  is_delete : bool;
  regard_par : bool;
};;

(* end;; *)
