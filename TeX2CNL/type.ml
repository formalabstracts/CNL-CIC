(* module Type = struct *)

type token = 
   | Natural of int
   | Numeric of string
   | Eol
   | Par 
   | Comment
   | Input of string
   | Cnlenvdel of string
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
   | Error of string
   | Warn of string 
   | Eof
   | Ignore
   | NotImplemented of string;;

type partrack =
  | TrackPar
  | NoTrackPar;;


type io_channels = 
  {
    infile : string;
    outfile : string;
    mutable intoks : token list ;
    outc : out_channel;
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
