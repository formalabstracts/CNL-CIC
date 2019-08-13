(** A custom lexbuffer that automatically keeps track of the source location.
    This module is a thin wrapper arounds sedlexing's default buffer, which does
    not provide this functionality. 

    Adapted from https://github.com/smolkaj/ocaml-parsing/blob/master/src/LexBuffer.ml

    Compare https://github.com/ocaml-community/sedlex/blob/v1.99.4/src/lib/sedlexing.mli
 *)


(** the lex buffer type *)
type t = {
  buf : Sedlexing.lexbuf;
  mutable pos : Lexing.position;
  mutable pos_mark : Lexing.position;
  mutable last_char : int option;
  mutable last_char_mark : int option;
  }

let lexeme b = Sedlexing.Utf8.lexeme b.buf

let pos b = b.pos             

let of_sedlex ?(file="<n/a>") ?pos buf =
  let pos = Extlib.Option.default (Lexing.{
    pos_fname = file;
    pos_lnum = 1;
    pos_bol = 0; 
    pos_cnum = 0; }) pos
  in
  {  buf; pos; pos_mark = pos; last_char = None; last_char_mark = None; }

let of_utf8_string ?pos s =
  of_sedlex ?pos Sedlexing.(Utf8.from_string s) 

let of_utf8_file file =
  let chan = open_in file in
  of_sedlex ~file Sedlexing.(Utf8.from_channel chan)

(** The next four functions are used by sedlex internally.
    See https://www.lexifi.com/sedlex/libdoc/Sedlexing.html.  *)
let mark lexbuf p =
  lexbuf.pos_mark <- lexbuf.pos;
  lexbuf.last_char_mark <- lexbuf.last_char;
  Sedlexing.mark lexbuf.buf p

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.pos_mark;
  lexbuf.last_char <- lexbuf.last_char_mark;
  Sedlexing.backtrack lexbuf.buf

let start lexbuf =
  lexbuf.pos_mark <- lexbuf.pos;
  lexbuf.last_char_mark <- lexbuf.last_char;
  Sedlexing.start lexbuf.buf

(** location of next character *)
let next_loc lexbuf =
  { lexbuf.pos with pos_cnum = lexbuf.pos.pos_cnum + 1 }

let cr = 13 (* Char.code '\r' *)
let lf = 10 (* Char.code '\n' *)

(** next character *)
let next lexbuf =
  let c = Sedlexing.next lexbuf.buf in
  let pos = next_loc lexbuf in
  begin match c with
  | 13
  | 10 when not (lexbuf.last_char = Some cr) ->
                lexbuf.pos <- { pos with 
                                pos_bol = pos.pos_cnum - 1;
                                pos_lnum = pos.pos_lnum + 1; }
  | 10 -> ()
  | _ -> lexbuf.pos <- pos
  end;
  lexbuf.last_char <- Some c;
  c

let raw lexbuf : int array =
  Sedlexing.lexeme lexbuf.buf

let ascii ?(skip=0) ?(drop=0) lexbuf : string =
  let len = Sedlexing.(lexeme_length lexbuf.buf - skip - drop) in
  Sedlexing.(Utf8.sub_lexeme lexbuf.buf skip len)

