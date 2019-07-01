(*
   Boilerplate for using sedlex with Menhir, based on
   https://github.com/Drup/llvm/blob/3c43000f4e86af5b9b368f50721604957d403750/test/Bindings/OCaml/kaleidoscope/src/syntax.ml
https://github.com/unhammer/ocaml_cg_streamparse/blob/master/sedlex_menhir.ml  

Positions are described here: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html
https://github.com/ocaml-community/sedlex/blob/master/src/lib/sedlexing.mli

The latest github sedlex has val lexing_positions : lexbuf -> Lexing.position*Lexing.position,
but I haven't been able to make it install in a usable way, with
opam pin add sedlex https://github.com/ocaml-community/sedlex.git
But eventually, with a sedlex upgrade, this file can be deleted.

See https://github.com/smolkaj/ocaml-parsing/blob/master/src/Lexer.cppo.ml
for a better implementation.
 
*)

(*
(** The state of the parser, a stream and a position. *)
type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
       pos_lnum = lcp.pos_lnum + 1;
       pos_bol = lcp.pos_cnum;
    }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

(** The last matched word. *)
let lexeme { stream; _ } = Sedlexing.Utf8.lexeme stream


(** [ParseError (file, line, col, token)] *)

 *)

exception ParseError of (string * int * int * string * string)

let string_of_ParseError (file, line, cnum, tok, msg) =
  Printf.sprintf
    "Parse error: file: \"%s\"\nline %d, column %d, token \"%s\", \nmessage \"%s\"\n"
    file line cnum tok msg

let raise_ParseError msg lexbuf =
  let pos = Lexbuffer.pos lexbuf in
  let tok = Lexbuffer.lexeme lexbuf in
  let open Lexing in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise @@ ParseError (pos.pos_fname, line, col, tok, msg)

let convert lexer' parser' lexbuf =
  let lexer () =
    let ante_position = Lexbuffer.pos lexbuf in
    let token = lexer' lexbuf in
    let post_position = Lexbuffer.pos lexbuf
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised parser'
  in
  try
    parser lexer
  with
    | Parser_cnl.Error -> raise_ParseError "parser-error" lexbuf
    | Sedlexing.MalFormed -> raise_ParseError "Sedlexing.malformed" lexbuf
    | Sedlexing.InvalidCodepoint _ -> raise_ParseError "Sedlexing.InvalidCodepoint" lexbuf
    | Failure msg -> raise_ParseError msg lexbuf;;
