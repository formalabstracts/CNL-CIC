open Parser_tex
(*
let keyword = function
  | WITH -> "with"
  | VERT -> "|"
  | CONTEXT -> "context"
  | UNICODE_UNKNOWN -> "?"
  | THM -> "thm"
  | THEN -> "then"
  | STRUCTURE -> "structure"
  | STRING -> "string"
  | SORT -> "sort"
  | SEP -> "sep"
  | RPAREN -> "rparen"
  | RIGHT -> "right"
  | QED -> "qed"
(*  | POST -> "postulate" *)
  | PERIOD -> "."
  | PACKAGE -> "package"
  | OP -> "op"
  | NUM -> "num"
  | NOTATION -> "notation"
  | MATCH -> "match"
  | LPAREN -> "lparen"
  | LET -> "let"
  | LEFT -> "left"
  | INDUCTIVE -> "inductive"
  | IN -> "in"
  | IMPORT -> "import"
  | IF -> "if"
  | IDENT -> "ident"
  | HASHPRINT -> "hashprint"
  | HASHEXPR -> "hashexpr"
  | EOF -> "eof"
  | END -> "end"
  | ELSE -> "else"
  | DEF -> "def"
  | DECIMAL -> "decimal"
  | COMMA -> "comma"
  | COLON -> ":"
  | CHAR -> "char"
  | BLANK_NUM -> "_#"
  | BLANK -> "_"
  | BINDER -> "binder"
  | ASSIGN -> ":="
 *)

let to_string t =
  let mk_paren s = "("^s^")" in
  let s = match t with
    | EXTEND -> "extend"
      | HINT -> "hint"
  | WITH -> "with"
  | QED -> "qed"
  | META s -> "meta "^s
  | PERIOD -> "."
  | PACKAGE -> "package"
  | CONTEXT    -> "context"
  | BLANK -> "_"
  | ASSIGN -> ":="
  | EOF -> "eof"
  | END -> "end"
  | ELSE -> "else"
  | DEF -> "def"
  | COLON -> ":"
  | INDUCTIVE -> "inductive"
  | IN -> "in"
  | IMPORT -> "import"
  | IF -> "if"
  | THM -> "thm"
  | THEN -> "then"
  | STRUCTURE -> "structure"
  | NOTATION -> "notation"
  | MATCH -> "match"
  | LET -> "let"
  | DECIMAL s -> "decimal "^s
  | COMMA s -> "comma "^s
  | BINDER s -> "binder "^s
  | STRING s -> "string "^s
  | SORT s -> "sort "^s
  | SEP s -> "sep "^s
  | RPAREN s -> "rparen "^s
  | RIGHT s -> "right "^s
  | OP s -> "op "^s
  | UNICODE_UNKNOWN s -> "unknown_unicode "^s
  | VERT s -> "vert "^s
  | LPAREN s -> "lparen "^s
  | LBRACE s -> s
  | RBRACE s -> s
  | LEFT s -> "left "^s
  | IDENT s -> "ident "^s
  | IDENT_NOTE s -> "ident_note "^s
  | IDENT_PRINT s -> "ident_print "^s
  | HASHPRINT -> "#print"
  | HASHCHECK -> "#check"
  | HASHEVAL -> "#eval"
  | HASHCONFIG -> "#config"
  | CHAR s -> "char "^Char.escaped s
  | BLANK_NUM s -> "_# "^string_of_int s
  | NUM s ->  "num "^string_of_int s in
                       (*  | _ -> "not-found" in *)
  mk_paren s;;

