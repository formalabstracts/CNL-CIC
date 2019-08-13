
let to_string = function
  | NATURAL i -> (string_of_int i)
  | NUMERIC s -> s
  | CONTROLSEQ s -> s
  | CONTROLCHAR s -> s
  | ARG i -> "#"^(string_of_int i)
  | L_PAREN -> "("
  | R_PAREN -> ")"
  | L_BRACK -> "["
  | R_BRACK -> "]"
  | L_BRACE -> "{"
  | R_BRACE -> "}"
  | SUB -> "\\sb"
  | FORMAT_EOL -> "&"
  | FORMAT_COL -> "\\"
  | TOK s -> s
  | SYMBOL s -> s
  | _ -> "";;


