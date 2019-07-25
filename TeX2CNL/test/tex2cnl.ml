
type token = 
   | Natural of int
   | Numeric of string
   | Eol
   | ControlSeq of string
   | Arg of int
   | LParen
   | RParen
   | LBrack
   | RBrack
   | LBrace
   | RBrace
   | LDisplay
   | RDisplay
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
   | Eof
   | NotImplemented;;


let token_to_string = function
  | Natural i -> (string_of_int i)
  | Numeric s -> s
  | ControlSeq s -> s
  | Arg i -> "#"^(string_of_int i)
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Dollar -> "$"
  | Sub -> "\\sb"
  | Comma -> ","
  | Semi -> ";"
  | FormatEol -> "&"
  | FormatCol -> "\\"
  | Label s -> s
  | Tok s -> s
  | Symbol s -> s
  | Error s -> "[Error: "^s^"]"
  | Eof -> "EOF"
  | NotImplemented -> "NotImplemented"
  | _ -> "";;


let string_clean =
  let f c = match c with
    | 'a' .. 'z' -> c
    | 'A' .. 'Z' -> c
    | '0' .. '9' -> c
    | '\'' -> c
    | _ -> '_' in
  String.map f;;

let test = string_clean "a;sldfkj7698769as2!&*()*&)(&:";;

let token_to_clean_string = function 
  | Natural i -> (string_of_int i)
  | Numeric s -> string_clean s
  | ControlSeq s -> string_clean s
  | Tok s -> string_clean s
  | Symbol s -> string_clean s
  | Label s -> string_clean s
  | Sub -> "sb"
  | _ -> "_";;

let tokens_to_clean_string toks = 
  let cs = List.map token_to_clean_string toks in 
  String.concat "'" cs;;



let mk_label toks = 
  let t = "L"^(tokens_to_clean_string toks) in (* force initial letter *)
  Label t ;;

let no_expand = ControlSeq "noexpand";;

let paren(toks) = LParen :: toks @ [RParen];;

let brace(toks) = LBrace :: toks @ [RBrace];;

let bracket(toks) = LBrack :: toks @ [RBrack];;

let outstream = ref [];;

let pushout a =
 let _ = outstream := a :: (!outstream) in ();;

let error s = pushout (Error s);;


let stream = ref [];;

(* We preserve line numbers in transforming from input to output. The
   LF is placed in the output the first time encountered. Except I/O
   doesn't exactly preserve lines during macro expansions and other
   operations that return tokens to the input stream. 
 *)
let popstream = 
  let pop1() =
    let a = List.hd !stream in
    let _ = stream := List.tl !stream in
    a in
  let rec poprec acc k = 
    if (k=0) then List.rev acc
    else 
      match pop1() with 
      | Eol -> (pushout Eol; poprec acc k)
      | _ as t -> poprec (t::acc) (k-1) in
  fun k -> poprec [] k;;

let pop() = List.hd(popstream 1);;

let returnstream l = 
 stream := l @ !stream;;

let popfilter f s = 
  let t = pop() in
  let _ = f t || failwith(s) in 
  t;;

let poptok tok = popfilter ((=) tok) ("expected "^token_to_string tok);;

let peek k = 
  let ls = List.init k (fun _ -> pop()) in
  let _ = returnstream ls in
  ls;;
  

(* note that this delimiter matching often takes place before macro
   expansion.  We are therefore a strong invariance of delimiter matching;
   delimiters should match no matter how fully expanded the macros might be.
 *)

let rec match_delim read state left right acc brs = 
  let (a,state') = read state in 
  if (a = left) then match_delim read state' left right (left :: acc) (brs+1)
  else if (a = right) then 
    (if brs = 1 then 
       (List.rev acc,state') 
     else match_delim read state' left right (right :: acc) (brs-1))
  else match_delim read state' left right (a :: acc) brs;;

let popbraces_rec brs = fst(match_delim (fun () -> pop(), ()) () LBrace RBrace [] brs);;

let popparen_rec brs = fst(match_delim (fun () -> pop(), ()) ()  LParen RParen [] brs);;
 
let popbrace1 () = 
  let _ = poptok LBrace in 
  popbraces_rec 1;;

let rec popbrace acc k = 
  if (k=0) then List.rev acc else 
    let t = popbrace1() in 
    popbrace (t::acc) (k-1);;

let rec macroexpand acc pattern args = 
  match pattern with
  | [] -> List.rev acc
  | (Arg i) :: rest -> macroexpand (List.rev(List.nth args (i-1)) @ acc) rest args
  | t :: rest -> macroexpand (t :: acc) rest args;;

let readnum () = 
  let (a,b,c) = (pop(),pop(),pop()) in
  match(a,b,c) with
  | (LBrack,Arg i,RBrack) -> i
  | _ -> (returnstream [a;b;c]; 0);;

let macrotable = ref [];;
let setmacro f = (macrotable := f :: !macrotable);;

let is_controlseq t = 
  match t with 
  | ControlSeq _ -> true
  | _ -> false;;

let popcs () = popfilter is_controlseq "CnlDelete expects cs" ;;

let process_environment toks = toks;; (* debug *)

(* convert tuple to list at outermost layer, with respect to () {}:
   (a,b,(c,d),e,\frac{f}{g,h}) --> [a;b;(c,d);e;\frac{f}{g,h}]
 *)

let popparen_ls ls brs = match_delim (function | [] -> (Error "missing closing paren",[]) | t::rest -> (t,rest)) ls LParen RParen [] brs;;

let popbraces_ls ls brs = match_delim (function | [] -> (Error "missing closing brace",[]) | t::rest -> (t,rest)) ls LBrace RBrace [] brs;;

let rec list_of_tuple_rec sep acc toks = (* outer brackets not included *)
  match toks with
  | [] -> paren(List.rev acc)
  | a :: rest ->
      match a with
      | LBrace -> 
          let (m,rest') = popbraces_ls rest 1 in 
          let ls = brace(m) in 
          list_of_tuple_rec sep ((List.rev ls) @ acc) rest'
      | LParen -> 
          let (m,rest') = popparen_ls rest 1 in 
          let ls = paren(m) in
          list_of_tuple_rec sep ((List.rev ls) @ acc) rest' 
      | RParen -> error "unmatched ) in list_of_tuple_rec"; paren(List.rev acc )
(*          let _ = ((rest = []) || (error "unused tokens after tuple in \\list"; true)) in
                     paren(List.rev acc) *)
      | Comma -> list_of_tuple_rec sep (LParen :: sep(RParen :: acc)) rest
      | _ -> list_of_tuple_rec sep (a :: acc) rest;;


let list_of_uple toks =
  bracket(list_of_tuple_rec (fun t -> Semi :: t) [] toks);;

let curryargs toks = 
  list_of_tuple_rec (fun t -> t) [] toks ;;

let list_of_tuple = 0;;
let test = list_of_uple [LParen;RParen];;
let test = list_of_uple [ControlSeq "A";Comma;ControlSeq "B";Comma;ControlSeq "C"];;
let test = curryargs [ControlSeq "A";Comma;ControlSeq "B";Comma;ControlSeq "C"];;


let process_controlseq cs_string =
  match cs_string with
  | "\\CnlDelete" -> 
      let i = readnum() in
      let cs = popcs() in setmacro (cs,(i,[])); []
  | "\\noexpand" -> 
      let cs = popcs() in [cs]
  | "\\CnlNoExpand" -> 
      let i = readnum() in
      let cs = popcs() in setmacro (cs,(i,[no_expand;cs])); []
  | "\\CnlCustom" ->
      let i = readnum() in
      let cs = popcs() in
      let toks = popbrace1() in setmacro (cs,(i,toks)); []
  | "\\begin" ->
      let toks = popbrace1() in process_environment(toks)
  | "\\list" ->
      let toks = popbrace1() in (returnstream (list_of_uple toks) ; [])
  | "\\app" ->
      let ftoks = popbrace1() in 
      let argtoks = popbrace1() in 
      let cargs = curryargs argtoks in
      let ls = paren(paren(ftoks) @ cargs) in 
       (returnstream (ls) ; [])
  | _ -> [];;


(* Environments *) 


let delete_option() = (* material in [] *)
  let l = peek 1 in
  if (List.hd l = LBrack) then 
    ignore(match_delim (fun () -> pop(), ()) () LBrack RBrack [] 0)
  else ();;

let get_label() = (* read \label *)
 let l = peek 1 in
 match List.hd l with
 | ControlSeq "label" -> 
     let _ = pop() in 
     let toks = popbrace1() in Some(mk_label toks)
 | _ -> None;;

let write_declaration_preamble s label = 
  let _ = pushout (Tok s) in
  let _ = match label with | None -> () | Some t -> pushout t in
  pushout (Symbol ".");;

let transcribe_until next_is_token = ();; (* debug, implement *)

let next_is cs = ();; (* debug, implement *)

let write_declaration_ending () = ();;

let do_definition () = (* \begin{definition} has been read *)
 let _ = delete_option() in
 let label = get_label() in 
 let _ = write_declaration_preamble "Definition" label in 
 let _ = transcribe_until (next_is (ControlSeq "end")) in
 write_declaration_ending ();;



(* fin *)
