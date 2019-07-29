(* issues *)

 (*
   - \input implementation 
   - make a record for I/O, when a new file is opened push the stack. 
     filename, tokenized-input, input-buffer, output-buffer, 
     file handles. flush command, open/close commands. 

   - process_document 
   - process_environment
   - \begin{cnl} \end{cnl}, seek_cnl (no matching)
  *)

(* types *)
type token = 
   | Natural of int
   | Numeric of string
   | Eol
   | Par 
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
   | Eof
   | Ignore
   | NotImplemented;;

(* string operations *)
let token_to_string = function
  | Natural i -> (string_of_int i)
  | Numeric s -> s
  | Eol -> "\n"
  | Par -> ""
  | ControlSeq s -> "\\"^s
  | Arg i -> "#"^(string_of_int i)
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Dollar -> "$"
  | Display -> "$$"
  | Sub -> "\\sb"
  | Comma -> ","
  | Semi -> ";"
  | FormatEol -> "&"
  | FormatCol -> "\\"
  | Label s -> s
  | Tok s -> s
  | Symbol s -> s
  | Error s -> "[TeX2CnlError: "^s^"]"
  | Eof -> "EOF"
  | Ignore -> ""
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

let token_to_clean_string default = function 
  | Natural i -> (string_of_int i)
  | Numeric s -> string_clean s
  | ControlSeq s -> string_clean s
  | Tok s -> string_clean s
  | Symbol s -> string_clean s
  | Label s -> string_clean s
  | Sub -> "_"
  | _ -> default;;

let tokens_to_clean_string toks = 
  let cs = List.map (token_to_clean_string "_") toks in 
  String.concat "'" cs;;

let mk_label toks = 
  let t = "L'"^(tokens_to_clean_string toks) in (* force initial letter *)
  Label t ;;

let tokens_to_clean_string_short toks = 
  let cs = List.map (token_to_clean_string "") toks in 
  String.concat "" cs;;

let mk_var toks = 
  Tok ("V__"^(tokens_to_clean_string_short toks));;

let mk_id toks = 
  Tok ("id_"^(tokens_to_clean_string_short toks));;

let test = mk_var [ControlSeq "alpha";Sub;Natural 3];;
let test = mk_var [ControlSeq "alpha";Sub;LBrace;Natural 33;RBrace];;

let no_expand = ControlSeq "noexpand";;

let id_fun x = x;;
(* I/O output *)

let outstream = ref [];;

let write_output a =
 let _ = outstream := a :: (!outstream) in print_string (token_to_string a);;

let write_output_list ls = 
let _ = outstream := ls @ !outstream in 
ignore(List.map print_string (List.map token_to_string ls));;

let write_error_message s = (* this takes the form of a CNL string instruction *)
  write_output_list [LBrack;Tok "error";Symbol "\"";Tok s;Symbol "\"";RBrack];;

let (write_error,get_error_count) =
  let error_limit = 20 in
  let error_count = ref 0 in 
  ((fun s ->
        let _ = write_error_message s in
        let _ = error_count := 1 + !error_count in
        if !error_count > error_limit then 
          write_error_message "error_limit exceeded"
                  (* flush and close outfile, NOT_IMPLEMENTED, debug *)
        else ()),
  (fun () -> !error_count));;

(* I/O source stream pop, peek, and return *)

let stream = ref [];;

 (* We preserve line numbers in transforming from input to output. The
   LF is placed in the output the first time encountered. Except I/O
   doesn't exactly preserve lines during macro expansions and other
   operations that return tokens to the input stream. 
 *)

let popstream = 
  let pop1 show_par =
    if !stream = [] then Eof else 
      let a = List.hd !stream in
      let _ = stream := List.tl !stream in
      let a' = if show_par && (a = Eol) && (!stream=[]) && (Eol = List.hd !stream)
               then Par 
               else a in
      a' in
  let rec poprec acc k show_par = 
    if (k=0) then List.rev acc
    else 
      match pop1 show_par with 
      | Eol -> (write_output Eol; poprec acc k show_par)
      | Par -> (write_output Eol; poprec (Eol::acc) (k-1) show_par)
      | _ as t -> poprec (t::acc) (k-1) show_par in
  fun show_par k -> poprec [] k show_par;;

let pop b = List.hd(popstream b 1);;

let returnstream ls = 
  let ls' = List.filter (fun t -> not(t = Par)) ls in
 stream := ls' @ !stream;;

let popfilter b f s = 
  let t = pop b in
  let _ = f t || (write_error s ; true) in 
  t;;

let poptok b tok = popfilter b ((=) tok) ("expected "^token_to_string tok);;

let peek b = 
  let a = pop b in
  let _ = returnstream [a] in
  a;;
  
let pop_brack_num b = 
  let (x,y,z) = (pop b,pop b,pop b) in
  match(x,y,z) with
  | (LBrack,Arg i,RBrack) -> i
  | _ -> (returnstream [x;y;z]; 0);;

let is_controlseq t = 
  match t with 
  | ControlSeq _ -> true
  | _ -> false;;

let get_csname t = 
  match t with
  | ControlSeq s -> s
  | _ -> "";; (* softfailure *)

let popcs b = popfilter b is_controlseq "control sequence  expected" ;;


(* delimiters {} [] ()  *)

 (* wrappers *)

let paren(toks) = LParen :: toks @ [RParen];;

let brace(toks) = LBrace :: toks @ [RBrace];;

let bracket(toks) = LBrack :: toks @ [RBrack];;

 (*
   Delimiter matching often takes place before macro
   expansion.  We are therefore a strong invariance of delimiter matching;
   delimiters should match no matter how fully expanded the macros might be.
  *)

let left_mate = 
  function 
  | RParen -> LParen
  | RBrace -> LBrace
  | RBrack -> LBrack
  | ControlSeq ")" -> ControlSeq "("
  | ControlSeq "]" -> ControlSeq "["
  | ControlSeq "end" -> ControlSeq "begin" 
  | _ -> NotImplemented;;

let check_mate (ldlims,tok) = 
  match ldlims with 
  | t :: ldlims' -> (ldlims',(if t = left_mate tok then [] else [Error "mismatched end group"]))
  | _ -> ldlims,[Error "unexpected end group"];;

let record_level (ldlims,a) = (* ldlims = left delimiter stack  *)
  match a with
  | ControlSeq "(" | ControlSeq "[" | ControlSeq "begin" | LParen | LBrace | LBrack -> 
     (a :: ldlims,[a])
  | ControlSeq ")" | ControlSeq "]" | ControlSeq "end" | RParen | RBrace | RBrack -> 
     let (ldlims',err) = check_mate (ldlims,a) in (ldlims', (a :: err))
  | Dollar -> if (match ldlims with | Dollar :: _ -> true | _ -> false) then 
                (List.tl ldlims,[])
              else (Dollar :: ldlims,[])
  | Display -> if (match ldlims with | Display :: _ -> true | _ -> false) then 
                (List.tl ldlims,[])
              else (Display :: ldlims,[])
  | _ -> (ldlims,[a]);;

 (*    We read the tokens using an arbitrary read function with state. *)

 (*
let rec leveled_pop_until acc ldlims endif =
  let a = pop() in
  if (endif (ldlims,a)) then (returnstream [a]; List.rev acc)
  else 
    let (ldlims',a') = record_level (ldlims,a) in 
    leveled_pop_until (a' @ acc) ldlims' endif;;
  *)

 (* tr is an arbitrary token transformation *)
let rec leveled_read_until acc read state ldlims tr endif = 
  try (
    let (a,state') = read state in
    if endif (ldlims,a) then ([a],List.rev acc,state)
    else 
      let (ldlims',a') = record_level (ldlims,a) in 
      leveled_read_until 
        ((List.map (fun t -> tr(ldlims',t)) a') @ acc) read state' ldlims' tr endif)
  with _ -> ([],List.rev acc,state);;

 (* 
   The left,right are the delimiters to be matched.

   The ldlims is the nesting of left delimiters.
   The code is organized in such a way that the first left is often already
   encountered when mate_delim is initialized with say ldlims=[LBrace].
 *)


let rec mate_delim read state right ldlims = 
  let left = left_mate right in
  let (current,toks,state') = leveled_read_until [] read state ldlims snd 
   (fun (ldlims,a) -> (ldlims = [left] && (a = right))) in 
  (toks,state');;

let mate_from_list = 
  let f ls = (List.hd ls,List.tl ls) in 
  fun right ls -> let (toks,unused) = mate_delim f ls right [left_mate right] in 
            (toks,unused);;

(* pop_matched_brace pop balanced expression, output doesn't include delimiters.  *)
let pop_to_right b right = 
  let _ = poptok b (left_mate right) in 
  fst(mate_delim (fun () -> pop b, ()) () right [left_mate right]);;

let pop_to_right_wo_par right = 
  let toks = pop_to_right true right in
  let toks' = List.filter (fun t -> not(t = Par)) toks in
  let error = if List.length toks = List.length toks' then [] else [Error "unexpected par while scanning {}"] in
  error @ toks' ;;

let rec pop_matched_brace_list acc k = 
  if (k=0) then List.rev acc else 
    let t = pop_to_right_wo_par RBrace in 
    pop_matched_brace_list (t::acc) (k-1);;

(* tuples *)
 (* convert tuple to list at outermost layer, with respect to () {}:
   (a,b,(c,d),e,\frac{f}{g,h}) --> [a;b;(c,d);e;\frac{f}{g,h}]
 *)

let rec list_of_tuple_rec b insep outsep toks = (* outer brackets not included *)
  let (_,toks',_) = leveled_read_until [] (fun ls -> (List.hd ls,List.tl ls)) toks []
   (fun (ds,a) -> if (ds = [] && a = insep) then outsep else a)
   (fun (_,_) -> false) in (* read until List.hd gives error *)
  toks';;

(*
  match toks with
  | [] -> paren(List.rev acc)
  | a :: rest ->
      match a with
      | LBrace -> 
          let (m,rest') = mate_brace_from_list rest in 
          let ls = brace(m) in 
          list_of_tuple_rec sep ((List.rev ls) @ acc) rest'
      | LParen -> 
          let (m,rest') = mate_paren_from_list rest in 
          let ls = paren(m) in
          list_of_tuple_rec sep ((List.rev ls) @ acc) rest' 
      | RParen -> write_error "unmatched ) in list_of_tuple_rec"; paren(List.rev acc )
      | Comma -> list_of_tuple_rec sep (LParen :: sep(RParen :: acc)) rest
      | _ -> list_of_tuple_rec sep (a :: acc) rest;;
 *)

let list_of_noparen_tuple b toks =
  bracket(list_of_tuple_rec b Comma Semi toks);;

let curryargs b toks = 
  list_of_tuple_rec b Comma Ignore toks;;

let test = list_of_noparen_tuple false [LParen;RParen];;
let test = list_of_noparen_tuple false [ControlSeq "A";Comma;ControlSeq "B";Comma;ControlSeq "C"];;
let test = curryargs false [ControlSeq "A";Comma;ControlSeq "B";Comma;ControlSeq "C"];;



(* macro expansion *)

let rec macroexpand acc pattern args = 
  match pattern with
  | [] -> List.rev acc
  | (Arg i) :: rest -> macroexpand (List.rev(List.nth args (i-1)) @ acc) rest args
  | t :: rest -> macroexpand (t :: acc) rest args;;

let macrotable = ref [];;
let setmacro f = (macrotable := f :: !macrotable);;

let rec opt_assoc s ls = 
  match ls with
  | [] -> None
  | e::tl -> if (s = fst e)  then Some (snd e) else opt_assoc s tl;;

(* control sequence processing *)

let is_math_font cs_string = 
  (String.length cs_string > 3) && ("math" = String.sub cs_string 0 4);;

let test = is_math_font "mathfrak";;
let test = is_math_font "Bbb";;

 (* Several functions take p_environ, which eventually gets instantiated with the
    function process_environment. 
  *)

let nopar toks s = 
  let toks' = List.filter (fun t -> not(t = Par)) toks in
  let _ = (List.length toks' = List.length toks) || (write_error s; true) in
  toks';;

let process_controlseq p_environ p_document cs_string =
  match cs_string with
  | "CnlDelete" -> 
      let i = pop_brack_num true in
      let cs = popcs true in setmacro (get_csname cs,(i,[])); []
  | "CnlNoExpand" -> 
      let i = pop_brack_num true in
      let cs = popcs true in setmacro (get_csname cs,(i,[no_expand;cs])); []
  | "CnlCustom" | "CnlDef"  ->
      let i = pop_brack_num true in
      let cs = popcs true in
      let toks = pop_to_right_wo_par RBrace in setmacro (get_csname cs,(i,toks)); []
  | "input" -> 
      let toks = pop_to_right_wo_par RBrace in
      (match toks with
      | [Tok s] -> 
          let ls = [LBrack;Tok "read";Symbol "\"";Tok s;Symbol "\"";RBrack] in
          let _ = p_document s in
          ls
      | _ -> [Error "\\input filename missing"])
  | "noexpand" -> 
      let cs = popcs true in [cs]
  | "begin" ->
      let toks = pop_to_right_wo_par RBrace in p_environ(toks)
  | "var" -> [mk_var (pop_to_right_wo_par RBrace)]
  | "id" -> [mk_id (pop_to_right_wo_par RBrace)]
  | "list" ->
      let toks = pop_to_right_wo_par RBrace in 
      (returnstream (list_of_noparen_tuple true toks) ; [])
  | "concat" ->
      let t1 = pop_to_right_wo_par RBrace in
      let t2 = pop_to_right_wo_par RBrace in
      (match (t1,t2) with 
       | ([Tok s1],[Tok s2]) -> [Tok (s1^s2)]
       | _ -> [Error ("cannot concat ("^(tokens_to_clean_string t1)^") ("^
                        (tokens_to_clean_string t2)^")")])
  | "app" ->
      let ftoks = (pop_to_right_wo_par RBrace) in 
      let argtoks = (pop_to_right_wo_par RBrace) in 
      let cargs = nopar argtoks "illegal par in cargs" in
      let ls = paren(paren(ftoks) @ cargs) in 
       (returnstream (ls) ; [])
  | _ -> let otoks = opt_assoc cs_string !macrotable in
         (match otoks with
         | Some (k,pat) -> 
             let args = (pop_matched_brace_list [] k) in 
             let repl_text = macroexpand [] pat args in 
             (returnstream repl_text;[])
         | None ->
             if is_math_font(cs_string) then (* \mathfrak{C} -> C__mathfrak *)
               let toks = pop_to_right_wo_par RBrace in 
               if (List.length toks != 1) then (returnstream toks; [])
               else 
                 match List.hd toks with 
                 | Tok s -> [Tok (s^"__"^cs_string)]
                 | _ -> (returnstream toks; [])
             else [ControlSeq cs_string]);;

(* environments *) 


let delete_option b = (* material in [] *)
  let l = peek b in
  if (l = LBrack) then 
    ignore(pop_to_right b RBrack)
  else ();;

let get_label() = (* read \label *)
 let l = peek true in
 match l with
 | ControlSeq "label" -> 
     let toks = pop_to_right_wo_par RBrace in Some(mk_label toks)
 | _ -> None;;

let write_declaration_preamble s label = 
  let _ = write_output (Tok s) in
  let _ = match label with | None -> () | Some t -> write_output t in
  write_output (Symbol ".");;

let transcribe1 p_environ p_document writeif a = 
  let p a = if writeif a then write_output a else () in
  let ps toks = write_output_list (List.filter writeif toks) in 
  match a with
  | ControlSeq s -> ps(process_controlseq p_environ p_document s) (* recursion here into sub environ *)
  | Arg _ -> write_error "# parameters must appear in macro patterns"
  | FormatEol -> write_error "FormatEol must appear in environment"
  | FormatCol -> write_error "FormatCol must appear in environment"
  | Eol -> write_error "unexpected EOL"
  | Eof -> write_error "unexpected EOF"
  | _ -> p a;;

let rec transcribe_until b p_environ writeif endif =
  let a = pop b in
  if (endif a) then (returnstream [a])
  else 
    let () = transcribe1 p_environ writeif a in
    transcribe_until b p_environ writeif endif;;

let declaration_table = 
  [
    ("def","Definition");
    ("thm","Theorem");
    ("cor","Corollary");
    ("prop","Proposition");
  ];;

let cleanup_environ b env_name = 
  let t = popcs b in
  let _ = (t = ControlSeq "end") || (write_error ("end "^env_name^" expected"); true) in
  let s = tokens_to_clean_string(pop_to_right_wo_par RBrace) in
  let _ = (s = env_name) || (write_error ("end "^env_name^" expected; "^s^" found"); true) in
  ();;

let transcribe_decl_environ p_environ env_name = (* \begin{definition} has been read *)
 let _ = delete_option false in
 let label = get_label() in 
 let odecl = opt_assoc env_name declaration_table in 
 let s = match odecl with 
   | None -> String.capitalize_ascii env_name
   | Some s' -> s' in
 let _ = write_declaration_preamble s label in 
 let _ = transcribe_until false p_environ (fun _ -> true) ((=) (ControlSeq "end")) in
 cleanup_environ false env_name;;

let delete_environ b p_environ env_name =  (* \begin{env_name} already read *)
  let _ = transcribe_until b p_environ (fun _ -> false) ((=) (ControlSeq "end")) in
  cleanup_environ b env_name;;

let ignore_in_environ b p_environ env_name ignored_chars = 
  let _ = transcribe_until b p_environ (fun a -> not(List.mem a ignored_chars))
            ((=) (ControlSeq "end")) in
  cleanup_environ b env_name;;

let process_environment toks = toks;; (* debug *)


let process_document s = 
   open_file;
   tokenize;
   look_for_cnl_start;
   process_to_cnl_end;
   continue_to_loop_until_eof;
   flush_file_and_close;
   



(* fin *)
