(* types *)

open Type;;

(*

look for packages

\usepackage[utf8]{inputenc}
\inputencoding 

\usepackage[spanish]{babel}, etc.
\usepackage{CJK} % Chinese 

 *)



(* ignore and ignore stars following cs.
   This is the default. There is no need to 
 list them all *)

let rec droplast = 
  function
  | _ :: t2 :: ts -> droplast (t2 :: ts)
  | _ -> []

let split_replace (a,a') s = 
  let  s' = String.split_on_char a s in 
  String.concat a' s'

let rec string_escaped als s = 
  match als with
  | [] -> s
  | (a,a')::als' -> string_escaped als' (split_replace (a,a') s)
    
let html_escaped = string_escaped 
[    ('&',"&amp;"); 
     ('<',"&lt;");
     ('>',"&gt;");
     ('\'',"&apos;");
     ('\"',"&quot;")]

let _ = html_escaped "&lt &<\'\""

(* default. don't list
let cs_ignore = 
  ["documentclass";"usepackage";"title";
   "author";"date";"maketitle";"renewcommand";
   "setcounter";"tableofcontents";"listoftables";
   "newcommand"]
 *)

(* create an item for the cs and its args *)
let cs_record = (* <cs name="chapter">...</cs> *)
  ["chapter";"section";"subsection";"subsubsection";
   "part";"paragraph";"subparagraph";"emph";"textbf"]

(* replace cs by its text equivalent *)
let cs_text  = 
  ["alpha";"beta";"gamma";
   "ldots";"cdots"]


(* remove marking cs, pass the data inside through.
 *)
let cs_unmark = []


(* *)

(* allowed environments. Marked .
   [] {} directlly after \begin{env} are ignored.  *)

let env_record = 
  ["abstract";"enumerate";"itemize";"description";
   "labeling"]

(* keep data inside environment without 
   begin and end markers *)

let env_unmark = 
  ["spacing"]

let env_rename = (* allowed, renamed *)
  [("def","definition")]

(* string operations *)

let name_tag n = if n="" then "" else (" name=\"" ^html_escaped n^ "\"")

let empty_tag s n = "<"^s^ name_tag n ^ "/> "

let content_tag s n c = "<"^s^ name_tag n ^ "> " ^ html_escaped c ^ " </"^s^"> "

let tag_open s n = "<"^s^ name_tag n ^ "> "

let tag_closed s n = "</"^s^ name_tag n ^ "> "

let _ =  (empty_tag "cs" "input")

let _ =  (content_tag "cs" "input" "reinhardt.tex")

let token_to_string = Type.lex_token_to_string 

let tokens_to_string sep toks = 
  let cs = List.map token_to_string toks in
  String.concat sep cs;;

let token_to_verbose_string = Type.token_to_verbose_string

let token_to_string_output =
  function
  | XmlCs (s) -> empty_tag "cs" s
  | XmlEnv (s) -> empty_tag "env" s
  | XmlDisplayMath -> empty_tag "display" ""
  | XmlMath -> empty_tag "math" ""
  | XmlError n -> empty_tag "error" n
  | XmlWarn n -> empty_tag "warn" n
  | XmlMessage n -> empty_tag "message" n
  | Dollar -> " "
  | Display -> " "
  | Eof -> " "
  | BeginEnv _ -> " "
  | EndEnv _ -> " "
  | ControlSeq s -> "\\"^s^" "
  | Input _ -> " "
  | NotImplemented _ -> " "
  | _ as t -> (token_to_string t)^" ";;


let _ = token_to_string_output (Input "file");;

(* debug *)


let debug = false;;

let print_debug_endline s = if debug then print_endline s else ();;
let print_debug s = if debug then print_string s else ();;

let print_debug_token tok = 
  if debug then print_string ((token_to_string_output tok)^" ") else ();;

(* I/O output *)

let output_recent = ref (Ignore,Ignore)

let output_prev r = fst r

let output_prev2 r = 
  [fst r ; snd r]

let rec subset xs ys = 
  match xs with 
  | [] -> true
  | t :: ts -> List.mem t ys && subset ts ys

let eol_count = ref 0

let output_token tok = 
  let _ = print_debug_token tok in 
  let r = !output_recent in 
  let _ = output_recent := (tok,fst !output_recent) in 
  let ism t = List.mem t [XmlMath;XmlDisplayMath] in
  ( 
    if (ism tok && ism (output_prev r)) then ()
    else if (ism tok && 
               subset (output_prev2 r) [XmlMath;Eol;XmlDisplayMath]) 
    then (output_recent := (Eol,Eol))
    else if (tok = Eol && subset (output_prev2 r) [Eol]) then ()
    else 
      (
        let e = !eol_count in 
        if (tok = Eol) then eol_count := !eol_count + 1
        else 
          (eol_count := 0;
          if (e > 1) 
          then 
            (
             print_string (tag_closed "par" "");
             print_string ("\n\n");
             print_string (tag_open "par" "");
            );
           print_string (token_to_string_output tok));
      ));;

let output_token_list toks = 
  ignore(List.map output_token toks);;

let (output_error,get_error_count) =
  let error_limit = 1000 in
  let error_count = ref 0 in 
  let o_error s = output_token (XmlError s) in
  ((fun s ->
        let _ = o_error s in
        let _ = error_count := 1 + !error_count in
        if !error_count > error_limit then 
          let _ = o_error "error_limit exceeded" in
          failwith "error limit exceeded"  (* debug, close all files *)
        else ()),
  (fun () -> !error_count));;

let output_failure s = (output_error s; failwith s)

let mk_infile s = 
  if Sys.file_exists s then s 
  else 
    let s' = s ^ ".tex" in
    if Sys.file_exists s' then s'
    else (output_failure ("File "^s^" does not exist"))

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

let mk_iochannels f s = (* f should terminate lines with Eol *)
  let infile = mk_infile s in
  let rs = 
    try (read_lines infile) 
    with _ -> output_failure ("unable to read file "^s) in 
  let toks = 
    try List.map f rs 
    with _ -> output_failure ("unable to tokenize file "^s) in
  {
    infile = infile; 
    intoks = List.flatten toks;
  };;

(* I/O input, peek, and return *)

let regard_par = function
  | TrackPar -> true
    | _ -> false;;

let next_eol ios = 
  not(ios.intoks = []) && (Eol = List.hd ios.intoks);;

let inputE  = 
  let input_1 ios partrack =
    if ios.intoks = [] then Eof else 
      let tok = List.hd ios.intoks in
      let _ = ios.intoks <- List.tl ios.intoks in
      let tok' = if regard_par partrack && (tok = Eol) && next_eol ios
               then Par 
               else tok in
      tok' in
  let rec input_rec ios partrack give_output = 
    match input_1 ios partrack with 
      | Comment _ -> (input_rec ios partrack give_output)
      | Eol -> (if give_output then output_token Eol; input_rec ios partrack give_output)
      | Par -> (if give_output then output_token Eol; Par)
      | _ as t -> t in 
  input_rec;;

let input ios partrack = inputE ios partrack true

let return_input ios ls = 
  let ls' = List.filter (fun t -> not(t = Par)) ls in
  ios.intoks <- ls' @ ios.intoks;;

let input_filter ios b f s = 
  let t = input ios b in
  let s' = ", found "^(token_to_verbose_string t) in
  let _ = f t || (output_error (s^s') ; true) in 
  t;;

let input_tok ios b tok = input_filter ios b ((=) tok) 
  ("expected "^token_to_verbose_string tok)

let peek ios b = 
  let tok = input ios b in
  let _ = return_input ios [tok] in
  tok;;

let is_controlseq t = 
  match t with 
  | ControlSeq _ -> true
  | _ -> false;;

let get_csname = function
  | ControlSeq s -> s
  | _ -> "";; (* quiet failure *)

let input_cs ios b = input_filter ios b is_controlseq "control sequence  expected" ;;


(* delimiters {} [] ()  *)

 (* wrappers *)

let brace(toks) = LBrace :: toks @ [RBrace];;

let bracket(toks) = LBrack :: toks @ [RBrack];;

let left_mate = 
  function 
  | RBrace -> LBrace
  | RBrack -> LBrack
  | RDisplay -> LDisplay
  | Dollar -> Dollar
  | Display -> Display
  | ControlSeq ")" -> ControlSeq "("
  | ControlSeq "]" -> ControlSeq "[" (* \[ \] *)
  | _ -> NotImplemented "";;


let check_mate (ldlims,tok) = 
  match ldlims with 
  | t :: ldlims' -> 
      (let _ = (t = left_mate tok) || (output_error "mismatched end group"; true) in
       ldlims')
  | _ -> (output_error "unexpected end group"; [])

let level_toks = 
  [ControlSeq "(";ControlSeq "[";LBrace;LBrack;LDisplay
  ;ControlSeq ")";ControlSeq "]";RBrace;RBrack;RDisplay
  ;Dollar;Display
  ]

let record_level matchf (ldlims,tok) = (* ldlims = left delimiter stack  *)
  if not(matchf tok) then (ldlims,[tok])
  else
    match tok with
    | ControlSeq "(" | ControlSeq "[" | LBrace | LBrack 
    | LDisplay -> 
        (tok :: ldlims,[tok])
    | ControlSeq ")" | ControlSeq "]" | RBrace | RBrack 
    | RDisplay -> 
        let ldlims' = check_mate (ldlims,tok) in (ldlims', [])
    | Dollar -> if (match ldlims with | Dollar :: _ -> true | _ -> false) then 
                  (List.tl ldlims,[])
                else (Dollar :: ldlims,[])
    | Display -> if (match ldlims with | Display :: _ -> true | _ -> false) then 
                   (List.tl ldlims,[])
                 else (Display :: ldlims,[])
    | _ -> (ldlims,[tok]);;

let rec leveled_read_until acc ios b matchf ldlims endif = 
  try (
    let tok = input ios b in
    if endif (ldlims,tok) then ([tok],List.rev acc)
    else 
      let (ldlims',tok') = record_level matchf (ldlims,tok) in 
      leveled_read_until 
        (tok' @ acc) ios b matchf ldlims' endif)
  with _ -> ([],List.rev acc);;

 (* 
   The left,right are the delimiters to be matched.

   The ldlims is the nesting of left delimiters.
   The code is organized in such a way that the first left is often already
   encountered when mate_delim is initialized with say ldlims=[LBrace].
 *)

let mate_delim ios b matchf right ldlims = 
  let left = left_mate right in
  let endif = (fun (ldlims,tok) -> (ldlims = [left] && (tok = right))) in 
  let (_,toks) = leveled_read_until [] ios b matchf ldlims endif in
  toks;;

(* input_matched_brace input balanced expression, output doesn't include delimiters.  *)
let input_to_right ios b ignore right = 
  let _ = input_tok ios b (left_mate right) in 
  mate_delim ios b ignore right [left_mate right];;

let input_to_right_wo_par ios ignore right = 
  let toks = input_to_right ios TrackPar ignore right in
  let toks' = List.filter (fun t -> not(t = Par)) toks in
  let errormsg = "unexpected par before "^token_to_verbose_string right in
  let error = if List.length toks = List.length toks'
              then []
              else [XmlError errormsg] in 
  error @ toks' ;;

let match_brace t = t = RBrace || t = LBrace

let rec delete_matching_brack_brace ios b = (* material in [] *)
  let match_brack t = match_brace t || t = RBrack || t = LBrack in
  let l = peek ios b in
  if (l = LBrack) then 
    (ignore(input_to_right ios b match_brack RBrack);
     delete_matching_brack_brace ios b)
  else if (l = LBrace) then 
    (ignore(input_to_right ios b match_brace RBrace);
     delete_matching_brack_brace ios b)
  else ()

let rec delete_matching_math ios b = 
  let match_math t = List.mem t [Dollar;LBrace;RBrace;Display;RDisplay;LDisplay] in
  let l = peek ios b in
  if (l = Dollar || l = Display) then 
    (let t = if (l = Dollar) then XmlMath else XmlDisplayMath in 
     output_token t;
     ignore(input_to_right ios b match_math l);
     delete_matching_math ios b)
  else if (l = LDisplay) then
    (output_token XmlDisplayMath;
     ignore(input_to_right ios b match_math RDisplay);
     delete_matching_math ios b)
  else ()


(* environments *)

let rec opt_assoc ls s = 
  match ls with
  | [] -> None
  | e::tl -> if (s = fst e)  then Some (snd e) else opt_assoc tl s;;

let rec default_assoc ls s = 
  match ls with 
  | [] -> s
  | e::tl -> if (s = fst e) then snd e else default_assoc tl s;;

(* control sequence processing *)

let nopar toks s = 
  let toks' = List.filter (fun t -> not(t = Par)) toks in
  let _ = (List.length toks' = List.length toks) || (output_error s; true) in
  toks';;

let process_controlseq ios cs_string =
(*  let _ = print_debug ("[process-cs:"^cs_string^"]") in *)
  match cs_string with
  | "par" -> []
  | "_" -> [Tok "_"]
  | _ -> (delete_matching_brack_brace ios TrackPar; [ControlSeq cs_string]);;

(* environments  *) 

 (* move one token to output, raising exception if Eof *)

let transcribe_token ifexpand outputif tr tok = 
  let p tok = if outputif tok then output_token (default_assoc tr tok) else () in
  let ps toks = output_token_list
                  (List.map (default_assoc tr) (List.filter outputif toks)) in 
  let _ = ifexpand in
  let _ = ps in 
  match tok with
(*  | ControlSeq s -> if ifexpand then ps(process_controlseq s) else p tok *)
  | Eol -> output_error "unexpected EOL"
  | Eof -> raise End_of_file
  | _ -> p tok;;

let null_env  = 
  {
    name = "NULL_ENV";
    begin_token = Ignore;
    end_token = Ignore;
    tr_token = [];
    drop_toks = [];
    is_delete = false;
    stay_in_par = NoTrackPar;
  };;

let mk_drop_env s drop is_delete = 
  {
    name = s;
    begin_token = Tok (tag_open "env" s);
    end_token = Tok (tag_closed "env" s);
    tr_token = [];
    drop_toks = drop;
    is_delete = is_delete;
    stay_in_par = NoTrackPar;
  };;


let mk_delete_env s = 
  {
    name = s;
    begin_token = XmlCs s;
    end_token = Ignore;
    tr_token = [];
    drop_toks = [];
    is_delete = true;
    stay_in_par = NoTrackPar;
  };;

let mk_itemize_env s item is_delete =
  {
    name = s;
    begin_token = Tok (tag_open "env" s);
    end_token = Tok (tag_closed "env" s);
    tr_token = [(ControlSeq "item",item)];
    drop_toks = [];
    is_delete = is_delete;
    stay_in_par = TrackPar;
  };;

let mk_case_env s is_delete =
  {
    name = s;
    begin_token = Tok "case";
    end_token = Tok "end";
    tr_token = [];
    drop_toks = [];
    is_delete = is_delete;
    stay_in_par = TrackPar;
  };;

let is_prologue_env s =
  match s with
  | "def" | "definition" | "Definition" 
  | "thm" | "theorem" | "Theorem" | "lemma" | "remark"
  | "prop" | "proposition" | "Proposition"
  | "axiom" | "Axiom"
  | "hyp" | "hypothesis" | "Hypothesis"
  | "cor" | "corollary" | "Corollary" -> true
  | _ -> false;;


let mk_e_item s e =
  if is_prologue_env s then mk_drop_env s [] (e.is_delete)
  else 
    match s with
    | "itemize" | "enumerate"  ->
                    mk_itemize_env s  (Tok "_item_") (e.is_delete)
      | "center" -> mk_drop_env s [] (e.is_delete)
      | "cases" -> mk_case_env s e.is_delete
      | "flushleft" | "flushright" | "minipage" 
      | "quotation" | "quote" | "verse" -> mk_drop_env s [] (e.is_delete)
      | "tabbing" -> mk_drop_env s [ControlSeq "=";ControlSeq ">";ControlSeq "+";ControlSeq "-"] e.is_delete
      | "multline" | "split" -> mk_drop_env s [] (e.is_delete)
      | _ -> (mk_delete_env s);;


 (*
   output token will be unhandled EndDocument, Input, Eof 
  *)

let rec process_environ ios e_stack =
  let out tok is_delete = if (is_delete) then () else output_token tok in 
  let par_filter e = 
    let nrp = not(regard_par e.stay_in_par) in
    let _ = nrp || (output_error 
                      ("illegal paragraph ending in environment "^e.name); true) in
    nrp in
  if (e_stack=[]) 
  then (output_error "unexpected empty environment. ending document envir"; EndDocument) else 
    let e = List.hd e_stack in
    let tok = inputE ios (e.stay_in_par) (not e.is_delete) in 
    match tok with
    | ControlSeq s -> (delete_matching_brack_brace ios TrackPar; 
                       out (XmlCs (s)) e.is_delete;
                       process_environ ios e_stack)
    | Dollar | Display | LDisplay -> 
                           (return_input ios [tok]; delete_matching_math ios TrackPar; 
                           process_environ ios e_stack)
    | Eof -> Eof 
    | Par -> (output_error "illegal paragraph ended before envir end."; 
              let ignore_par_stack = 
                List.filter par_filter  e_stack in 
              process_environ ios ignore_par_stack)
    | EndEnv s -> if (s = null_env.name) then
                    let msg = null_env.name^" is an illegal environment name; ignored" in
                    (output_error msg; process_environ ios e_stack)
                  else if (s = e.name) then
                    ((if not(e_stack = []) then
                       let e' = List.hd e_stack in 
                       out e.end_token e'.is_delete); 
                     process_environ ios (List.tl e_stack))
                  else 
                    let msg = "\\end{" ^(e.name)^ "} expected, \\end{" 
                              ^s^ "} found; input ignored." in
                    (output_error msg; process_environ ios e_stack) 
    | BeginEnv s -> if (s = null_env.name) then
                    let msg = null_env.name^" is an illegal environment name; ignored" in
                    (output_error msg; process_environ ios e_stack)
                    else
                      if (s = "document") then 
                        (output_token(XmlMessage "begin document");
                         process_environ ios e_stack)
                      else
                        let e' = mk_e_item s e in
                        let _ = if not(e.is_delete)
                                then out e'.begin_token e.is_delete in
                        process_environ ios (e' :: e_stack)
    | _ -> let ifexpand = not(e.is_delete) in
           let outputif tok = not(e.is_delete) && not(List.mem tok (e.drop_toks)) in 
           let tr = e.tr_token in
           (transcribe_token ifexpand outputif tr tok;
            process_environ ios e_stack)
;;

let rec debug_process_environ ios e  = 
  let tok = input ios NoTrackPar in 
  match tok with
  | EndDocument | Input _ | Eof -> (tok) 
    | _ ->          (transcribe_token false (fun _ -> true) [] tok;
            debug_process_environ ios e)
;;

let rec process_ios convert_toks io_stack = 
  if io_stack = [] then true 
  else
    try 
      let ios = List.hd io_stack in
      let _ = output_token (XmlMessage ("reading from "^ ios.infile)) in
      match process_environ ios [null_env] with
      | Eof -> (output_token(XmlMessage("end of file "^ios.infile));
                process_ios convert_toks (List.tl io_stack))
      | EndDocument -> 
          (output_token (XmlMessage("end of document in"^ios.infile));
           true)
      | Input filename -> 
          (let ios' = mk_iochannels convert_toks filename in
           output_token (XmlMessage ("input file "^filename));
           process_ios convert_toks (ios' :: io_stack))
      | _ -> (output_error ("fatal file error. Popping file stack.");
              process_ios convert_toks (List.tl io_stack))
    with _ -> (output_token(XmlError("unknown exception")); false);;

  
let process_doc convert_toks filename = 
  print_string "<root><par>";
  ignore(process_ios convert_toks [mk_iochannels convert_toks filename]);
  print_string "</par></root>";;


(* fin *)
