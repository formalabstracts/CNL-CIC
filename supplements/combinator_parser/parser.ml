(*
 Combinator Parser for CNL
 Thomas Hales, 2019
 

 *)

open Cnl_parse.Lexer_cnl

(* a few lines from HOL Light lib.ml *)

let rec assocd a l d =
  match l with
    [] -> d
  | (x,y)::t -> if Pervasives.compare x a = 0 then y else assocd a t d;;

let rec rev_assocd a l d =
  match l with
    [] -> d
  | (x,y)::t -> if Pervasives.compare y a = 0 then x else rev_assocd a t d;;

let rec zip l1 l2 =
  match (l1,l2) with
        ([],[]) -> []
      | (h1::t1,h2::t2) -> (h1,h2)::(zip t1 t2)
      | _ -> failwith "zip";;

let rec unzip =
  function [] -> [],[]
         | ((a,b)::rest) -> let alist,blist = unzip rest in
                            (a::alist,b::blist);;

let rec itlist2 f l1 l2 b =
  match (l1,l2) with
    ([],[]) -> b
  | (h1::t1,h2::t2) -> f h1 h2 (itlist2 f t1 t2 b)
  | _ -> failwith "itlist2";;

let rec butlast l =
  match l with
    [_] -> []
  | (h::t) -> h::(butlast t)
  | [] -> failwith "butlast";;

let rec last l =
  match l with
    [x] -> x
  | (_::t) -> last t
  | [] -> failwith "last";;

let rec chop_list n l =
  if n = 0 then [],l else
  try let m,l' = chop_list (n-1) (List.tl l) in (List.hd l)::m,l'
  with Failure _ -> failwith "chop_list";;

let _ = chop_list 3 [5;6;7;8;9;10;11;12];;


(* Here are a few lines from the HOL Light parser.ml *)

exception Noparse;;

let (|||) parser1 parser2 input =
  try parser1 input
  with Noparse -> parser2 input;;

let (++) parser1 parser2 input =
  let result1,rest1 = parser1 input in
  let result2,rest2 = parser2 rest1 in
  (result1,result2),rest2;;

let rec many prs input =
  try let result,next = prs input in
      let results,rest = many prs next in
      (result::results),rest
  with Noparse -> [],input;;

let (>>) prs treatment input =
  let result,rest = prs input in
  treatment(result),rest;;

let fix err prs input =
  try prs input
  with Noparse -> failwith (err ^ " expected");;

let rec listof prs sep =
  prs ++ many (sep ++ prs >> snd) >> (fun (h,t) -> h::t);;

let nothing input = [],input;;

let unit input = ((),input);;

let elistof prs sep =
  listof prs sep ||| nothing;;

let leftbin prs sep cons err =
  prs ++ many (sep ++ fix err prs) >>
  (fun (x,opxs) -> let ops,xs = unzip opxs in
                   itlist2 (fun op y x -> cons op x y) (List.rev ops) (List.rev xs) x);;

let rightbin prs sep cons err =
  prs ++ many (sep ++ fix err prs) >>
  (fun (x,opxs) -> if opxs = [] then x else
                   let ops,xs = unzip opxs in
                   itlist2 cons ops (x::butlast xs) (last xs));;

let possibly prs input =
  try let x,rest = prs input in [x],rest
  with Noparse -> [],input;;

let some p = 
  function
      [] -> raise Noparse
    | (h::t) -> if p h then (h,t) else raise Noparse;;

let someX p = 
  function
    [] -> raise Noparse
  | h :: t -> let (b,x) = p h in
              if b then (x,t) else raise Noparse;;

let a tok = some (fun item -> item = tok);;

let rec atleast n prs i =
  (if n <= 0 then many prs
   else prs ++ atleast (n - 1) prs >> (fun (h,t) -> h::t)) i;;

let finished input =
  if input = [] then 0,input else failwith "Unparsed input";;

(* end of HOL Light *)

let out _ = ();;

let failparse _ = raise Noparse
  

let canparse f x = try (ignore(f x); true) with Noparse -> false

 (* commitment test *)
let ( <|> ) (test,parser1) parser2 input = 
  if canparse test input then 
    parser1 input 
  else parser2 input

let commit err test parse input = 
  if canparse test input then 
    fix err parse input  
  else parse input 

let paren parser  = 
  (a L_PAREN ++ parser ++ a R_PAREN) >> (fun ((_,p),_) -> p) 

let bracket parser = 
  (a L_BRACK ++ parser ++ a R_BRACK) >> (fun ((_,p),_) -> p) 

let brace parser = 
  (a L_BRACE ++ parser ++ a R_BRACE) >> (fun ((_,p),_) -> p) 

let brace_semi parser =
  brace(listof parser (a SEMI))

let opt_paren parser = 
  paren parser ||| parser

let comma = a COMMA 

let comma_nonempty_list parser = 
  listof parser comma 

let comma_list err parser = 
  elistof err parser comma

let opt_comma_nonempty_list parser = 
  listof parser (possibly comma) 

(* let sep_list below *)

let lit_binder_comma = comma

let cs_brace parser1 parser2 = 
  parser1 ++ elistof (brace parser2) nothing 


(* set up synonym hashtable *)
(* XX need to add multiword synonyms *)

let synonym = Hashtbl.create 200;;

let sort = List.sort_uniq (fun a b -> if (a > b) then 1 else if (a=b) then 0 else -1);;

let _ = sort ["the";"THE";"that";"a";"z"]

let syn_add1 (key,value) = 
  if Hashtbl.mem synonym key then 
    let value' = Hashtbl.find synonym key in 
    failwith ("synonym already declared "^key^" "^value')
  else
    Hashtbl.add synonym key value

let syn_add ts = 
  let ts' = sort (List.map String.uppercase_ascii ts) in
  if ts' = [] then ()
  else 
    let value = List.hd ts' in
    ignore (List.map (fun t -> syn_add1 (t,value)) ts')

let find_syn key = 
  try Hashtbl.find synonym key 
  with Not_found -> key


let frozen = 
  List.map syn_add (List.map (fun t -> [t]) 
[
"A";"AN";"ALL";"AND";"ANY";"ARE";"AS";"ASSUME";"BE";"BY";
"CASE";"CLASSIFIER";"CLASSIFIERS"
])

let word s = 
  let u = find_syn (String.uppercase_ascii s) in 
  someX (function 
      | WORD (w,wu) -> 
          let wsyn = find_syn wu in
          ((wsyn = u),WORD (w,wsyn))
      | _ -> (false,UNKNOWN""))

let rec parse_all ps input = 
  match ps with
  | [] -> ([],input)
  | p::ps' -> let (result,rest) = p input in 
              let (result',rest') = parse_all ps' rest in
              (result :: result'),rest'

let rec parse_some ps input = 
  match ps with 
  | [] -> raise Noparse
  | p :: ps' -> try (p input) with Noparse -> parse_some ps' input 
  
let phrase s = 
  let ps = List.map word (String.split_on_char ' ' s) in 
  parse_all ps

let someword s = 
  let s' = List.map word (String.split_on_char ' ' s) in
  parse_some s'

let rec somecomb parser = 
  function
  | [] -> failparse 
  | t :: ts -> parser t ||| somecomb parser ts

let phrase_list_transition = 
let w _ = "phrase list transition" in
(somecomb phrase
[
"a basic fact is";"accordingly";"additionally";"again";"also";"and yet";"as a result";
"as usual";"as we have seen";"as we see";"at the same time";"besides";"but";
"by definition";"certainly";"clearly";"computations show";"consequently";
"conversely";"equally important";"explicitly";"finally";"first";"for example";
"for instance";"for simplicity";"for that reason";"for this purpose";"further";
"furthermore";"generally";"hence";"here";"however";"importantly";"in addition";
"in any event";"in brief";"in consequence";"in contrast";"in contrast to this";
"in each case";"in fact";"in general";"in other words";"in particular";"in short";
"in sum";"in summary";"in the present case";"in the same way";"in this computation";
"in this sense";"indeed";"it follows";"it is clear";"it is enough to show";
"it is known";"it is routine";"it is trivial to see";"it is understood";
"it turns out";"last";"likewise";"more precisely";"moreover";"most importantly";
"neverthess";"next";"nonetheless";"note";
"notice";"now";"observe";"obviously";"of course";"on the contrary";"on the other hand";
"on the whole";"otherwise";"second";"similarly";"so";"specifically";"still";
"that is";"the point is";"then";"therefore";"third";"this gives";"this implies";
"this means";"this yields";"thus";"thus far";"to begin with";"to this end";
"trivially";"we claim";"we emphasize";"we first show";"we get";"we have seen";
"we have";"we know";"we check";"we may check";"we obtain";"we remark";"we say";"we see";
"we show";"we understand";"we write";"recall";"we recall";
"without loss of generality";"yet";
] ++ possibly (word "that") >> w)

let phrase_list_filler = 
  let w _ = "phrase_list_filler" in 
  (possibly (word "we") ++ someword "put write" >> w) |||
    (word "we" ++ someword "have know see" ++ possibly (word "that") >> w)

let phrase_list_proof_statement = 
  let w _ = "phrase_list_proof_statement" in
  (phrase  "we proceed as follows" >> w) |||
    (word "the" ++ someword "result lemma theorem proposition corollary" ++
       possibly (word "now") ++ word "follows" >> w) |||
    (phrase "the other cases are similar" >> w) |||
    (phrase "the proof is" ++ someword "obvious trivial easy routine" >> w)

let  sep_and_comma = 
  word "and" ||| comma 

let sep_list parser = listof parser sep_and_comma

let case_sensitive_word = 
  someX (function 
      | WORD (w,_) -> true,(ATOMIC_IDENTIFIER w)
      | _ -> (false,UNKNOWN ""))

let the_case_sensitive_word s = 
  someX (function 
      | WORD (w,_) -> (w=s),(ATOMIC_IDENTIFIER w)
      | _ -> (false,UNKNOWN ""))

let atomic_identifier =
  some (function 
      | ATOMIC_IDENTIFIER _ -> true
      | _ -> false)
    
let atomic =
  case_sensitive_word ||| atomic_identifier 

let hierarchical_identifier = 
  some (function
      | HIERARCHICAL_IDENTIFIER _ -> true
      | _ -> false)

let identifier = 
  (case_sensitive_word 
   >> 
     function 
     | ATOMIC_IDENTIFIER s -> HIERARCHICAL_IDENTIFIER s
     | _ -> failwith "Fatal:identifier") |||
    hierarchical_identifier 

let the_id s = 
  some (function
      | HIERARCHICAL_IDENTIFIER s' -> (s = s')
      | _ -> false)

let lit_a = 
  word "a" ||| word "an"

let article = 
  lit_a ||| word "the"

let lit_defined_as = 
  phrase "said to be" |||
    phrase "defined as" |||
    phrase "defined to be"

let lit_is = 
  let wbe = WORD ("be",find_syn "be") in
  let w = (fun _ -> wbe) in
  (word "is" >> w) |||
    (word "are" >> w) |||
    ((possibly (word "to") ++ word "be") >> w)

let lit_iff = 
  let wiff = WORD ("iff",find_syn "iff") in
  let w = (fun _ -> wiff) in
  (phrase "iff" >> w) |||
    (phrase "if and only if" >> w)  |||
    ((lit_is ++ possibly (word "the") ++ word "predicate") >> w)

let lit_denote = 
  let wd = WORD ("denote",find_syn "denote") in
  let w = (fun _ -> wd) in
  (phrase "stand for" >> w) |||
    (word "denote" >> w) 

let lit_do = someword "do does"

let lit_equal = word "equal" ++ word "to"

let lit_has = someword "has have"

let lit_with = someword "with of having"

let lit_true = someword "on true yes"

let lit_false = someword "off false no"

let lit_its_wrong = phrase "it is wrong that"

let lit_we_record = 
  possibly (word "we") ++
    someword "record register" ++
    possibly (word "that")

let lit_any = someword "every each all any some no" |||
    (phrase "some and every" >> (fun _ -> WORD ("some and every","SOME AND EVERY")))

let lit_exists = someword "exist exists"

let lit_lets = 
  (word "let" ++ possibly (word "us")) |||
    (word "we" ++ possibly (word "can"))

let lit_fix = someword "fix let"

let lit_assume = someword "assume suppose"

let lit_then = someword "then therefore hence"

let lit_choose = someword "take choose"

let lit_prove = someword "prove show"

let lit_say = someword "say write"

let lit_we_say = possibly (word "we") ++ lit_say ++ possibly (word "that")

let lit_left = someword "left right no"

let lit_type = someword "type types"

let lit_proposition = someword "proposition propositions"

let lit_field_key = 
  someword "coercion notationless notation parameter type map"

let lit_qed = someword "end qed obvious literal"

let lit_document = 
  someword "document article section subsection subsubsection subdivision division"

let lit_enddocument = 
  someword "endsection endsubsection endsubsubsection enddivision endsubdivision"

let lit_def = someword "def definition"

let lit_axiom = someword "axiom conjecture hypothesis equation formula"

let lit_property = someword "property properties"

let lit_with_properties = word "with" ++ lit_property

let lit_theorem = someword "proposition theorem lemma corollary"

let lit_location = parse_some [lit_document;lit_theorem;lit_axiom;lit_def]

let lit_classifier = someword "classifier classifiers"

let lit_sort = the_case_sensitive_word "Type" |||
                 the_case_sensitive_word "Prop"

let label = atomic

(* primitives XX *)

let section_tag = lit_document ||| lit_enddocument

let period = some (function | PERIOD -> true | _ -> false)

let current_scope = ref [""]

let string_of_scope = 
  String.concat "." (List.rev !current_scope)

let is_scope_end =
  function
  | "ENDSECTION" | "ENDSUBSECTION" | "ENDSUBSUBSECTION" | "ENDDIVISION" -> true
  | _ -> false

let scope_level = 
  function
  | "DOCUMENT" | "ARTICLE" -> 0
  | "SECTION" | "ENDSECTION" -> 1
  | "SUBSECTION" | "ENDSUBSECTION" -> 2
  | "SUBSUBSECTION" | "ENDSUBSUBSECTION" -> 3
  | "DIVISION" | "SUBDIVISION" | "ENDDIVISION" | "ENDSUBDIVISION" -> 4
  | s -> failwith ("bad scope_level "^s)

let pad k x ls =
    if (k <= List.length ls) then snd(chop_list k ls)
    else (List.init (k - List.length ls) (fun _ -> x) @ ls)

let rec cutat p =
  function
  | [] -> failwith "cutat not found "
  | t :: ts as ls -> if p t then ls else cutat p ts 

let getlabel =
  function
  | [] -> "" 
  | [ATOMIC_IDENTIFIER l] -> l 
  | _ -> failwith "bad format: section label"

(* XX do scoping of variables etc. *)

let treat_section_preamble =
  function
  | ((WORD(_,sec),ls),_) -> (
    let l = getlabel ls in
    let scope_end = is_scope_end sec in
    let new_level = scope_level sec in 
    if not(scope_end) then 
      if (4 <= new_level) then 
        current_scope := l :: !current_scope
      else 
        current_scope := l :: (pad (new_level) "" !current_scope)
    else (* scope_end *) 
      if (4 <= new_level) then 
        current_scope := List.tl (cutat (fun s -> (l="" || l = s)) !current_scope)
      else 
        let _ = new_level < List.length !current_scope || 
                  failwith "ending division that was not started" in 
        let ts = pad (new_level + 1) "" !current_scope in 
        let _ = (List.hd ts = l) || failwith "ending division does not match start" in
        current_scope := List.tl ts 
  )
  | _ -> failwith "bad format: treat_section_preamble"


let section_preamble = 
  commit "preamble" section_tag
    ((section_tag ++ possibly label ++ period) >> treat_section_preamble)

(* namespace XX NOT_IMPLEMENTED *)

let namespace = failparse

(* instructions *)

let instruct_commands = ref []
let instruct_strings = ref []
let instruct_bools = ref []
let instruct_ints = ref []

(* For now the parser stores instructions without any action *)

let put_commands = function 
  | WORD (_,w) -> (instruct_commands := (w :: !instruct_commands) )
  | _ -> ()

let put_strings = function 
  | (WORD (_,w),STRING s) -> (instruct_strings := ((w,s) :: !instruct_strings) )
  | _ -> ()

let put_bools = function 
  | (WORD (_,w),b) -> (instruct_bools := ((w,b) :: !instruct_bools) )
  | _ -> ()

let put_ints = function 
  | (WORD (_,w),i) -> (instruct_ints := ((w,i) :: !instruct_ints) )
  | _ -> ()


let instruct_keyword_command = someword "exit"
let instruct_keyword_int = someword "timelimit"
let instruct_keyword_bool = someword "printgoal dump ontored"
let instruct_keyword_string = someword "read library error warning"
let instruct_command = bracket instruct_keyword_command >> put_commands

let integer = someX (function | INTEGER x -> true,int_of_string x | _ -> false,0)

let instruct_int = 
  bracket(instruct_keyword_int ++ integer) >> put_ints

let bool_tf = (lit_true >> (fun _ -> true)) ||| 
                (lit_false >> (fun _ -> false))

let instruct_bool = 
  bracket(instruct_keyword_bool ++ bool_tf) >> put_bools

let string = some (function | STRING _ -> true | _ -> false)

let instruct_string = 
  bracket(instruct_keyword_string ++ string) >> put_strings

(* We depart slightly from the grammar to make SLASHDASH easier to process *)

let rec expand_slashdash =
  function
  | [] -> []
  | WORD (_,w2) :: SLASHDASH :: WORD (_,w2') :: ts -> 
      (w2 :: (w2^w2') :: expand_slashdash ts)
  | WORD (_,w) :: ts -> w :: expand_slashdash ts
  | _ -> failwith "in synonyms, /- must fall between words"

(* XX need to implement multiword synonyms syntoken -> many syntoken *)

let synlist = 
  let syntoken = someX (function 
                  | SLASHDASH -> true,SLASHDASH 
                  | WORD _ as wd -> true,wd
                  | VAR v as var -> (
                    let u = String.uppercase_ascii v in 
                    let isword = String.length u = 1 && 'A' <= u.[0] && u.[0] <= 'Z' in
                    if isword then true,WORD(v,u) else false,var) 
                  | SLASH -> false,SLASH
                  | _ -> failwith "illegal token in synonym list") in
    listof syntoken (a SLASH) >> expand_slashdash 

let instruct_synonym = bracket(word "synonyms" ++ synlist) >> 
                         (fun (_,ls) -> syn_add ls)

let instruction = 
commit "instruction" (a L_BRACK)
(instruct_synonym |||
instruct_command |||
instruct_string |||
instruct_bool |||
instruct_int)

let synonym_statement = 
  let synhead = possibly (word "we") ++ possibly(word "introduce") ++ word "synonyms" in
  commit "synonym_statement" synhead
    (synhead ++ synlist >> (fun (_,ls) -> syn_add ls))

(* end of instructions *)

(* this_exists *)

type this_adj =
  | ThisUnique
  | ThisCanonical
  | ThisWelldefined
  | ThisWellpropped
  | ThisTotal
  | ThisExhaustive 
  | ThisRecursion
  | ThisExist

let this_directive_adjective = 
  let w a _ = a in
  (word "unique" >> w ThisUnique) |||
    (word "canonical" >> w ThisCanonical) |||
    (word "welldefined" >> w ThisWelldefined) |||
    (word "wellpropped"  >> w ThisWellpropped) |||
    (word "total" >> w ThisTotal) |||
    (word "exhaustive" >> w ThisExhaustive) |||
    (phrase "well defined" >> w ThisWelldefined) |||
    (phrase "well propped" >> w ThisWellpropped) |||
    (the_id "well_propped" >> w ThisWellpropped) |||
    (the_id "well_defined" >> w ThisWelldefined)

let this_directive_right_attr = 
  (phrase "by recursion" >> (fun _ -> ThisRecursion))

let this_directive_verb = 
  (word "exists" ++ possibly(this_directive_right_attr) >>
     fun (_,ts) -> ThisExist :: ts)

let this_directive_pred = 
  (word "is" ++ sep_list(this_directive_right_attr) >> snd) |||
    this_directive_verb 

let this_exists = 
  commit "this_exists" (word "this" ++ someword "exists is")
  (word "this" ++ sep_list(this_directive_pred) >>
     (fun (_,ls) -> List.flatten ls))

let nonbrack = 
  some(
      function
      | L_PAREN | R_PAREN | L_BRACE | R_BRACE | L_BRACK | R_BRACK | PERIOD -> false
      | _ -> true
    )

let exclude p parser input = 
  let (result,rest) = parser input in 
  if p result then raise Noparse else (result,rest)
  
let rec balanced input = 
  (many (
      many(nonbrack) |||
        paren(balanced) |||
        bracket(balanced) |||
        brace(balanced)) >> List.flatten  ) input

type text_deferred = 
  | TextStatement of token list
  | LetAnnotation of token list

(* text *)
type text_token = 
  | Section_preamble of string (* new current section *)
  | Instruction of string (* keyword *)
  | Axiom of string*string * (text_deferred list)*text_deferred  (* kind,label,statements,conclusion *)
  | Definition of string * (text_deferred list)*text_deferred (* label,statements,conclusion *)
  | Theorem of string * (text_deferred list)*text_deferred  (* label, statements,conclusion *)
  | Synonym
  | Macro of text_deferred list (* statements *)
  | Namespace 


(* axiom *)
let then_prefix = possibly (lit_then)

let assumption_prefix = 
  lit_lets ++ lit_assume ++ possibly (word "that")

let assumption = 
  (assumption_prefix ++ balanced ++ (a PERIOD) >> 
     (fun ((_,b),_) -> TextStatement b)) |||
    (balanced ++ (a PERIOD) >> (fun (b,_) -> LetAnnotation b))

let axiom_preamble = 
  lit_axiom ++ possibly (label) ++ (a PERIOD) >>
    (function 
     | ((WORD (_,w),ls),_) -> (w,getlabel ls) 
     | _ -> raise Noparse)

let axiom = 
  commit "axiom" axiom_preamble
  (axiom_preamble ++ many(assumption) ++ then_prefix ++ balanced ++ a(PERIOD) >>
     (fun (((((w,labl),ls),_),st),_) -> Axiom (w,labl,ls,TextStatement st)))
  
(* theorem *) 
let ref_item = sep_list(possibly lit_location ++ label) 

let by_ref = possibly(paren(word "by" ++ ref_item))

let proof_method = 
  let has_that = List.exists (function | WORD(_,w) -> (w = "THAT") | _ -> false) in
  let ex_that = exclude has_that in
  commit "proof method" (someword "contradiction case induction")
  ((word "contradiction" >> out) |||
    (phrase "case analysis" >> out) |||
    (word "induction" ++ possibly (word "on" ++ ex_that balanced) >> out))

let by_method = 
  possibly(word "by" ++ proof_method)

let choose_prefix = 
  then_prefix ++ possibly(lit_lets) ++ lit_choose 

let canned_proof = phrase_list_proof_statement >> out

let canned_prefix = sep_list(phrase_list_transition) ++ possibly(a(COMMA))

let goal_prefix = 
  possibly(lit_lets) ++ lit_prove ++ by_method ++ possibly(word "that")

let proof_preamble = 
  (word "proof" ++ by_method ++ a(PERIOD) >> out) |||
    (word "indeed" >> out)

(* big mutual recursion for proof scripts *)

let rec affirm_proof input = 
  (statement_proof ||| goal_proof) input 

and statement_proof input = 
  (then_prefix ++ balanced ++ by_ref ++ a(PERIOD) ++
    possibly (proof_script) >> 
     (fun ((((_,b),_),_),_) -> b)) input

and goal_proof input = 
  (goal_prefix ++ balanced ++ by_ref ++ a(PERIOD) ++ 
     proof_script >> 
     (fun ((((_,b),_),_),_) -> b)) input

and proof_script input = 
  (proof_preamble ++ 
    many(canned_prefix ++ proof_body ++ canned_prefix ++ proof_tail) ++
    lit_qed ++ a(PERIOD) >> out) input 

and proof_body input = 
  (proof_tail ||| (assumption >> out) >> out) input 

and proof_tail input = 
  ((affirm_proof >> out) ||| canned_proof ||| case ||| choose >> out) input

and case input = 
  (word "case" ++ balanced ++ a(PERIOD) ++ proof_script >> out) input 

and choose input =
  (choose_prefix ++ balanced ++ a(PERIOD) ++ choose_justify >> out) input 

and choose_justify input = 
  (possibly(proof_script) >> out) input;;

(* end big recursion *)

let theorem_preamble = 
  (word "theorem" ++ possibly(label) ++ a(PERIOD) >>
     (fun ((_,ls),_) -> getlabel ls))

let theorem = 
  commit "theorem" theorem_preamble 
  (theorem_preamble ++ many(assumption) ++ affirm_proof >>
    (fun ((l,ls),st) -> Theorem(l,ls,TextStatement st)))

(* definitions *)

let copula = 
  (lit_is ++ possibly(lit_defined_as) >> out) |||
    (a(ASSIGN) >> out) |||
    (lit_denote >> out)

let iff_junction = lit_iff 

let opt_say = possibly(lit_we_say)

let opt_record = possibly(lit_we_record) >> out 

let opt_define = 
  (possibly(lit_lets) ++ possibly(word "define") >> out) |||
    (opt_record)

let macro_inferring = 
  paren(word "inferring" ++ nonempty_list(var) ++ opt_colon_balanced)

let class_words = comma_nonempty_list(word)

let classifier_def = 
  word "let" ++ class_words ++ lit_is ++ possibly(lit_a) ++ lit_classifier





(* variables *)

let colon = a COLON 

let annotated_var = paren(var )
