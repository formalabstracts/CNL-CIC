(*
 Combinator Parser for CNL
 Thomas Hales, October 2019
 

 *)

(* terms, typ, and props  - type declaration 
XX expand as we go.
Primes for still unparsed/unprocessed material.
 *)

open Cnl_parse.Lexer_cnl

type term = 
  | TVar of string*(typ option)
  | TVarAtomic of string*(typ option)
  | Annotated of term*typ 
  | Decimal of string
  | Error'
  | Integer of int (* XX should be BigInt *)
  | String of string
  | Blank
  | Id of string* typ option
  | Unparsed' of token list
  | ControlSeq of string*expr list
  | Make of (token * typ * token list) list 
  | Plain' of token list
  | List of term list
  | Tuple of term list
  | SetEnum of term list
  | Case of (prop*term) list
  | Match of (term list) * (term list * term) list
  | MatchFunction of (token list list) * (token * typ) list * typ * (term list * term) list
  | Comprehension of term * term list * statement
  | FieldAccessor of term * token 
  | ApplySub of term * term list

and typ = 
  | TyVar of token
  | Colon' of token list 
  | Type' of token list
  | TyControlSeq of token * token list list

and prop = 
  | PVar of token
  | Prop' of token list

and statement = 
  | Statement' of token list 
  | LetAnnotation' of token list

and proof = 
  | Proof

and expr = 
  | Eterm of term
  | Etyp of typ
  | Eprop of prop
  | Expr' of token list

type associativity = 
  | AssocLeft
  | AssocRight
  | AssocNone

type wordpattern = 
  | Wp_wd of token
  | Wp_sym of token 
  | Wp_syn of token list 
  | Wp_opt of token 
  | Wp_var of token* typ
  | Wp_fix of token* typ
  | Wp_ty_word of wordpattern list
  | Wp_bin_cs of wordpattern*(token*(wordpattern list))*wordpattern
  | Wp_ty_identifier of token* token list list * ((token * typ) list)
  | Wp_ty_cs of token * wordpattern list
  | Wp_cs of token * wordpattern list
  | Wp_sympat of wordpattern list * int option  * associativity
  | Wp_sympatP of wordpattern list * int option  * associativity
  | Wp_sympatT of wordpattern list
  | Wp_identifier of token* token list list * ((token * typ) list)
  | Wp_identifierP of token* token list list * ((token * typ) list)
  | Wp_fun_word of wordpattern list
  | Wp_adj of wordpattern list
  | Wp_adjM of wordpattern list
  | Wp_verb of wordpattern list
  | Wp_verbM of wordpattern list
  | Wp_inferring of wordpattern * token list * typ
  | Wp_classifier of token list list
  | Wp_record of token list list

type this_adj =
  | ThisUnique
  | ThisCanonical
  | ThisWelldefined
  | ThisWellpropped
  | ThisTotal
  | ThisExhaustive 
  | ThisRecursion
  | ThisExist

type scope = string list

type prim = 

  | Prim_classifier of scope * string (* phrase *)
  (* all cs primitives are 0-ary or 2-ary *)
  (* cs token, braceargs, precedence, assoc, def, free vars  -- always binary *)
  | Prim_term_op_controlseq of scope * token * int * int * associativity * term * term list
  (* cs token, braceargs, def, frees *)
  | Prim_binary_relation_controlseq of scope * token * int * prop * term * term list
  (* cs token, braceargs, prec, assoc, def, frees *)
  | Prim_propositional_op_controlseq of 
      scope * token * int * int * associativity * prop * term list 
  (* cs token, braceargs, def, frees -- alway binary, fixed prec, right assoc. *)
  | Prim_type_op_controlseq of scope * token * int * term * term list
  (* cs token, brace args, def, frees -- only brace args *)
  | Prim_term_controlseq of scope * token * int * term * term list 
  (* cs token, brace args, def, frees *)
  | Prim_type_controlseq of scope * token * int * typ * expr list 
  (* token, def -- binders *)
  | Prim_lambda_binder of scope * token * term 
  | Prim_pi_binder of scope * token * typ 
  | Prim_binder_prop of scope * token * prop 

  (* -- non cs *)
  (* pattern, def, frees *)                             
  | Prim_typed_name of scope * wordpattern * typ * expr list 
  | Prim_adjective of scope * wordpattern * prop * expr list
  | Prim_adjective_multisubject of scope * wordpattern * prop * expr list
  | Prim_simple_adjective of scope * wordpattern * prop * expr list
  | Prim_simple_adjective_multisubject of scope * wordpattern * prop * expr list
  | Prim_definite_noun of scope * wordpattern * term * expr list
  | Prim_identifier_term of scope * token * term * expr list
  | Prim_identifier_type of scope * token * typ * expr list
  | Prim_possessed_noun of scope * wordpattern * term * expr list
  | Prim_verb of scope * wordpattern * prop * expr list 
  | Prim_verb_multisubject of scope * wordpattern * prop * expr list
  | Prim_structure of scope * wordpattern * typ * expr list 
  | Prim_type_op of scope * token * typ * typ list
  | Prim_term_op of scope * wordpattern * term * expr list
  | Prim_binary_relation_op of scope * token * prop * (term * term)
  | Prim_propositional_op of scope * token * int * associativity * prop * prop list 
  | Prim_relation of scope * wordpattern * typ * term list 

type text_token = 
  | Section_preamble of string (* new current section *)
  | Instruction of string (* keyword *)
  | Axiom of string*string * (statement list)*statement  (* kind,label,statements,conclusion *)
  | Definition of string * (statement list)*statement (* label,statements,conclusion *)
  | Theorem of string * (statement list)*statement  (* label, statements,conclusion *)
  | Synonym
  | Macro of statement list (* statements *)
  | Namespace 

(*
type parses = 
  {
    ptm : token list -> term;
    pty : token list -> typ;
    ppr : token list -> prop
  }
 *)

(* XX do recursion as follows:
let rec parse_term = parse_term' 
    { ptm := parse_term; pty := parse_type; ppr := parse_prop }
and parse_type = parse_type' { etc. }
and parse_prop = parse_prop' { etc. }
 *)




(* a few lines from HOL Light lib.ml *)

let report s =
  Format.print_string s; Format.print_newline();;

let warn cond s =
  if cond then report ("Warning: "^s) else ();;

let curry f x y = f(x,y);;

let uncurry f(x,y) = f x y;;

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

let (>>) prs treatment input =
  let result,rest = prs input in
  treatment(result),rest;;

let rec many prs input =
  try let result,next = prs input in
      let results,rest = many prs next in
      (result::results),rest
  with Noparse -> [],input;;

let fix err prs input =
  try prs input
  with Noparse -> failwith (err ^ " expected");;

let rec separated_list prs sep =
  prs ++ many (sep ++ prs >> snd) >> (fun (h,t) -> h::t);;

let nothing input = [],input;;

let unit input = ((),input);;

let eseparated_list prs sep =
  separated_list prs sep ||| nothing;;

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

let rec atleast n prs input =
  (if n <= 0 then many prs
   else prs ++ atleast (n - 1) prs >> (fun (h,t) -> h::t)) input;;

let finished input =
  if input = [] then (),input else failwith "Unparsed input";;

(* end of HOL Light *)

let pair x y = (x,y)

let string_sort = List.sort_uniq String.compare

let _ = string_sort ["the";"THE";"that";"a";"z"]

let consume parser input = 
  let ((a,_),_) = (parser ++ finished) input in a

let plus prs = atleast 1 prs

let discard _ = ();;

let failparse _ = raise Noparse

let canparse f x = try (ignore(f x); true) with Noparse -> false

let must f parser input = 
  let (r,rest) = parser input in 
  let _ = f r || raise Noparse in
  (r,rest)

(*
let ( <|> ) (test,parser1) parser2 input = 
  if canparse test input then 
    parser1 input 
  else parser2 input
 *)
 (* commitment test *)

let commit err test parse input = 
  if canparse test input then 
    fix err parse input  
  else parse input 

let commit_head err parse1 parse2 input = 
  try(  
    let (result,rest) = parse1 input in 
    fix err (parse2 (pair result)) rest)
  with Noparse -> parse2 failparse input

let followedby parse input =
  let (_,_) = parse input in 
  (),input 

 (* accumulate parse1s until parse2 succeeds *)
let rec until parse1 parse2 input = 
  try parse2 input 
  with Noparse -> 
         let result,next = parse1 input in
         let (r1,r2),rest = until parse1 parse2 next in
         (result :: r1, r2),rest

(* parentheses *)

let paren parser  = 
  (a L_PAREN ++ parser ++ a R_PAREN) >> (fun ((_,p),_) -> p) 

let bracket parser = 
  (a L_BRACK ++ parser ++ a R_BRACK) >> (fun ((_,p),_) -> p) 

let brace parser = 
  (a L_BRACE ++ parser ++ a R_BRACE) >> (fun ((_,p),_) -> p) 

 (*
   let brace_semi parser =
   brace(separated_list parser (a SEMI))
  *)

let opt_paren parser = 
  paren parser ||| parser

let nonbrack accept = 
  some(
      function
      | L_PAREN | R_PAREN | L_BRACE | R_BRACE | L_BRACK | R_BRACK | PERIOD -> false
      | t -> accept t
    )

let rec balancedB accept input = 
  (many (
      many(nonbrack accept) |||
        paren(balanced) |||
        bracket(balanced) |||
        brace(balanced)) >> List.flatten  ) input
and balanced input = balancedB (fun _ -> true) input

(*
let separated p = 
  let sep p =
    nonbrack p >> (fun x -> [x]) ||| 
      paren(balanced) ||| bracket(balanced) ||| brace(balanced) in
  (many(sep p) >> List.flatten)
 *)

let brace_semi = 
  let semisep = balancedB (function | SEMI -> false | _ -> true) in
  brace(semisep ++ many(a(SEMI) ++ semisep)) >>
    (fun (a,bs) -> a :: (List.map snd bs))

let comma = a COMMA 

let comma_nonempty_list parser = 
  separated_list parser comma 

let comma_list err parser = 
  eseparated_list err parser comma

let opt_comma_nonempty_list parser = 
  separated_list parser (possibly comma) 

(* let sep_list below *)

let lit_binder_comma = comma

let cs_brace parser1 parser2 = 
  parser1 ++ many (brace parser2) 

(* set up synonym hashtable *)
 (* XX need to add multiword synonyms *)

let synonym = Hashtbl.create 200;;

let syn_add1 (key,value) = 
  if Hashtbl.mem synonym key then 
    let value' = Hashtbl.find synonym key in 
    failwith ("synonym already declared "^key^" "^value')
  else
    Hashtbl.add synonym key value

let syn_add ts = 
  let ts' = string_sort (List.map String.uppercase_ascii ts) in
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
  
let rec somecomb parser = 
  function
  | [] -> failparse 
  | t :: ts -> parser t ||| somecomb parser ts


(* words *)
let word s = 
  let u = find_syn (String.uppercase_ascii s) in 
  someX (function 
      | WORD (w,wu) -> 
          let wsyn = find_syn wu in
          ((wsyn = u),WORD (w,wsyn))
      | _ -> (false,UNKNOWN""))

let anyword = some(function | WORD _ -> true | _ -> false)

let anyphrase = plus(anyword) 

let phrase s = 
  let ps = List.map word (String.split_on_char ' ' s) in 
  parse_all ps

let someword s = 
  let s' = List.map word (String.split_on_char ' ' s) in
  parse_some s'


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

let sep_list parser = separated_list parser sep_and_comma

let case_sensitive_word = 
  someX (function 
      | WORD (w,_) -> true,(ATOMIC_IDENTIFIER w)
      | x -> (false,x))

let the_case_sensitive_word s = 
  someX (function 
      | WORD (w,_) -> (w=s),(ATOMIC_IDENTIFIER w)
      | x -> (false,x))

let atomic_identifier =
  some (function 
      | ATOMIC_IDENTIFIER _ -> true
      | _ -> false)
    
let atomic =
  case_sensitive_word ||| atomic_identifier 

let field_accessor = 
  some (function
      | FIELD_ACCESSOR _ -> true
      | _ -> false)

let var = some (function | VAR _ -> true | _ -> false)

let var_or_atomic = var ||| atomic

let var_or_atomics = plus(var_or_atomic)

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

let inscope scope = 
  let curscope = !current_scope in
  let is = List.length scope in
  let ic = List.length curscope in 
  if is > ic then false
  else 
    let curscope' = snd(chop_list (ic - is) curscope) in 
    (scope = curscope')

let section_preamble = 
  commit_head "preamble" section_tag
    (fun t -> ((t ++ possibly label ++ period) >> treat_section_preamble))

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
    separated_list syntoken (a SLASH) >> expand_slashdash 

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
  let head = possibly (word "we") ++ possibly(word "introduce") ++ word "synonyms" in
  commit_head "synonym_statement" head
    (fun head -> (head ++ synlist >> (fun (_,ls) -> syn_add ls)))

(* end of instructions *)

let stored_string = 
  function
  | STRING s -> s
  | CONTROLSEQ s -> s
  | DECIMAL s -> s
  | INTEGER s -> s
  | SYMBOL s -> s
  | QUANTIFIER s -> s
  | VAR s -> s
  | ATOMIC_IDENTIFIER s -> s
  | HIERARCHICAL_IDENTIFIER s -> s
  | FIELD_ACCESSOR s -> s
  | WORD (_,s) -> s
  | _ -> warn true "stored string token expected"; ""


(* this_exists *)


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

 (* doesn't work properly
 let exclude p parser = 
  function
  | [] -> parser []
  | t :: ts as input -> if p t then raise Noparse else parser input 
  let (result,rest) = parser input in 
  if p result then raise Noparse else (result,rest)
  *)
  


(* text *)

(* axiom *)
let then_prefix = possibly (lit_then)

let assumption_prefix = 
  lit_lets ++ lit_assume ++ possibly (word "that")

let assumption = 
  (assumption_prefix ++ balanced ++ (a PERIOD) >> 
     (fun ((_,b),_) -> Statement' b)) |||
    (balanced ++ (a PERIOD) >> (fun (b,_) -> LetAnnotation' b))

let axiom_preamble = 
  lit_axiom ++ possibly (label) ++ (a PERIOD) >>
    (function 
     | ((WORD (_,w),ls),_) -> (w,getlabel ls) 
     | _ -> raise Noparse)

let axiom = 
  commit_head "axiom" axiom_preamble
  (fun head -> (head ++ many(assumption) ++ then_prefix ++ balanced ++ a(PERIOD) >>
     (fun (((((w,labl),ls),_),st),_) -> Axiom (w,labl,ls,Statement' st))))
  
(* theorem *) 
let ref_item = sep_list(possibly lit_location ++ label) 

let by_ref = possibly(paren(word "by" ++ ref_item))

let by_method = 
  (* exclude 'that' to avoid grammar ambiguity in goal_prefix *)
  let except = (function | WORD(_,w) -> not(w = "THAT") | _ -> true) in
  commit_head "proof method" (word "by")
  (fun head -> (head ++ 
    ((word "contradiction" >> discard) |||
    (phrase "case analysis" >> discard) |||
    (word "induction" ++ possibly (word "on" ++ balancedB except) >> discard)) ++
  followedby ((word "that" ||| a(PERIOD))) >> discard))

let choose_prefix = 
  then_prefix ++ possibly(lit_lets) ++ lit_choose 

let canned_proof = phrase_list_proof_statement >> discard

let canned_prefix = sep_list(phrase_list_transition) ++ possibly(a(COMMA))

let goal_prefix = 
  possibly(lit_lets) ++ lit_prove ++ 
    (word "that" >> discard ||| (possibly(by_method ++ word "that") >> discard))

let proof_preamble = 
  (word "proof" ++ possibly(by_method) ++ a(PERIOD) >> discard) |||
    (word "indeed" >> discard)

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
    lit_qed ++ a(PERIOD) >> discard) input 

and proof_body input = 
  (proof_tail ||| (assumption >> discard) >> discard) input 

and proof_tail input = 
  ((affirm_proof >> discard) ||| canned_proof ||| case ||| choose >> discard) input

and case input = 
  (word "case" ++ balanced ++ a(PERIOD) ++ proof_script >> discard) input 

and choose input =
  (choose_prefix ++ balanced ++ a(PERIOD) ++ choose_justify >> discard) input 

and choose_justify input = 
  (possibly(proof_script) >> discard) input;;

(* end big recursion *)

let theorem_preamble = 
  (word "theorem" ++ possibly(label) ++ a(PERIOD) >>
     (fun ((_,ls),_) -> getlabel ls))

let theorem = 
  commit_head "theorem" theorem_preamble 
  (fun head -> (head ++ many(assumption) ++ affirm_proof >>
    (fun ((l,ls),st) -> Theorem(l,ls,Statement' st))))


(* patterns *)
 (* XX need to process multiword synonyms *)

let pattern_banned = 
  ["IS";"BE";"ARE";"DENOTE";"STAND";"IF";"IFF";"THE";"A";"AN";
   "SAID";"DEFINED";"OR";"AND";"INFERRING"]

let not_banned =
  let p s = not(List.mem s pattern_banned) in
  function
  | WORD(_,s) -> p s
  | VAR s -> p s
  | ATOMIC_IDENTIFIER s -> p s
  | HIERARCHICAL_IDENTIFIER s -> p s
  | _ -> true

let any_pattern_word = 
  some(function 
      | WORD (_,_) as w  -> not_banned w
      | _ -> false)

let word_in_pattern = 
  any_pattern_word >> (fun s -> [Wp_wd s]) |||   
  (paren(comma_nonempty_list(word "or" ++ plus(any_pattern_word))) 
  >> (fun ss -> 
            let ss' = List.map snd ss in 
             List.map (fun s' -> Wp_syn s') ss')) |||
  (paren(any_pattern_word) >> (fun s -> [Wp_opt s]))

let words_in_pattern = 
  (any_pattern_word ++ many(word_in_pattern)) 
  >> (fun (a,b) -> Wp_wd a :: List.flatten b)

let post_colon_balanced = 
  balancedB (* true tokens can follow opt_colon_type *)
    (function | ASSIGN | SEMI | COMMA | ALT | COLON -> false 
              | WORD (_,s) -> not(s = "END") && not(s = "WITH")
              | _ -> true)

let opt_colon_type = 
  possibly(a(COLON) ++ post_colon_balanced) >> 
    (fun bs -> Colon' (List.flatten (List.map snd bs)))
                         
let tvarpat = 
  paren(var ++ opt_colon_type) >>
    (fun (v,bs) -> Wp_var (v,bs))

let word_pattern = 
  words_in_pattern ++ many(tvarpat ++ words_in_pattern) ++ possibly(tvarpat)
   >>
     fun ((a,bs),c) ->
           a @ (List.flatten (List.map (fun (b,b')-> b::b') bs)) @ c

let type_word_pattern = possibly(lit_a) ++ word_pattern >> (fun (_,bs) -> Wp_ty_word bs)

let function_word_pattern = 
  word("the") ++ word_pattern >> 
    (fun (_,w) -> Wp_fun_word w)
                              
let adjective_pattern = 
  tvarpat ++ word "is" ++ possibly (word "called") ++ word_pattern
  >> (fun (((a,_),_),b) -> Wp_adj (a ::b)) 

let var_multisubject_pat = 
  tvarpat ++ a(COMMA) ++ tvarpat >> (fun ((a,_),c) -> a,c) |||
    (paren(var ++ a(COMMA) ++ var ++ opt_colon_type)
     >>
       (fun (((v,_),v'),bs) -> 
              (Wp_var(v,bs),Wp_var(v',bs)))
    )

let adjective_multisubject_pattern =
  var_multisubject_pat ++ word "are" ++ possibly(word "called") ++ word_pattern 
  >> (fun ((((v,v'),_),_),w) -> Wp_adjM (v :: v' ::w))

let verb_pattern = 
  tvarpat ++ word_pattern 
  >> fun (a,b) -> Wp_verb (a::b)

let verb_multisubject_pattern = 
  var_multisubject_pat ++ word_pattern 
  >> fun ((a,a'),b) -> Wp_verbM (a::a'::b)

let predicate_word_pattern =
  adjective_pattern |||
    adjective_multisubject_pattern |||
    verb_pattern |||
    verb_multisubject_pattern

let opt_args_pat = 
  brace_semi

 (* XX When we distribute an empty type, we should require that
   the types are all the same through a metavariable.  *)

let required_arg_pat = 
  paren(var_or_atomics ++ opt_colon_type) >>
    (fun (vs,bs) -> List.map (fun v -> v,bs) vs)
        |||
    (must not_banned var_or_atomic  >> fun a -> [a,Colon' []])

let args = 
  (possibly opt_args_pat >> List.flatten) ++ (many(required_arg_pat) >> List.flatten)

let identifier_pattern = 
  (must not_banned identifier ||| a(BLANK)) ++
    args

let controlseq = some(function | CONTROLSEQ _ -> true | _ -> false)

let controlseq_pattern = 
  controlseq ++ many(brace(tvarpat))

let binary_controlseq_pattern = 
  tvarpat ++ controlseq_pattern ++ tvarpat >>
    (fun ((a,b),c) -> Wp_bin_cs (a,b,c))

let symbol = 
  some(function | SLASH | SLASHDASH -> true | SYMBOL _ -> true | _ -> false) 
  >> (fun a -> Wp_sym a) |||
    (controlseq_pattern >> (fun (t,w) -> Wp_cs (t,w)))

let precedence_level = 
  let assoc = 
    function 
    | WORD (_,s) ->
        (match s with 
         | "LEFT" -> AssocLeft 
         | "RIGHT" -> AssocRight 
         | _ -> AssocNone)
    | _ -> AssocNone in
  word "with" ++ word "precedence" ++ integer ++
    possibly(word "and" ++ lit_left ++ word "associativity") >>
    (fun (((_,_),i),bs) -> 
           let a = (function 
             | [] -> AssocNone
             | ((_,l),_) :: _ -> assoc l) bs in 
           (i,a))

let paren_precedence_level = 
  precedence_level ||| paren precedence_level

let symbol_pattern = 
  possibly(tvarpat) ++ symbol ++
    many(tvarpat ++ symbol) ++ possibly(tvarpat) ++ possibly(paren_precedence_level) >>
    (fun ((((a,b),cs),ds),e) -> 
           let cs' = List.flatten (List.map (fun (c,c') -> [c;c']) cs) in
           let e' = (function 
                     | [] -> (None,AssocNone)
                     | (i,a) :: _ -> (Some i,a)) e in
           ((a @ (b :: cs' @ ds)),fst e',snd e'))



(* pattern end *)

(* macro *)

let insection = 
  commit_head "macro" (phrase "in this")
    (fun head -> (head ++ lit_document 
     >> 
       (function | (_,WORD(_,s)) -> 
                     (let i = scope_level s in
                      let subdivision = (i>3) in
                      if subdivision 
                      then max 0 (List.length !current_scope - 1)
                      else i)
                 | _ -> 0)
    ))


let we_record_def = 
  lit_we_record ++ 
    comma_nonempty_list 
      (balancedB (function | COMMA | SEMI | PERIOD -> false | _ -> true)) >> snd

let annotated_var = paren(var ++ opt_colon_type)

let annotated_vars = paren(plus(var) ++ opt_colon_type) >>
                       (fun (vs,o) -> List.map (fun v -> Wp_var(v,o)) vs)

let let_annotation_prefix = 
  (word("let") ++ comma_nonempty_list(var) ++ word "be" ++ possibly(lit_a)) >>
    fun (((_,a),_),_) -> a

let fix_var fix v = if fix then 
                      (match v with
                       | Wp_var(v,o) -> Wp_fix(v,o)
                       | _ -> v)
                    else v

let let_annotation = 
  ((word "fix" >> (fun _ -> true) ||| (word "let" >> (fun _ -> false))) ++ 
    comma_nonempty_list(annotated_vars) 
   >>
     (fun (t,bs) -> List.map (fun b -> (fix_var t b,[])) (List.flatten bs))
     )
  |||
    ((let_annotation_prefix ++ post_colon_balanced)
     >>
       fun (vs,b) -> List.map (fun v -> (Wp_var (v,Colon' b),[])) vs 
    )

(* macro end *)

(* definitions *)

let copula = 
  (lit_is ++ possibly(lit_defined_as) >> discard) |||
    (a(ASSIGN) >> discard) |||
    (lit_denote >> discard)

let function_copula =
  copula >> (fun _ -> Colon' []) |||
    (opt_colon_type ++ a(ASSIGN) >> fst)

let iff_junction = lit_iff 

let opt_say = possibly(lit_we_say)

let opt_record = possibly(lit_we_record) >> discard 

let opt_define = 
  (possibly(lit_lets) ++ possibly(word "define") >> discard) |||
    (opt_record)

let macro_inferring = 
  paren(word "inferring" ++ plus(var) ++ opt_colon_type)
  >> (fun ((_,a),b) -> (a,b))

let class_words = comma_nonempty_list(anyphrase)

 (* XX need to add prim actions *)
let classifier_def = 
  word "let" ++ class_words ++ lit_is ++ possibly(lit_a) ++ lit_classifier 
  >> (fun ((((_,c),_),_),_) -> c)

let type_head = 
  type_word_pattern |||
    (symbol_pattern >> (fun (a,_,_) -> Wp_sympatT (a))) |||
    (identifier_pattern >> (fun (a,(b,c)) -> Wp_ty_identifier (a,b,c))) |||
    (controlseq_pattern >> (fun (a,b) -> Wp_ty_cs (a,b)))

let type_def = 
  (opt_define ++ type_head ++ 
      ((a(COLON) ++ the_case_sensitive_word "Type" ++ 
         copula ++ possibly(lit_a) >> discard) ||| 
         (copula ++ word "the" ++ lit_type >> discard)) ++
      balanced ++ followedby (a(PERIOD))) >>
    (fun ((((_,a),_),b),_) -> (a,b))

let function_head = 
  (function_word_pattern |||
    (symbol_pattern >> (fun (a,b,c) -> Wp_sympat (a,b,c))) ||| 
       (identifier_pattern >> (fun (a,(b,c)) -> Wp_identifier (a,b,c))))

let function_def = 
  (opt_define ++ function_head ++ possibly(macro_inferring) ++
     copula ++ possibly(lit_equal) ++ possibly(word "the") ++ 
     balanced ++ followedby (a(PERIOD))) >>
    (fun (((((((_,h),m),_),_),_),b),_) -> 
           let h' = if m=[] then h 
                    else let (i,i')= List.hd m in Wp_inferring(h,i,i')
           in (h',b))

let predicate_head = 
  predicate_word_pattern |||
  (symbol_pattern >> (fun (a,b,c) -> Wp_sympatP (a,b,c))) |||
    (identifier_pattern >> (fun (a,(b,c)) -> Wp_identifierP (a,b,c)))

let predicate_def = 
  opt_say ++ predicate_head ++ possibly(macro_inferring) ++ iff_junction
  ++ balanced ++ followedby (a(PERIOD)) 
  >>
    (fun (((((_,h),m),_),b),_) -> 
           let h' = if m=[] then h 
                    else let (i,i')= List.hd m in Wp_inferring(h,i,i')
           in (h',b)
    )

let macro_body = 
  (type_def 
   ||| function_def 
   ||| (we_record_def 
        >> (fun a -> Wp_record a,[])
       ) 
   ||| predicate_def
  )  
  >> (fun a -> [a])  
  ||| (classifier_def 
       >> (fun a -> [Wp_classifier a,[]])
      ) 
  ||| let_annotation

let macro_bodies = 
  separated_list macro_body (a(SEMI) ++ possibly(word "and")) ++ a(PERIOD) 
  >> fst

let macro = 
  possibly(insection) ++ macro_bodies 
  >>
    fun (i,m) -> 
          let sec_level = (match i with | [] -> 0 | i'::_ -> i') in
          sec_level,List.flatten m
    
(* end macro *)                 

(* primitives *)

let prim_tbl = Hashtbl.create 1000 (* global table *)

(* let prim_add (key,value) =  *)
  







let prim_scope = 
  function 
  | Prim_classifier (scope,_) -> scope 
  | Prim_term_op_controlseq (scope,_,_,_,_,_,_) -> scope
  | Prim_binary_relation_controlseq (scope,_,_,_,_,_) -> scope
  | Prim_propositional_op_controlseq (scope,_,_,_,_,_,_ ) -> scope
  | Prim_type_op_controlseq (scope,_,_,_,_) -> scope
  | Prim_term_controlseq (scope,_,_,_,_ ) -> scope
  | Prim_type_controlseq (scope,_,_,_,_ ) -> scope
  | Prim_lambda_binder (scope,_,_ ) -> scope
  | Prim_pi_binder (scope,_,_ ) -> scope
  | Prim_binder_prop (scope,_,_ ) -> scope
  | Prim_typed_name (scope,_,_,_ ) -> scope
  | Prim_adjective (scope,_,_,_) -> scope
  | Prim_adjective_multisubject (scope,_,_,_) -> scope
  | Prim_simple_adjective (scope,_,_,_) -> scope
  | Prim_simple_adjective_multisubject (scope,_,_,_) -> scope
  | Prim_definite_noun (scope,_,_,_) -> scope
  | Prim_identifier_term (scope,_,_,_) -> scope
  | Prim_identifier_type (scope,_,_,_) -> scope
  | Prim_possessed_noun (scope,_,_,_) -> scope
  | Prim_verb (scope,_,_,_ ) -> scope
  | Prim_verb_multisubject (scope,_,_,_) -> scope
  | Prim_structure (scope,_,_,_ ) -> scope
  | Prim_type_op (scope,_,_,_) -> scope
  | Prim_term_op (scope,_,_,_) -> scope
  | Prim_binary_relation_op (scope,_,_,_) -> scope
  | Prim_propositional_op (scope,_,_,_,_,_ ) -> scope
  | Prim_relation (scope,_,_,_ ) -> scope

let prim_find_inscope tbl key = 
  let vs = Hashtbl.find_all tbl key in
  List.filter (fun v -> inscope (prim_scope v)) vs

let prim_add tbl (key,value) = 
  warn (not (prim_find_inscope tbl key = []))
    ("primitive already declared: "^key); 
    Hashtbl.add tbl key value

let prim_token = function
  | Prim_term_op_controlseq (_,token,_,_,_,_,_) -> token
  | Prim_binary_relation_controlseq (_,token,_,_,_,_) -> token
  | Prim_propositional_op_controlseq (_,token,_,_,_,_,_ ) -> token
  | Prim_type_op_controlseq (_,token,_,_,_) -> token
  | Prim_term_controlseq (_,token,_,_,_ ) -> token
  | Prim_type_controlseq (_,token,_,_,_ ) -> token
  | Prim_lambda_binder (_,token,_ ) -> token
  | Prim_pi_binder (_,token,_ ) -> token
  | Prim_binder_prop (_,token,_ ) -> token
  | Prim_identifier_term (_,token,_,_) -> token
  | Prim_identifier_type (_,token,_,_) -> token
  | Prim_type_op (_,token,_,_) -> token
  | Prim_binary_relation_op (_,token,_,_) -> token
  | Prim_propositional_op (_,token,_,_,_,_ ) -> token
  | _ -> failwith "prim_token: token expected" 

let prim_wordpattern = function 
  | Prim_typed_name (_,wordpattern,_,_) -> wordpattern
  | Prim_relation (_,wordpattern,_,_ ) -> wordpattern
  | Prim_adjective_multisubject (_,wordpattern,_,_) -> wordpattern
  | Prim_simple_adjective (_,wordpattern,_,_) -> wordpattern
  | Prim_simple_adjective_multisubject (_,wordpattern,_,_) -> wordpattern
  | Prim_definite_noun (_,wordpattern,_,_) -> wordpattern
  | Prim_possessed_noun (_,wordpattern,_,_) -> wordpattern
  | Prim_verb (_,wordpattern,_,_ ) -> wordpattern
  | Prim_verb_multisubject (_,wordpattern,_,_) -> wordpattern
  | Prim_structure (_,wordpattern,_,_ ) -> wordpattern
  | Prim_term_op (_,wordpattern,_,_) -> wordpattern
  | _ -> failwith "prim_wordpattern: wordpattern expected"

let prim_token_in_scope tbl key =
  List.mem key 
  (List.map prim_token (prim_find_inscope tbl key))

(*  key=token for prim_token primitives *)

let prim_identifier_term_tbl = Hashtbl.create 200

let prim_identifier_exists key =
  prim_token_in_scope prim_identifier_term_tbl key

(*  | Prim_type_controlseq of scope * token * int * typ * expr list  *)

let prim_type_controlseq_tbl = Hashtbl.create 100

let prim_type_controlseq_exists key = 
  prim_token_in_scope prim_type_controlseq_tbl key



(* proof_expr *)

let proof_expr = 
  a SYMBOL_QED 
  ||| paren(a SYMBOL_QED) >> (fun _ ->Proof)

(* variables *)

let tvar = 
  var 
  >> (fun v -> TVar (stored_string v,None))
  ||| (annotated_var 
       >> fun (v,ty) -> TVar (stored_string v,Some ty))

let pre_expr = (balancedB (function | COMMA | SEMI | PERIOD -> false | _ -> true))

let assign_expr = a ASSIGN ++ pre_expr >> snd

let record_assign_item = 
  var_or_atomic ++ opt_colon_type ++ assign_expr >>
    (fun ((a,b),c) -> 
           let s = stored_string a in
           (TVarAtomic(s,Some b),Expr' c))

let record_assign_term = 
  brace_semi >> (fun ts -> List.map (consume record_assign_item) ts)

(* tightest terms *)

let var_term = var >> fun v -> TVar(stored_string v,None)

let decimal = someX(function | DECIMAL t -> true,Decimal t  | _ -> false,Error')

let integer_term = integer >> (fun i -> Integer i)

let string_term = someX(function | STRING s -> true, String s | _ -> false,Error')

let blank_term = a BLANK >> (fun _ -> Blank)

 (* XX to do - hierarchical identifiers *)

let prim_identifier_term = 
  someX(fun t -> prim_identifier_exists t,Id(stored_string t,None))

let controlseq1 = someX(function | CONTROLSEQ s -> (true,s) | _ -> (false,""))

let controlseq_term = 
  cs_brace controlseq1 balanced 
  >> (fun (s,bs) -> ControlSeq (s,(List.map (fun b -> Expr' b) bs)))

let paren_term = 
  paren(balanced) >> (fun b -> Unparsed' b)

let annotated_term = 
  paren(post_colon_balanced ++ opt_colon_type)
    >> (fun (t,ty) -> Annotated (Unparsed' t,ty))

let var_or_atomic_or_blank = 
  var_or_atomic ||| a(BLANK)

let make_term = 
  let make_term_item = 
    var_or_atomic_or_blank ++ opt_colon_type ++ 
      possibly(assign_expr)
    >> fun ((v,ty),ae) -> (v,ty,List.flatten ae) in 
  word "make" 
  ++ (brace_semi 
      >> (fun ts -> Make (List.map (consume make_term_item) ts)) )
  >> snd 

let list_term =
  let semisep = balancedB (function | SEMI | COMMA -> false | _ -> true) in
  bracket(separated_list semisep (a(SEMI))) 
  >> (fun ts -> List (List.map (fun t -> Plain' t) ts))

let commasep = balancedB (function | SEMI | COMMA -> false | _ -> true)

let tuple_term = 
  paren(separated_list commasep (a(COMMA))) >>
    (fun ts -> Tuple (List.map (fun t -> Plain' t) ts))
  
let set_enum_term = 
  brace(separated_list commasep (a(COMMA))) >>
    (fun ts -> SetEnum (List.map (fun t -> Plain' t) ts))

let holding_var = (* comma needed for disambiguation *)
  possibly
    (a COMMA ++ paren(word "holding" ++ comma_nonempty_list(var))
     >> (fun (_,(_,vs)) -> List.map (fun v -> TVar(stored_string v,None)) vs)
    ) >> List.flatten

let set_comprehension_term = 
  brace(commasep ++ holding_var ++ a MID ++ balanced) 
  >> (fun (((a,b),_),c) -> Comprehension (Plain' a,b,Statement' c))

let case_term = 
  let alt_case = 
    a(ALT) ++ post_colon_balanced ++ a(ASSIGN) ++ post_colon_balanced in 
  word "case" ++ plus(alt_case) ++ word "end" 
  >> (fun ((_,bs),_) -> Case (List.map (fun (((_,b),_),b') -> (Prop' b,Plain' b')) bs))

let match_term = 
  let csv = separated_list (post_colon_balanced >> (fun s -> Plain' s)) (a COMMA) in
  word "match" ++ csv ++ word "with" 
  ++ plus(a ALT ++ csv ++ a ASSIGN ++ post_colon_balanced
          >> (fun (((_,ss),_),p)-> (ss,Plain' p)))
  ++ word "end"
  >> (fun ((((_,ss),_),ps),_) -> Match (ss,ps))

let match_function = 
  let csv = separated_list (post_colon_balanced >> (fun s -> Plain' s)) (a COMMA) in
  word "function" ++ args ++ opt_colon_type
  ++ plus(a ALT ++ csv ++ a ASSIGN ++ post_colon_balanced
          >> (fun (((_,ss),_),p) -> (ss,Plain' p)))
  ++ word "end"
  >> (fun ((((_,(s,s')),ty),ps),_) -> MatchFunction (s,s',ty,ps))

let tightest_prefix = 
  decimal 
  ||| integer_term
  ||| string_term
  ||| blank_term 
  ||| var_term
  ||| prim_identifier_term
  ||| controlseq_term
  ||| paren_term
  ||| annotated_term
  ||| make_term
  ||| list_term
  ||| tuple_term
  ||| set_enum_term
  ||| set_comprehension_term
  ||| case_term
  ||| match_term
  ||| match_function 

let rec tightest_term input = 
  (tightest_prefix
   ||| (tightest_term ++ field_accessor 
        >> fun (t,f) -> FieldAccessor (t,f)
       )
   ||| (tightest_term ++ a APPLYSUB ++ tightest_terms
       >> (fun ((t,_),ts) -> ApplySub (t,ts))
       )
  ) input

and tightest_terms input = 
  paren(plus(tightest_term)) input

(* tightest types *)

let paren_type = 
  paren(balanced) >> (fun t -> Type' t)

let annotated_type = 
  paren(post_colon_balanced ++ a COLON ++ the_case_sensitive_word "Type") 
  >> (fun ((t,_),_) -> Type' t)

let controlseq_type = 
  let controlseq1 = some(fun t -> prim_type_controlseq_exists t) in
  cs_brace controlseq1 balanced 
  >> fun (t,ts) -> TyControlSeq (t,ts)


