(*
 Combinator Parser for CNL
 Thomas Hales, October 2019
 

 *)

(* terms, typ, and props  - type declaration 
XX expand as we go.
Primes for still unparsed/unprocessed material.
 *)

open Lexer_cnl
open Type
open Lib
open Primitive

let rec get_trace_data =
  function 
  | TrMany t -> List.flatten (List.map get_trace_data t)
  | TrAdd t -> List.flatten (List.map get_trace_data t)
  | TrOr [] -> []
  | TrOr t -> get_trace_data (List.hd (List.rev t))
  | TrGroup (_,t) -> get_trace_data t
  | TrFail (_,_,_) -> []
  | TrData t -> t
  | TrString _ -> []
  | TrEmpty -> []

let filter_nonempty = 
  List.filter (fun t -> not (t = TrEmpty))

let endswithplus s = 
  if s = "" then false 
  else s.[String.length s - 1] = '+'

let is_trstring =
  function
  | TrString _ -> true
  | _ -> false

let rec clean_tr ls = 
  let d def ts' = 
    match List.length ts' with 
    | 0 -> TrEmpty 
    | 1 -> clean_tr (List.hd ts')
    | _ -> def in 
  match ls with 
  | TrMany ts -> let ts' = filter_nonempty (List.map clean_tr ts) in 
                 let ts' = 
                   if List.for_all is_trstring ts' 
                   then [TrString (String.concat " " (List.map (function | TrString s -> s | _ -> failwith "trString expected") ts'))]
                   else ts' in 
                 d (TrMany ts') ts'
  | TrAdd ts -> 
      let ts' = filter_nonempty (List.map clean_tr ts) in 
      let ts' = 
        if List.for_all is_trstring ts' 
        then [TrString ("("^(String.concat " ++ " (List.map (function | TrString s -> s | _ -> failwith "trString expected") ts'))^")")]
        else ts' in 
      d (TrAdd ts') ts' 
  | TrOr ts -> let ts' = filter_nonempty (List.map clean_tr ts) in 
                d (TrOr ts') ts' 
  | TrGroup (s,TrGroup(s',t')) -> 
      let sep = if endswithplus s then "" else "/" in
                    clean_tr (TrGroup(s^sep^s',t'))
  | TrGroup (s,TrString s') -> TrString(s^":"^s')
  | TrGroup (s,t) as trg  -> let t' = clean_tr t in 
                     if (t = t') then trg else clean_tr (TrGroup(s,t'))
  | TrData ts -> TrString (string_of_toks ts)
  | TrFail (i,j,tok) -> TrString("unexpected token:'"^string_of_toks [tok]^"' at line="^string_of_int i^" col="^string_of_int j)
  | t -> t

exception Noparse of trace;;

exception Nocatch of trace;;

let trEof =  TrFail (0,0,EOF);;

let failEof = Noparse(trEof)

let getpos = 
  function 
  | [] -> raise failEof
  | t::_ as input -> t.pos,(TrEmpty,input)

let trPos = 
  function (* input *)
    | [] -> trEof
    | n :: _ -> 
        let p = fst(n.pos) in 
        let line = p.pos_lnum in 
        let col = p.pos_cnum - p.pos_bol in 
        (TrFail(line,col,n.tok))

let mergeOr (t,t') = 
  match t with 
  | TrOr t'' -> TrOr (t'' @ [t'])
  | _ -> TrOr [t;t']

let mergeAdd (t,t') = 
  match t with 
  | TrEmpty -> t' 
  | TrAdd t'' -> TrAdd (t'' @ [t'])
  | _ -> TrAdd [t;t']


let pos n = n.pos 

let rec merge_pos = 
  function
  | [t] -> t
  | t :: ts -> let m = merge_pos ts in (fst t,snd m)
  | _ -> failwith "empty merge_pos" 

let pair_pos (a,b) = merge_pos [a;b]

let pair_noparse sep (p1,s1) (p2,s2) =
(pair_pos (p1,p2),s1^sep^s2)

let par x = if x = "" then "" else "("^x^")"

(* let group fm parser input = 
  try 
    parser input 
  with Noparse (p,m) -> 
         let p' = fst(getpos input) in 
         raise (Noparse (pair_pos (p,p'),fm m))
 *)

let group msg parser input  = 
  try 
    let (a,(t,ins)) = parser input in 
    (a,(TrGroup(msg,t),ins)) 
  with 
  | Noparse t -> raise (Noparse (TrGroup(msg,t)))
  | Nocatch t -> raise (Nocatch (TrGroup(msg,t)))




(* Here are a few lines adapted from HOL Light parser.ml *)

let (|||) parser1 parser2 input =
  try parser1 input
  with Noparse t1 -> 
         try 
           parser2 input
         with Noparse t2 -> raise (Noparse (mergeOr(t1,t2)))

let (++) parser1 parser2 input =
  let result1,(t1,rest1) = parser1 input in
  try 
    let result2,(t2,rest2) = parser2 rest1 in
    (result1,result2),(mergeAdd(t1,t2),rest2)
  with | Noparse t2 -> raise(Noparse (mergeAdd(t1,t2)))
       | Nocatch t2 -> raise(Nocatch (TrGroup ("..++",t2)));;

let (>>) prs treatment input =
  let result,rest = prs input in
  treatment(result),rest;;

let rec many prs input =
  try let result,(t,next) = prs input in
      let results,(tr,rest) = many prs next in
      let tr' = 
        match tr with
        | TrMany ts -> TrMany (t :: ts)
        | _ -> failwith "many:unreachable state" in 
      (result::results),(tr',rest)
  with Noparse _ -> [],(TrMany [],input);;

let fix err prs input =
  try prs input
  with | Noparse tr | Nocatch tr -> raise (Nocatch (TrGroup (err,tr)));;

let separated_list prs sep =
  prs ++ many (sep ++ prs >> snd) >> (fun (h,t) -> h::t);;

let nothing input = [],input;;

let unit input = ((),input);;

let empty input = ((),(TrEmpty,input))

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
  with Noparse _ -> [],(TrEmpty,input);;

let some p = 
  function
      [] -> raise failEof
    | (h::t) as ts -> 
        if p h.tok then 
          let tr = TrData [h.tok] in 
          (h,(tr,t)) 
        else raise (Noparse (trPos ts))

let rec atleast n prs input =
  (if n <= 0 then many prs
   else prs ++ atleast (n - 1) prs >> (fun (h,t) -> h::t)) input;;

let finished input =
  if input = [] then (),(TrEmpty,input) else raise (Noparse (trEof))

(* end of HOL Light *)

let (-|) f g x = f(g(x))

let mk (t,p) = { tok = t; pos = p }

let someX p = 
  function
    [] -> raise failEof
  | h :: t -> let (b,x) = p h in
              if b then 
                let tr = TrData [h.tok] in 
                (x,(tr,t)) 
              else raise (Noparse (trPos [h]))

(* let someXt p f = someX p (f -| tok) *)

let a tok input = 
  try 
    some (fun item -> item = tok) input
  with
    Noparse t -> raise (Noparse (TrGroup ("expected:"^string_of_toks [tok],t)))

let pair x y = (x,y)



let consume parser input = 
  let ((a,_),_) = (parser ++ finished) input in a

let plus prs = atleast 1 prs

let discard _ = ();;

let failparse = 
  function 
    [] -> raise failEof 
   | h :: _ -> raise (Noparse (trPos[h]))

let failtr tr _ = raise (Noparse tr)

let canparse f x = try (ignore(f x); true) with Noparse _ -> false

let must f parser input = 
  let (r,(tr,rest)) = parser input in 
  let _ = f r || raise (Noparse (TrGroup("must",trPos input))) in 
  (r,(TrGroup("must",tr),rest))

let commit err test parse input = 
  if canparse test input then 
    fix err parse input  
  else raise (Noparse (TrGroup ("bad header in "^err,trPos input)))

let some_nodeX p = 
  function
    [] -> raise failEof
  | h :: t -> let (b,x) = p h.tok in
              if b then 
                let tr = TrData [h.tok] in 
                (mk (x,h.pos),(tr,t)) 
              else raise (Noparse(trPos[h]))

let rec parse_all ps input = 
  match ps with
  | [] -> ([],(TrMany[],input))
  | p::ps' -> let (result,(t,rest)) = group "parse_all" p input in 
              let (result',(tr,rest')) = parse_all ps' rest in
              let tr' = match tr with
              | TrMany ts -> TrMany (t :: ts)
              | _ -> failwith "many:unreachable state" in 
              (result :: result'),(tr',rest')

let rec parse_some ps input = 
  match ps with 
  | [] -> raise (Noparse (TrGroup ("parse_some",trPos input)))
  | p :: ps' -> try (p input) with Noparse _ -> parse_some ps' input 
  
let rec somecomb parser = 
  function
  | [] -> failparse 
  | t :: ts -> parser t ||| somecomb parser ts



(* words *)

let is_word v = 
  let u = String.lowercase_ascii v in 
  let isword = String.length u = 1 && 'a' <= u.[0] && u.[0] <= 'z' in
  (isword,u)


let word s input = 
  let u = String.lowercase_ascii s in
  try 
    let (a,(_,rest)) = 
      some_nodeX 
        (function 
         | WORD (_,w) as w' -> 
             ((w = u),w')
         | VAR v -> 
             let (b,v') = is_word v in 
             (b && u=v'),WORD(v,v')
         | t -> (false,t)) input in 
    (a,(TrString ("matched:"^u),rest))
  with 
    Noparse _ -> raise (Noparse (TrGroup (("expected:"^u),trPos input)))

let anyword = some(function | WORD _ -> true | VAR v -> fst(is_word v) | _ -> false)

let anywordexcept banned = 
  let banned' = List.map String.lowercase_ascii banned in 
  some(function 
      | WORD(_,w) -> not(List.mem w banned') 
      | VAR v -> let (b,v') = is_word v in b && not(List.mem v' banned')
      | _ -> false)

(* let anyphrase = plus(anyword)  *)

let phrase s = 
  let ps = List.map word (String.split_on_char ' ' s) in 
  parse_all ps

let someword s = 
  let s' = List.map word (String.split_on_char ' ' s) in
  parse_some s'




(*
let commit_head err parse1 parse2 input = 
  (try(  
    let (result,(t,rest)) = parse1 input in 
    fix err (parse2 (fun ins -> result,(t,ins))) rest)
  with Noparse t1 -> parse2 (failtr t1) input)
 *)

let commit_head err parse1 parse2 input = 
  let (result,(t,rest)) = 
    try parse1 input
    with Noparse _ -> raise (Noparse (TrGroup("bad header in "^err,trPos input))) in 
  try (
    (parse2 (fun ins -> result,(t,ins))) rest
  )
  with Noparse t2 -> raise (Nocatch(TrGroup(err,t2)))

let phantom parse input =
  try 
    let (_,_) =  group "phantom" parse input in 
    (),(TrEmpty,input)
  with Noparse _ -> raise (Noparse(TrGroup("phantom",trPos input)))

 (* accumulate parse1s until parse2 succeeds *)
let rec until parse1 parse2 input = 
  try parse2 input 
  with Noparse _ -> 
         let result,next = parse1 input in
         let (r1,r2),rest = until parse1 parse2 next in
         (result :: r1, r2),rest

(* parentheses *)

let paren parser  = 
  (a L_PAREN ++ parser ++ a R_PAREN) >> (fun ((_,p),_) -> p) 

let bracket parser = 
  (a L_BRACK ++ parser ++ a R_BRACK) >> (fun ((_,p),_) -> p) 

let pos_bracket parser = 
    (a L_BRACK ++ parser ++ a R_BRACK) >> (fun ((p',p),p'') -> (pair_pos(p'.pos,p''.pos),p)) 

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
      plus(nonbrack accept) |||
        paren(balanced) |||
        bracket(balanced) |||
        brace(balanced)) >> List.flatten  ) input
and balanced input = balancedB (fun _ -> true) input

let brace_semi = 
  let semisep = balancedB (function | SEMI -> false | _ -> true) in
  brace(semisep ++ many(a(SEMI) ++ semisep)) >>
    (fun (a,bs) -> a :: (List.map snd bs))

let comma = a COMMA 

let comma_nonempty_list parser = 
  separated_list parser comma 

let  and_comma = (* no Oxford comma allowed, which is reserved for sentence conjunction *)
  word "and" ||| comma

let and_comma_nonempty_list parser = separated_list parser and_comma

let lit_binder_comma = comma

let cs_brace parser1 parser2 = 
  parser1 ++ many (brace parser2) 

(* set up synonym hashtable *)


let expanded_word s input = 
  let u = find_syn (String.lowercase_ascii s) in 
  try 
    let (a,(_,rest)) = some_nodeX 
      (function 
       | WORD (w,wu) -> 
           let wsyn = find_syn wu in
           ((wsyn = u),WORD (w,wsyn))
       | VAR v -> 
           let (b,v') = is_word v in 
           (b && v'=u),WORD(v,v')
       | t -> (false,t)) input in 
    (a,(TrString ("matched:"^u),rest))
  with 
    Noparse _ -> raise (Noparse (TrGroup (("expected:"^u),trPos input)))


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

let case_sensitive_word = 
  some_nodeX (function 
      | WORD (w,_) -> true,(ATOMIC_IDENTIFIER w)
      | x -> (false,x))

let the_case_sensitive_word s = 
  some_nodeX (function 
      | WORD (w,_) -> (w=s),(ATOMIC_IDENTIFIER w)
      | x -> (false,x))

let atomic_identifier =
  some_nodeX (function 
      | ATOMIC_IDENTIFIER _ as x -> true,x
      | INTEGER s -> true,ATOMIC_IDENTIFIER s 
      | t -> false,t)
    
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
     | { tok = ATOMIC_IDENTIFIER s; pos } -> mk (HIERARCHICAL_IDENTIFIER s,pos)
     | _ -> failwith "Fatal:identifier") 
  |||
    atomic_identifier
  |||
    hierarchical_identifier 

let the_id s = 
  some (function
      | WORD(s',_) -> (s = s')
      | ATOMIC_IDENTIFIER s' -> (s = s')
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
    possibly(phrase "as identification") ++
    possibly (word "that")

let lit_any = someword "every each all any some no" |||
    (phrase "some and every" 
     >> (fun ts -> mk(WORD ("some and every","some and every"),merge_pos (List.map pos ts))))

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

let lit_we_say = possibly (word "we") ++ lit_say ++ possibly (word "that") >>
                   (fun ((we,s),r) -> we @ [s] @ r)

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
  | [{ tok = ATOMIC_IDENTIFIER l; _}] -> l 
  | _ -> failwith "bad format: section label"

 (* XX do scoping of variables etc. *)

let set_current_scope (label,scope_end,new_level) = 
    if not(scope_end) then 
      if (4 <= new_level) then 
        current_scope := label :: !current_scope
      else 
        current_scope := label :: (pad (new_level) "" !current_scope)
    else (* scope_end *) 
      if (4 <= new_level) then 
        current_scope := List.tl (cutat (fun s -> (label="" || label = s)) !current_scope)
      else 
        let _ = new_level < List.length !current_scope || 
                  failwith "ending division that was not started" in 
        let ts = pad (new_level + 1) "" !current_scope in 
        let _ = (List.hd ts = label) || failwith "ending division does not match start" in
        current_scope := List.tl ts 

let treat_section_preamble =
  (function
  | (({ tok = WORD(_,sec); pos },ls),p') -> (
    let l = getlabel ls in
    let scope_end = is_scope_end sec in
    let new_level = scope_level sec in 
    set_current_scope (l,scope_end,new_level); 
    Section_preamble(pair_pos(pos,p'.pos),new_level,l))
  | _ -> failwith "bad format: treat_section_preamble")


let section_preamble = 
  commit_head "section" section_tag
    (fun t -> ((t ++ possibly label ++ period) >> treat_section_preamble))

(* namespace XX NOT_IMPLEMENTED *)

let namespace = failparse

(* instructions *)

let instruct_commands = ref []
let instruct_strings = ref []
let instruct_bools = ref []
let instruct_ints = ref []

 (* For now the parser stores instructions without any action *)



let put_commands = 
  function 
  | { tok = WORD (_,w); _ } -> (instruct_commands := (w :: !instruct_commands) ; w )
  | _ -> ""

let put_strings = function 
  | ({ tok = WORD (_,w); _ },{ tok = STRING s; _ }) -> (instruct_strings := ((w,s) :: !instruct_strings) ; w)
  | _ -> ""

let put_bools = function 
  | ({ tok = WORD (_,w); _ },b) -> (instruct_bools := ((w,b) :: !instruct_bools); w )
  | _ -> ""

let put_ints = function 
  | ({ tok = WORD (_,w); _ },i) -> (instruct_ints := ((w,i) :: !instruct_ints); w )
  | _ -> ""


let instruct_keyword_command = someword "exit"
let instruct_keyword_int = someword "timelimit"
let instruct_keyword_bool = someword "printgoal dump ontored"
let instruct_keyword_string = someword "read library error warning"
let instruct_command = instruct_keyword_command >> put_commands

let integer = someX ((function | INTEGER x -> true,int_of_string x | _ -> false,0)-| tok)

let instruct_int = 
  (instruct_keyword_int ++ integer) >> put_ints

let bool_tf = (lit_true >> (fun _ -> true)) ||| 
                (lit_false >> (fun _ -> false))

let instruct_bool = 
  (instruct_keyword_bool ++ bool_tf) >> put_bools

let string = some (function | STRING _ -> true | _ -> false)

let instruct_string = 
  (instruct_keyword_string ++ string) >> put_strings

 (* We depart slightly from the grammar to make SLASHDASH easier to process *)

let rec expand_slashdash css cs =
  function
  | [] -> let css' = List.filter (fun c -> not(c=[])) (cs :: css) in 
          List.map List.rev css'
  | WORD(_,w2) :: ts -> expand_slashdash css (w2 :: cs) ts 
  | SLASH :: ts -> expand_slashdash (cs :: css) [] ts 
  | SLASHDASH :: WORD(_,w2') :: ts -> 
      (match cs with 
       | [] -> failwith "expand_slashdash: /- must fall between full words"
       | w2 :: hs -> 
           let cs2 = (w2^w2') :: hs in 
           expand_slashdash (cs2 :: cs :: css) [] ts 
      )
  | _ -> failwith "expand_slashdash: /- must fall between words"

let is_syntoken = 
  function 
  | SLASHDASH -> true,SLASHDASH 
  | WORD _ as wd -> true,wd
  | VAR v -> let (b,v') = is_word v in b,WORD(v,v')
  | SLASH -> true,SLASH
  | t -> false,t

let synlist = 
  let synnode = someX (is_syntoken -| tok) in
    many synnode >> (expand_slashdash [] [])

let instruct_synonym = (word "synonyms" ++ synlist) >> 
                         (fun (_,ls) -> syn_add ls; "synonyms")

let instruction = 
  commit "instruction" (a L_BRACK)
    (pos_bracket(instruct_synonym |||
       instruct_command |||
       instruct_string |||
       instruct_bool |||
       instruct_int) >> (fun (a,b) -> Instruction(a,b)))

let synonym_statement = 
  let head = possibly (word "we") ++ possibly(word "introduce") ++ word "synonyms" in
  commit_head "synonym_statement" head
    (fun head -> 
           ((getpos ++ head ++ synlist ++ getpos ++ a PERIOD  
            >> (fun ((((p,_),ls),p'),_) -> (syn_add ls;Synonym ( pair_pos (p,p')))))))

(* XX need to add to the namespace of the given structure *)
let morever_implements = 
  let except = (function | WORD(_,w) -> not(w = "implements") | _ -> true) in
  let head = word "moreover" ++ comma in
  commit_head "moreover_implements" head 
  (fun head ->
         getpos ++ head ++ balancedB except ++ word "implements" ++ brace_semi
           ++ getpos ++ a PERIOD
  ) 
  >> (fun ((((((p,_),b),_),b'),p'),_) -> Implement (pair_pos(p,p'),Wp_implement (b,b')))

(* end of instructions *)

let stored_string_token = 
  (function
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
  | _ -> warn true "stored string node expected"; "") 

let stored_string = stored_string_token -| tok


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
  group "this is" (word "is" ++ and_comma_nonempty_list(this_directive_adjective) >> snd) 
  |||
    (group "this_directive_verb" this_directive_verb)

let this_exists = (* no period *)
  commit "this_exists" (word "this" ++ someword "exists is")
  (getpos ++ word "this" ++ group "and_comma_nonempty_list" (and_comma_nonempty_list(this_directive_pred)) ++ getpos >>
     (fun (((p,_),ls),p') -> (pair_pos (p,p'),List.flatten ls)))

(* text *)

(* axiom *)
let then_prefix = possibly (lit_then)

let assumption_prefix = 
  possibly(lit_lets) ++ group "assume" lit_assume ++ possibly (word "that")

let assumption = 
  (assumption_prefix ++ balanced ++ (a PERIOD) >> 
     (fun ((_,b),_) -> Statement' b)) (* |||
    (* XXX This accepts too much *)
    (balanced ++ (a PERIOD) >> (fun (b,_) -> LetAnnotation' b)) *)

let axiom_preamble = 
  lit_axiom ++ possibly (label) ++ (a PERIOD) >>
    (function 
     | (({ tok = WORD (_,w); _ },ls),_) -> (w,getlabel ls) 
     | _ -> failwith "axiom_preamble:unreachable-state")

let axiom = 
  commit_head "axiom" axiom_preamble
  (fun head -> 
         (getpos ++ head ++ many(assumption) 
          ++ then_prefix ++ balanced ++ getpos ++ a(PERIOD) 
          >> (fun ((((((p,(w,labl)),ls),_),st),p'),_) -> 
            Axiom (pair_pos(p,p'),w,labl,ls,Statement' st))))

(* theorem *) 
let ref_item = and_comma_nonempty_list(possibly lit_location ++ label) 

let by_ref = possibly(paren(word "by" ++ ref_item))

let by_method = 
  (* exclude 'that' to avoid grammar ambiguity in goal_prefix *)
  let except = (function | WORD(_,w) -> not(w = "that") | _ -> true) in
  commit_head "proof method" (word "by")
  (fun head -> (head ++ 
    ((word "contradiction" >> discard) |||
    (phrase "case analysis" >> discard) |||
    (word "induction" ++ possibly (word "on" ++ balancedB except) >> discard)) ++
  phantom ((word "that" ||| a(PERIOD))) >> discard))

let choose_prefix = 
  then_prefix ++ possibly(lit_lets) ++ lit_choose 

let canned_proof = phrase_list_proof_statement >> discard

let canned_prefix = and_comma_nonempty_list(phrase_list_transition) ++ possibly(a(COMMA))

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
     (fun ((((_,b),_),p),p') -> (merge_pos (p.pos::p'),b))) input

and goal_proof input = 
  (goal_prefix ++ balanced ++ by_ref ++ a(PERIOD) ++ 
     proof_script >> 
     (fun ((((_,b),_),_),p) -> (p,b))) input

and proof_script input = 
  (proof_preamble ++ 
    many(canned_prefix ++ proof_body ++ canned_prefix ++ proof_tail) ++
    lit_qed ++ a(PERIOD) >> (fun (_,n) -> n.pos)) input 

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
     (fun ((t,ls),p) -> (pair_pos(t.pos,p.pos),getlabel ls)))

let theorem = 
  commit_head "theorem" theorem_preamble 
  (fun head -> (head ++ many(assumption) ++ affirm_proof >>
    (fun (((p,l),ls),(p',st)) -> Theorem(pair_pos (p,p'),l,ls,Statement' st))))


(* patterns *)
 (* XX need to process multiword synonyms *)

let pattern_banned = 
  ["is";"be";"are";"denote";"stand";"if";"iff";"the";"a";"an";
   "we";"say";"write";"define";"enter";"namespace";
   "said";"defined";"or";"fix";"fixed";"and";"inferring"]

let not_banned =
  let u = String.lowercase_ascii in 
  let p s = not(List.mem (u s) pattern_banned) in
  (function
  | WORD(_,s) -> p s
  | VAR s -> p s
  | ATOMIC_IDENTIFIER s -> p s
  | HIERARCHICAL_IDENTIFIER s -> p s
  | _ -> true)

let any_pattern_word = 
  some(function 
      | WORD (_,_) as w  -> not_banned w
      | _ -> false)

let word_in_pattern = 
  any_pattern_word >> (fun s -> [Wp_wd s]) |||   
  (paren(comma_nonempty_list(word "or" ++ plus(any_pattern_word))) (* synonym *)
  >> (fun ss -> 
            let ss' = List.map snd ss in 
             List.map (fun s' -> Wp_syn s') ss')) |||
  (paren(any_pattern_word) >> (fun s -> [Wp_opt s]))

let words_in_pattern = 
  (any_pattern_word ++ many(word_in_pattern)) 
  >> (fun (a,b) -> Wp_wd a :: List.flatten b)

let post_colon_balanced = 
  balancedB (* true nodes can follow opt_colon_type *)
    (function | ASSIGN | SEMI | COMMA | ALT | COLON -> false 
              | WORD (_,s) -> not(s = "end") && not(s = "with")
              | _ -> true)

let colon_type = 
  a COLON ++ post_colon_balanced >> snd 
               
let opt_colon_type = 
  possibly(colon_type) 
  >> (fun bs -> Colon' (List.flatten bs))
                         
let tvarpat = 
  (paren(var ++ opt_colon_type) >>
    (fun (v,bs) -> Wp_var (v,bs))) 
  |||
    (var >> (fun v -> Wp_var(v,Colon' [])))

let word_pattern = group "word_pattern"
  (words_in_pattern ++ many(tvarpat ++ words_in_pattern) ++ possibly(tvarpat))
   >>
     fun ((a,bs),c) ->
           a @ (List.flatten (List.map (fun (b,b')-> b::b') bs)) @ c

let type_word_pattern = possibly(lit_a) ++ word_pattern >> (fun (_,bs) -> Wp_ty_word bs)

let function_word_pattern = 
  word("the") ++ word_pattern >> 
    (fun (_,w) -> Wp_fun_word w)

let notion_pattern = 
  tvarpat ++ word "is" ++ lit_a ++ word_pattern
  >> (fun (((v,_),_),w) -> Wp_notion (v::w) )
                              
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
  notion_pattern ||| 
  adjective_pattern |||
    adjective_multisubject_pattern |||
    verb_pattern |||
    verb_multisubject_pattern

let opt_args_pat = 
  brace_semi

 (* XX When we distribute an empty type, we should require that
   the types are all the same through a metavariable.  *)

let required_arg_pat = 
  paren(var_or_atomics ++ opt_colon_type) 
  >> (fun (vs,bs) -> List.map (fun v -> v,bs) vs)
  |||
    group "required_arg_pat2" 
      (must (not_banned -| tok) var_or_atomic  >> fun a -> [a,Colon' []])

let args = 
  (possibly opt_args_pat >> List.flatten) ++ (many(required_arg_pat) >> List.flatten)

let identifier_pattern = group "identifier_pattern"
  (possibly(lit_a) ++ (must (not_banned -| tok)  identifier ||| a(BLANK)) ++
    group "args" args)

let controlseq = some(function | CONTROLSEQ _ -> true | _ -> false)

let controlseq_pattern = 
  group "controlseq_pattern" (controlseq ++ many(brace(tvarpat)))

let binary_controlseq_pattern = 
  tvarpat ++ controlseq_pattern ++ tvarpat >>
    (fun ((a,b),c) -> Wp_bin_cs (a,b,c))

let symbol = 
  some(function | SLASH | SLASHDASH -> true | SYMBOL _ -> true | _ -> false) 
  >> (fun a -> Wp_sym a) |||
    (controlseq_pattern >> (fun (t,w) -> Wp_cs (t,w)))

let precedence_level = 
  let assoc = 
    (function 
    | WORD (_,s) ->
        (match s with 
         | "left" -> AssocLeft 
         | "right" -> AssocRight 
         | _ -> AssocNone)
    | _ -> AssocNone) -| tok in
  word "with" ++ word "precedence" ++ integer ++
    possibly(word "and" ++ lit_left ++ word "associativity") >>
    (fun (((_,_),i),bs) -> 
           let a = (function 
             | [] -> AssocNone
             | ((_,l),_) :: _ -> assoc l) bs in 
           (i,a))

let paren_precedence_level = 
  precedence_level ||| paren precedence_level

let symbol_pattern = group "symbol_pattern"
  (possibly(tvarpat) ++ symbol ++
    many(tvarpat ++ symbol) ++ possibly(tvarpat) ++ possibly(paren_precedence_level)) >>
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
    (fun head -> (head ++ lit_document ++ possibly(comma)
     >> 
       (function | ((_,{ tok = WORD(_,s); _ }),_) -> 
                     (let i = scope_level s in
                      let subdivision = (i>3) in
                      if subdivision 
                      then max 0 (List.length !current_scope - 1)
                      else i)
                 | _ -> 0)
    ))


let we_record_def = group "we_record_def"
  (lit_we_record ++
    comma_nonempty_list 
      (balancedB (function | COMMA | SEMI | PERIOD -> false | _ -> true)) ++
  possibly(a PERIOD ++ this_exists >> snd)
  >> (fun (a,b) -> [Wp_record (snd a,List.flatten (List.map snd b)),[]]))

let annotated_var = paren(var ++ opt_colon_type)

let annotated_vars = paren(plus(var) ++ opt_colon_type) >>
                       (fun (vs,o) -> List.map (fun v -> Wp_var(v,o)) vs)

let let_annotation_prefix = 
  (word("let") ++ comma_nonempty_list(var) ++ word "be" ++ possibly(lit_a) 
   ++ possibly(word "fixed")) >>
    fun ((((_,a),_),_),w) -> (a,not(w = []))

let fix_var fix v = if fix then 
                      (match v with
                       | Wp_var(v,o) -> Wp_fix(v,o)
                       | _ -> v)
                    else v

let let_annotation = commit  "let_annotation" (phantom (someword "fix let"))
  (((word "fix" >> (fun _ -> true) ||| (word "let" >> (fun _ -> false))) ++ 
    comma_nonempty_list(annotated_vars) 
   >>
     (fun (t,bs) -> List.map (fun b -> (fix_var t b,[])) (List.flatten bs))
     )
  |||
    ((let_annotation_prefix ++ post_colon_balanced)
     >>
       (fun ((vs,t),b) -> List.map (fun v -> (fix_var t (Wp_var (v,Colon' b)),[])) vs 
    )))

(* macro end *)


(* definitions *)

let copula = group "copula"
  (phantom (someword "is are be denote stand" ||| a (ASSIGN)) ++ 
     (
       (lit_is ++ possibly(lit_defined_as) >> discard) 
       |||
         (a(ASSIGN) >> discard) 
       |||
         (lit_denote >> discard)
     )
  ) >> discard

let function_copula =
  copula >> (fun _ -> Colon' []) |||
    (opt_colon_type ++ a(ASSIGN) >> fst)

let iff_junction = lit_iff 

let opt_say = possibly(lit_we_say)

let opt_record = possibly(lit_we_record) >> discard 

let opt_define = 
  (possibly(lit_lets) ++ possibly(word "define") >> discard) 
  |||     (opt_record) (* creates both a definition and record *)

let macro_inferring = 
  paren(word "inferring" ++ plus(var) ++ opt_colon_type)
  >> (fun ((_,a),b) -> (a,b))

let classifier_words = comma_nonempty_list(plus(anywordexcept ["is";"are";"be"]))

 (* XX need to add prim actions *)
let classifier_def = group "classifier_def"
  (word "let" ++ classifier_words ++ lit_is ++ possibly(lit_a) ++ lit_classifier)
  >> (fun ((((_,c),_),_),_) -> [Wp_classifier c,[]])

let type_head = 
  group "type_word_pattern" type_word_pattern |||
    (symbol_pattern >> (fun (a,_,_) -> Wp_sympatT (a))) |||
    (identifier_pattern >> (fun ((_,a),(b,c)) -> Wp_ty_identifier (a,b,c))) |||
    (controlseq_pattern >> (fun (a,b) -> Wp_ty_cs (a,b)))

let type_def_body = 
  group "Type" (a(COLON) ++ the_case_sensitive_word "Type" ++ 
                  copula ++ possibly(lit_a) >> discard) 
  ||| (copula ++
        (
               (word "the" ++ lit_type >> discard)
             |||
               (possibly(article) 
                 ++ phantom (possibly(word "notational") ++ word "structure") >> discard)
             ||| 
               (possibly(article) 
                ++ phantom (possibly(word "mutual") ++ word "inductive") >> discard)
        ) >> discard
      )

let type_def = group "type_def"
  (opt_define  ++ group "type_head" type_head ++ type_def_body
   ++ balanced ++ phantom (a(PERIOD))) >>
    (fun ((((_,a),_),b),_) -> [(a,b)])

let function_head = 
  (function_word_pattern |||
    (symbol_pattern >> (fun (a,b,c) -> Wp_sympat (a,b,c))) ||| 
       (identifier_pattern >> (fun ((_,a),(b,c)) -> Wp_identifier (a,b,c))))

let function_def = group "function_def"
  (opt_define ++ function_head ++ possibly(macro_inferring) ++
     function_copula ++ possibly(lit_equal) ++ possibly(article) ++ 
     group "balanced" balanced  ++ phantom  (a(PERIOD))   ) >>
    (fun (((((((_,h),m),_),_),_),b),_) -> 
           let h' = if m=[] then h 
                    else let (i,i')= List.hd m in Wp_inferring(h,i,i')
           in [(h',b)])

let predicate_head = 
  predicate_word_pattern |||
  (symbol_pattern >> (fun (a,b,c) -> Wp_sympatP (a,b,c))) |||
    (identifier_pattern >> (fun ((_,a),(b,c)) -> Wp_identifierP (a,b,c)))

let predicate_def = group "predicate_def"
  (opt_say ++ predicate_head ++ possibly(macro_inferring) ++ iff_junction
  ++ balanced ++ phantom (a(PERIOD)))
  >>
    (fun (((((_,h),m),_),b),_) -> 
           let h' = if m=[] then h 
                    else let (i,i')= List.hd m in Wp_inferring(h,i,i')
           in [(h',b)]
    )

let enter_namespace = 
  group "enter_namespace" 
    (phrase "we enter the namespace" ++ identifier) >> 
    (fun (_,i) -> [(Wp_namespace i,[])])

let macro_body = 
  type_def    
  ||| function_def 
  ||| predicate_def
  ||| classifier_def
  ||| let_annotation

let macro_bodies = 
  we_record_def 
  ||| enter_namespace
  |||
    (separated_list macro_body (a(SEMI) ++ possibly(word "and"))  
     >> List.flatten)

let macro = 
  getpos ++ possibly(insection) ++ macro_bodies ++ getpos ++ a(PERIOD)
  >>
    fun ((((p,i),m),p'),_)-> 
          let sec_level = (match i with | [] -> 0 | i'::_ -> i') in
          Macro (pair_pos(p,p'),sec_level, m)
    
(* end macro *)                 

(* definition *)

let definition_statement = 
  (classifier_def)
  ||| (function_def)
  ||| type_def
  ||| (predicate_def)

let definition_preamble = 
  group "definition_preamble" 
  (getpos ++ group "lit_def" lit_def ++ group "label" (possibly(label)) ++ a PERIOD 
  >> (fun (((p,_),l),_) -> (p,getlabel l)))

let definition_affirm = 
  getpos ++ definition_statement ++ possibly(a PERIOD ++ this_exists >> snd) 
  ++ getpos ++ a PERIOD
  >> (fun ((((p,w),e),p'),_) ->
            let ex = List.flatten (List.map snd e) in 
            (pair_pos(p,p'),w,ex))

let definition = 
  commit_head "definition" definition_preamble 
  (fun t -> t ++ group "assumption" (many assumption) 
  ++ group "affirm" definition_affirm 
  >> (fun (((p,l),ma),(p',w,ex)) -> Definition (pair_pos (p,p'),l,ma,w,ex)))

let text = 
  group "text" 
    (
      (group "section_preamble" section_preamble)
      ||| (group "instruction" instruction)
      ||| (group "axiom" axiom)
      ||| (group "definition" definition)
      ||| (group "theorem" theorem)
      ||| (group "macro" macro)
      ||| (group "synonym_statement" synonym_statement)
      ||| (group "moreover_implements" morever_implements)
      ||| (group "namespace" namespace >> (fun _ -> Namespace))
    )





(* proof_expr *)

let proof_expr = 
  a SYMBOL_QED 
  ||| paren(a SYMBOL_QED) >> (fun _ ->Proof)

(* variables *)

let tvar = 
  var 
  >> (fun v -> TVar (v,None))
  ||| (annotated_var 
       >> fun (v,ty) -> TVar (v,Some ty))

let pre_expr = (balancedB (function | COMMA | SEMI | PERIOD -> false | _ -> true))

let assign_expr = a ASSIGN ++ pre_expr >> snd

let record_assign_item = 
  var_or_atomic ++ opt_colon_type ++ assign_expr >>
    (fun ((a,b),c) -> 
           (TVarAtomic(a,Some b),Expr' c))

let record_assign_term = 
  brace_semi >> (fun ts -> List.map (consume record_assign_item) ts)

(* tightest terms *)

let var_term = var >> fun v -> TVar(v,None)

let decimal = 
  someX
    (function 
     | { tok = DECIMAL _ ; _ } as t -> true,Decimal t  
     | _ -> false,Error')

let integer_term = integer >> (fun i -> Integer i)

let string_term = 
  someX
    (function 
     | { tok = STRING _; _ } as s -> true, String s 
     | _ -> false,Error')

let blank_term = a BLANK >> (fun _ -> Blank)

 (* XX to do - hierarchical identifiers *)

let prim_identifier_term = 
  someX(fun t -> prim_identifier_term_exists t.tok,Id(t,None))

let controlseq1 = 
  someX
    (function 
     | { tok = CONTROLSEQ _; _ } as s  -> (true,s) 
     | t -> (false,t))

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
     >> (fun (_,(_,vs)) -> List.map (fun v -> TVar(v,None)) vs)
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

let paren_type = (* XX need to know a priori that balanced is a type *)
  paren(balanced) >> (fun t -> Type' t)

let annotated_type = 
  paren(post_colon_balanced ++ a COLON ++ the_case_sensitive_word "Type") 
  >> (fun ((t,_),_) -> Type' t)

let controlseq_type = 
  let controlseq1 = some(fun t -> prim_type_controlseq_exists t) in
  cs_brace controlseq1 balanced 
  >> fun (t,ts) -> TyControlSeq (t,ts)

let const_type = 
  someX(fun t -> prim_identifier_type_exists t.tok,TyId t)

let var_type = 
  let f v = prim_type_var_exists (stored_string v) in 
  must f var >> (fun v -> TyVar (v))
  ||| (paren(var ++ a COLON ++ the_case_sensitive_word "Type") 
         >> (fun ((v,_),_) -> TyVar ( v)))

let subtype = group "subtype"
  (brace(commasep ++ holding_var ++ a TMID ++ balanced))
  >> (fun (((a,b),_),c) -> Subtype (Plain' a,b,Statement' c))

let colon_sort' = a COLON ++ post_colon_balanced >> snd

let alt_constructor = a ALT ++ identifier ++ args ++ a COLON ++ post_colon_balanced

let opt_alt_constructor = 
  a ALT ++ identifier ++ args ++ opt_colon_type
  >> (fun (((_,i),a),o) -> (i,a,o))

let inductive_type = group "inductive_type"
  ((word "inductive" ++ identifier ++ args ++ possibly(colon_sort')
    ++ many(opt_alt_constructor) ++ word "end")
   >> (fun (((((_,i),a),p),m),_) -> (i,a,p,m)))

let mutual_inductive_type = group "mutual_inductive"
  (word "inductive" ++ comma_nonempty_list(identifier) ++ args ++
    many(word "with" ++ atomic ++ args ++ colon_type ++ many(alt_constructor)) ++
    word "end")

let satisfying_preds = brace_semi
                 
let structure = group "structure"
  ((possibly(word "notational") ++ word "structure" 
  ++ possibly(phrase("with parameters")) ++ args
  ++ possibly(word "with") ++ brace_semi
  ++ possibly(possibly(lit_with_properties) ++ satisfying_preds >> snd))
  >> (fun ((((_,a),_),b),b') ->  Structure' (a,b,List.flatten b')))

(* XX
let tightest_type = 
  group "tightest_type"
    (
      paren_type
      ||| annotated_type
      ||| controlseq_type
      ||| const_type
      ||| var_type
      ||| subtype
      ||| inductive_type
      ||| mutual_inductive_type
      ||| structure
      ||| prim_structure
    )

 *)

(* let sentence =  *)
