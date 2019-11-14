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

(* common library *)
let sprintf = Printf.sprintf

let flatten = List.flatten

let map = List.map 

exception Noparse of trace;;

exception Nocatch of trace;;

let trEof =  TrFail (0,0,EOF);;

let failEof = Noparse(trEof)

let getpos = 
  function 
  | [] -> raise failEof
  | t::_ as input -> t.pos,(TrEmpty,input)

let line_col n = 
  let p = fst(n.pos) in 
  let line = p.pos_lnum in 
  let col = p.pos_cnum - p.pos_bol in 
  (line,col)

let line_col_string (line,col) = 
  "line="^string_of_int line^" col="^string_of_int col

let trPos = 
  function (* input *)
    | [] -> trEof
    | n :: _ -> 
        let (line,col)=line_col n in 
        (TrFail(line,col,n.tok))


let rec get_trace_data =
  function 
  | TrMany t -> flatten (map get_trace_data t)
  | TrAdd t -> flatten (map get_trace_data t)
  | TrOr [] -> []
  | TrOr t -> get_trace_data (List.hd (List.rev t))
  | TrGroup (_,t) -> get_trace_data t
  | TrFail (_,_,_) -> []
  | TrData t -> t
  | TrString _ -> []
  | TrEmpty -> []

let rec pick_best_tr = 
  function
  | [] -> (0,0),TrEmpty 
  | [x] -> x
  | ((i,j),t) :: ((i',j'),t') :: rest -> 
      (let ijt  = 
        if (i > i' || (i=i' && j > j')) then (i,j),t else (i',j'),t' in 
      pick_best_tr (ijt :: rest));;


let rec get_best_trace = 
  function
  | TrFail (i,j,_) as tr -> (i,j),tr
  | TrEmpty -> (0,0),TrEmpty
  | TrString _ as tr -> (0,0),tr
  | TrData _  as tr ->  (0,0),tr
  | TrGroup(s,tr) -> let (i,j),tr' = get_best_trace tr in (i,j),TrGroup(s,tr')
  | TrOr trs -> let trs' = map get_best_trace trs in pick_best_tr trs' 
  | TrAdd trs-> let trs' = map get_best_trace trs in 
                let (i,j),_ =  pick_best_tr trs' in 
                (i,j),TrAdd(map snd trs')
  | TrMany _ as t -> (0,0),t 

let filter_nonempty = 
  List.filter (fun t -> not (t = TrEmpty))

let endswithplus s = 
  if s = "" then false 
  else s.[String.length s - 1] = '+'

let is_trstring =
  function
  | TrString _ -> true
  | _ -> false

let rec join_string sep =
  function
  | TrString s :: TrString s' :: t -> join_string sep (TrString ("("^s ^ sep ^ s'^")") :: t)
  | t -> t 

let rec clean_tr ls = 
  let d def ts' = 
    match List.length ts' with 
    | 0 -> TrEmpty 
    | 1 -> clean_tr (List.hd ts')
    | _ -> def in 
  match ls with 
  | TrMany ts -> let ts' = filter_nonempty (map clean_tr ts) in 
                 let ts' = 
                   if List.for_all is_trstring ts' 
                   then [TrString (String.concat " " (map (function | TrString s -> s | _ -> failwith "trString expected") ts'))]
                   else ts' in 
                 d (TrMany ts') ts'
  | TrAdd ts -> 
      let ts' = join_string " ++ " (filter_nonempty (map clean_tr ts)) in 
      d (TrAdd ts') ts' 
  | TrOr ts -> let ts' = (filter_nonempty (map clean_tr ts)) in 
                d (TrOr ts') ts' 
  | TrGroup (s,TrGroup(s',t')) -> 
      let sep = if endswithplus s then "" else "/" in
                    clean_tr (TrGroup(s^sep^s',t'))
  | TrGroup (s,TrString s') -> TrString(s^":"^s')
  | TrGroup (s,t) as trg  -> let t' = clean_tr t in 
                     if (t = t') then trg else clean_tr (TrGroup(s,t'))
  | TrData ts -> TrString (string_of_toks ts)
  | TrFail (i,j,tok)-> TrString("unexpected token:'"^string_of_toks [tok]^"' failed at "^line_col_string (i,j))
  | TrEmpty -> TrString("empty")
  | t -> t

let show_trace = 
  function
  |  TrString s -> s
  | t -> show_trace t


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

let separated_nonempty_list prs sep =
  prs ++ many (sep ++ prs >> snd) >> (fun (h,t) -> h::t);;

let nothing input = [],input;;

let unit input = ((),input);;

let empty input = ((),(TrEmpty,input))

let separated_list prs sep =
  separated_nonempty_list prs sep ||| nothing;;

(*
let leftbin prs sep cons err =
  prs ++ many (sep ++ fix err prs) >>
  (fun (x,opxs) -> let ops,xs = unzip opxs in
                   itlist2 (fun op y x -> cons op x y) (List.rev ops) (List.rev xs) x);;

let rightbin prs sep cons err =
  prs ++ many (sep ++ fix err prs) >>
  (fun (x,opxs) -> if opxs = [] then x else
                   let ops,xs = unzip opxs in
                   itlist2 cons ops (x::butlast xs) (last xs));;

 *)
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

let dependent_plus parser1 parser2 input = 
  let result1,(t1,rest1) = parser1 input in
  try 
    let result2,(t2,rest2) = parser2 result1 rest1 in
    result2,(mergeAdd(t1,t2),rest2)
  with | Noparse t2 -> raise(Noparse (mergeAdd(t1,t2)))
       | Nocatch t2 -> raise(Nocatch (TrGroup ("<...> ++",t2)))

let (-|) f g x = f(g(x))

let mk (t,p) = { tok = t; pos = p }

let optionally p = 
  possibly p >> (function | [] -> None | x :: _ -> Some x)

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

let has_possibly p = possibly p >> (function | [] -> false | _ -> true)

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
  let u = singularize s in
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
  let banned' = map singularize banned in 
  some(function 
      | WORD(_,w) -> not(List.mem w banned') 
      | VAR v -> let (b,v') = is_word v in b && not(List.mem v' banned')
      | _ -> false)

(* let anyphrase = plus(anyword)  *)

let phrase s = 
  let ps = map word (String.split_on_char ' ' s) in 
  parse_all ps

let someword s = 
  let s' = map word (String.split_on_char ' ' s) in
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

(*
let pos_bracket parser = 
    (a L_BRACK ++ parser ++ a R_BRACK) >> (fun ((p',p),p'') -> (pair_pos(p'.pos,p''.pos),p)) 
 *)

let brace parser = 
  (a L_BRACE ++ parser ++ a R_BRACE) >> (fun ((_,p),_) -> p) 

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
        brace(balanced)) >> flatten  ) input
and balanced input = balancedB (fun _ -> true) input

let brace_semi = 
  let semisep = balancedB (function | SEMI -> false | _ -> true) in
  brace(semisep ++ many(a(SEMI) ++ semisep)) >>
    (fun (a,bs) -> a :: (map snd bs))

let comma = a COMMA 

let comma_nonempty_list parser = 
  separated_nonempty_list parser comma 

let  and_comma = (* no Oxford comma allowed, which is reserved for sentence conjunction *)
  word "and" ||| comma

let and_comma_nonempty_list parser = separated_nonempty_list parser and_comma

let lit_binder_comma = comma

let cs_brace parser1 parser2 = 
  parser1 ++ many (brace parser2)     

let phrase_list_transition = 
  let w _ = "phrase list transition" in
  (somecomb phrase phrase_list_transition_words 
   ++ possibly (word "that") >> w)

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

let mk_word s = WORD(s,singularize s)

let lit_is = 
  let wbe = mk_word "be" in 
  let w = (fun _ -> wbe) in
  (word "is" >> w) |||
    (word "are" >> w) |||
    ((possibly (word "to") ++ word "be") >> w)

let lit_iff = 
  let wiff = mk_word "iff" in 
  let w = (fun _ -> wiff) in
  (phrase "iff" >> w) |||
    (phrase "if and only if" >> w)  |||
    ((lit_is ++ possibly (word "the") ++ word "predicate") >> w)

let lit_denote = 
  let wd = mk_word "denote" in 
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

let section_tag = lit_document ||| lit_enddocument

let period = some (function | PERIOD -> true | _ -> false)


let getlabel =
  function
  | [] -> "" 
  | [{ tok = ATOMIC_IDENTIFIER l; _}] -> l 
  | _ -> failwith "bad format: section label"

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


 (* XX do scoping of variables etc. *)

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


 (* For now the parser stores instructions without any action *)

(*
let instruct_commands = ref []
let instruct_strings = ref [] 
let instruct_bools = ref []
let instruct_ints = ref []
*)


(*
let run_commands = 
  function 
  | { tok = WORD (_,w); _ } -> 
      ( 
        match w with 
        |  "exit" -> raise Exit
        | _ -> 
            (
              instruct_commands := (w :: !instruct_commands);
              raise (failwith "instruct_command: unreachable state")
            )
      )
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
 *)




let integer = someX ((function | INTEGER x -> true,int_of_string x | _ -> false,0)-| tok) 

let instruct_command = 
  let instruct_keyword_command = someword "exit" in 
  instruct_keyword_command 
  >> fun s -> InstructCommand (stored_string s)

let instruct_int = 
  let instruct_keyword_int = someword "timelimit" in 
  (instruct_keyword_int ++ integer
  >> fun (s,i) -> InstructInt (stored_string s,i)
  )

let instruct_bool = 
  let bool_tf = (lit_true >> (fun _ -> true)) ||| 
                  (lit_false >> (fun _ -> false)) in 
  let instruct_keyword_bool = someword "printgoal dump ontored" in 
  (instruct_keyword_bool ++ bool_tf
  >>  fun (s,b) -> InstructBool(stored_string s, b)
  )

let instruct_string = 
  let instruct_keyword_string = someword "read library error warning" in 
  let string = some (function | STRING _ -> true | _ -> false) in 
  (instruct_keyword_string ++ string
  >>  (fun (s,s') -> InstructString (stored_string s,stored_string s'))
  )

 (* 

  DEPRECATED: 
  We depart slightly from the grammar to make SLASHDASH easier to process 

let rec deprecated_expand_slashdash css cs =
  function
  | [] -> let css' = List.filter (fun c -> not(c=[])) (cs :: css) in 
          map List.rev css'
  | WORD(_,w2) :: ts -> deprecated_expand_slashdash css (w2 :: cs) ts 
  | SLASH :: ts -> deprecated_expand_slashdash (cs :: css) [] ts 
  | SLASHDASH :: WORD(_,w2') :: ts -> 
      (match cs with 
       | [] -> failwith "expand_slashdash: /- must fall between full words"
       | w2 :: hs -> 
           let cs2 = (w2^w2') :: hs in 
           deprecated_expand_slashdash (cs2 :: cs :: css) [] ts 
      )
  | _ -> failwith "expand_slashdash: /- must fall between words"

*)

let rec expand_slashdash acc =
  function
  | [] -> List.rev acc 
  | WORD(_,w) :: ts -> expand_slashdash (w :: acc) ts 
  | SLASH :: WORD(_,w) :: ts -> expand_slashdash (w :: acc) ts 
  | SLASHDASH :: WORD(_,suffix) :: ts -> 
      (match acc with 
       | [] -> failwith "expand_slashdash: /- must fall between full words"
       | prev :: _ -> 
           let acc' = (prev^suffix) :: acc  in 
           expand_slashdash acc' ts
      )
  | _ -> failwith "expand_slashdash: / /- must fall between words"

let is_syntoken = 
  function 
  | SLASHDASH -> true,SLASHDASH 
  | WORD _ as wd -> true,wd
  | VAR v -> let (b,v') = is_word v in b,WORD(v,v')
  | SLASH -> true,SLASH
  | t -> false,t

let synlist = 
  let synnode = someX (is_syntoken -| tok) in
    many synnode >> (expand_slashdash [])

let instruct_synonym = 
  (word "synonyms" ++ comma_nonempty_list(synlist)
   >> (fun (_,ss) -> InstructSyn ss))

let instruction = 
  commit "instruction" (a L_BRACK)
    (getpos 
     ++ (bracket(
             instruct_synonym |||
               instruct_command |||
               instruct_string |||
               instruct_bool |||
               instruct_int
        )
        )
     >> (fun (p,i) -> Instruction (p,i)))

let synonym_statement = 
  let head = possibly (word "we") ++ possibly(word "introduce") ++ word "synonyms" in
  commit_head "synonym_statement" head
    (fun head -> 
           ((getpos ++ head ++ synlist ++ getpos ++ a PERIOD  
            >> (fun ((((p,_),ls),p'),_) -> (syn_add ls;Synonym ( pair_pos (p,p')))))))

(* end of instructions *)



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
     (fun (((p,_),ls),p') -> (pair_pos (p,p'),flatten ls)))

(* text *)

(* axiom *)

let post_colon_balanced = 
  balancedB (* true nodes can follow opt_colon_type *)
    (function | ASSIGN | SEMI | COMMA | ALT | COLON -> false 
              | WORD (_,s) -> not(s = "end") && not(s = "with")
              | _ -> true)

               
let opt_colon_type = 
  let colon_type = 
    a COLON ++ post_colon_balanced >> snd in 
  possibly(colon_type) 
  >> (fun bs -> RawPostColon (flatten bs))

let mk_meta =
  let i = ref 0 in 
  fun () ->
        let () = i := !i + 1 in 
        string_of_int (!i)

let meta_node() = 
  {tok = METAVAR (mk_meta()); pos = (empty_pos,empty_pos)}

let opt_colon_type_meta = 
  opt_colon_type 
  >> (function 
      | RawPostColon [] -> RawPostColon [meta_node()]
      | _ as t -> t)

let colon_sortish = 
  possibly(a COLON ++ post_colon_balanced >> snd) 
  >> (fun t -> Raw_Colon_Sortish (flatten t))

let colon_sortish_meta = 
  possibly(a COLON ++ post_colon_balanced >> snd) 
  >> (fun t -> if t = [] then Raw_Colon_Sortish [meta_node()] else Raw_Colon_Sortish (flatten t))

let annotated_var = paren(var ++ opt_colon_type)

let annotated_vars = paren(plus(var) ++ opt_colon_type_meta) >>
                       (fun (vs,o) -> map (fun v -> (v,o)) vs)

let let_annotation_prefix = 
  (word("let") ++ comma_nonempty_list(var) ++ word "be" ++ possibly(lit_a) 
   ++ has_possibly(word "fixed")) >>
    fun ((((_,a),_),_),bw) -> (a,bw)

(* let fix_var e = Wp_env e *)
(* if fix then Wp_fix(v,o) else Wp_var(v,o) *)

(* don't commit to let_annotation, because lit_then might not lie ahead *)

let let_annotation = group  "let_annotation" 
  (((word "fix" >> (fun _ -> true) ||| (word "let" >> (fun _ -> false))) ++ 
    comma_nonempty_list(annotated_vars) 
   >>
     (fun (t,bs) -> map (fun (v,o) -> EVar (stored_string v,Etyp o,t)) (flatten bs))
     )
  |||
    ((let_annotation_prefix ++ post_colon_balanced)
     >>
       (fun ((vs,t),b) -> map (fun v -> EVar (stored_string v,Etyp (RawPostColon b),t)) vs 
    )))

let then_prefix = possibly (lit_then)

let assumption = 
  let assumption_prefix = 
    possibly(lit_lets) ++ group "assume" lit_assume ++ possibly (word "that") in 
  group "assumption"
  ((assumption_prefix ++ balanced ++ (a PERIOD) >> 
     (fun ((_,b),_) -> RawStatement b)) 
  |||
    (let_annotation >> (fun t -> LetAnnotation t)))

let possibly_assumption = 
  (possibly (many assumption ++ lit_then >> fst)) >> flatten

let axiom_preamble = 
  lit_axiom ++ possibly (label) ++ (a PERIOD) >>
    (function 
     | (({ tok = WORD (_,w); _ },ls),_) -> (w,getlabel ls) 
     | _ -> failwith "axiom_preamble:unreachable-state")

let axiom = 
  commit_head "axiom" axiom_preamble
  (fun head -> 
         (getpos 
          ++ head 
          ++ possibly_assumption
          ++ balanced ++ getpos ++ a(PERIOD) 
          ++ (many(word "moreover" ++ balanced ++ getpos ++ a (PERIOD)
               >> (fun (((_,b),p),_) -> (RawStatement b,p)))
             >> Lib.unzip)
          >> (fun ((((((p,(w,labl)),ls),st),p'),_),(sts,ps)) -> 
            Axiom (merge_pos(p :: p' :: ps),w,labl,ls,RawStatement st :: sts))))

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
  (getpos ++ then_prefix ++ balanced ++ by_ref ++ a(PERIOD)
   ++ many(word "moreover" ++ balanced ++ a (PERIOD)
            >> (fun ((_,b),_) -> RawStatement b))
   ++ getpos ++ possibly (proof_script) 
   >> 
     (fun (((((((p,_),b),_),_),sts),p'),_) -> (pair_pos (p,p'),RawStatement b :: sts))) input

and goal_proof input = 
  (goal_prefix ++ balanced ++ by_ref ++ a(PERIOD) ++ 
     proof_script >> 
     (fun ((((_,b),_),_),p) -> (p,[RawStatement b]))) input

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


(* patterns *)
 (* XX need to process multiword synonyms *)

let pattern_banned = 
  ["is";"be";"are";"denote";"define";
   "enter";"namespace";
   "stand";"if";"iff";"inferring";"the";"a";"an";
   "we";"say";"write";
   "said";"defined";"or";"fix";"fixed";(* "and"; *)
   (* "and" needed in phrases such as "resultant of f and g" *)
  ]

let not_banned =
  let u = singularize in 
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
            let ss' = map snd ss in 
             map (fun s' -> Wp_syn s') ss')) |||
  (paren(any_pattern_word) >> (fun s -> [Wp_opt s]))

let words_in_pattern = 
  (any_pattern_word ++ many(word_in_pattern)) 
  >> (fun (a,b) -> Wp_wd a :: flatten b)

let tvarpat = 
  (paren(var ++ opt_colon_type) >>
    (fun (v,bs) -> Wp_var (v,bs))) 
  |||
    (var >> (fun v -> Wp_var(v,RawPostColon [])))

let word_pattern = group "word_pattern"
  (words_in_pattern ++ many(tvarpat ++ words_in_pattern) ++ possibly(tvarpat))
   >>
     fun ((a,bs),c) ->
           a @ (flatten (map (fun (b,b')-> b::b') bs)) @ c

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
    (paren(var ++ a(COMMA) ++ var ++ opt_colon_type_meta)
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
  notion_pattern 
  ||| adjective_pattern
  ||| adjective_multisubject_pattern
  ||| verb_pattern
  ||| verb_multisubject_pattern

let pre_expr = (balancedB (function | COMMA | SEMI | PERIOD -> false | _ -> true))

let assign_expr = a ASSIGN ++ pre_expr >> snd

let var_or_atomic_or_blank = 
  var_or_atomic ||| a(BLANK)

(* assignments *)


let brace_assign = 
  let brace_assign_item = 
    var_or_atomic_or_blank ++ colon_sortish ++ possibly assign_expr >>
      (fun ((a,b),c) -> 
             (RawExpr [a]),b,RawExpr (flatten c)) in 
  brace_semi >> (fun ts -> map (consume brace_assign_item) ts)

let brace_noassign = 
  let brace_noassign_item = 
    var_or_atomics ++ optionally colon_sortish_meta >> 
      (fun (vs,o) -> map (fun v -> (RawExpr [v],o)) vs) in 
  brace_semi >> (fun ts -> map(consume brace_noassign_item) ts) >> flatten

(* nonbrace *)

let required_arg_template_pat = 
  paren(var_or_atomics ++ opt_colon_type_meta) 
  >> (fun (vs,bs) -> map (fun v -> v,bs) vs)
  |||
    group "required_arg_template_pat2" 
      (must (not_banned -| tok) var_or_atomic  >> fun a -> [a,RawPostColon []])

let args_template = 
  (possibly brace_noassign >> flatten) ++ (many(required_arg_template_pat) >> flatten)


(* *)

let identifier_pattern = group "identifier_pattern"
  (possibly(lit_a) ++ (must (not_banned -| tok)  identifier ||| a(BLANK)) ++
    group "args_template" args_template)

let controlseq = some(function | CONTROLSEQ _ -> true | _ -> false)

let the_controlseq s = some(function | CONTROLSEQ s' -> (s=s') | _ -> false)

let controlseq_pattern = 
  group "controlseq_pattern" (controlseq ++ many(brace(tvarpat)))

let binary_controlseq_pattern = 
  tvarpat ++ controlseq_pattern ++ tvarpat >>
    (fun ((a,b),c) -> Wp_bin_cs (a,b,c))

let symbol = 
  some(function | SLASH | SLASHDASH -> true | SYMBOL _ -> true | _ -> false) 
  >> (fun a -> Wp_sym a) |||
    (controlseq_pattern >> (fun (t,w) -> Wp_cs (t,w)))

let the_symbol s = 
  some
    (function 
     | SYMBOL s' -> (s=s') 
     | SLASH -> s="/" 
     | SLASHDASH -> s="/-" 
     | _ -> false
    )

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
           let cs' = flatten (map (fun (c,c') -> [c;c']) cs) in
           let e' = (function 
                     | [] -> (None,AssocNone)
                     | (i,a) :: _ -> (Some i,a)) e in
           ((a @ (b :: cs' @ ds)),fst e',snd e'))

let binary_symbol_pattern = group "binary_symbol_pattern"
  (tvarpat ++ symbol ++ tvarpat ++ possibly(paren_precedence_level) 
   >>
    (fun (((a,b),d),e) -> 
           let e' = (function 
                     | [] -> (None,AssocNone)
                     | (i,a) :: _ -> (Some i,a)) e in
           ((a :: b :: [d]),fst e',snd e')))



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
  >> (fun (a,b) -> [Wp_record (snd a,flatten (map snd b)),[]]))

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
  copula >> (fun _ -> RawPostColon []) |||
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

let symbol_type_pattern = 
    (symbol_pattern >> (fun (a,_,_) -> Wp_sympatT (a)))

let identifier_type_pattern = 
    (identifier_pattern >> (fun ((_,a),(b,c)) -> Wp_ty_identifier (a,b,c)))

let controlseq_type_pattern =     
  (controlseq_pattern >> (fun (a,b) -> Wp_ty_cs (a,b)))

let type_head = 
  group "type_word_pattern" 
    type_word_pattern 
  ||| symbol_type_pattern
  ||| identifier_type_pattern 
  ||| controlseq_type_pattern

let type_def_copula = 
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
  (opt_define  ++ group "type_head" type_head ++ type_def_copula
   ++ balanced ++ phantom (a(PERIOD))) >>
    (fun ((((_,a),_),b),_) -> [(a,b)])

let symbol_term_pattern = 
  (symbol_pattern >> (fun (a,b,c) -> Wp_sympat (a,b,c)))

let identifier_term_pattern = 
  (identifier_pattern >> (fun ((_,a),(b,c)) -> Wp_identifier (a,b,c)))

let function_head = 
  function_word_pattern 
   ||| symbol_term_pattern 
   ||| identifier_term_pattern

let inferring head = 
  head ++ possibly(macro_inferring) 
  >> (fun (h,m) -> 
           let h' = 
             if m=[] then h 
             else let (i,i')= List.hd m in Wp_inferring(h,i,i')
           in h')

let function_def head = group "function_def"
  (opt_define ++ inferring head ++ 
     function_copula ++ possibly(lit_equal) ++ possibly(article) ++ 
     group "balanced" balanced  ++ phantom  (a(PERIOD))   ) >>
    (fun ((((((_,h),_),_),_),b),_) -> [(h,b)])
(*
           let h' = if m=[] then h 
                    else let (i,i')= List.hd m in Wp_inferring(h,i,i')
           in [(h',b)])
 *)

let identifier_predicate_pattern = 
  (identifier_pattern >> (fun ((_,a),(b,c)) -> Wp_identifierP (a,b,c)))

let identifier_symbol_pattern = 
  (symbol_pattern >> (fun (a,b,c) -> Wp_sympatP (a,b,c)))

let predicate_head = 
  identifier_predicate_pattern
  ||| predicate_word_pattern 
  ||| identifier_symbol_pattern 


let predicate_def = group "predicate_def"
  (opt_say ++ predicate_head ++ possibly(macro_inferring) ++ iff_junction
  ++ balanced ++ phantom (a(PERIOD)))
  >>
    (fun (((((_,h),m),_),b),_) -> 
           let h' = if m=[] then h 
                    else let (i,i')= List.hd m in Wp_inferring(h,i,i')
           in [(h',b)]
    )

let binder_pattern = 
  symbol ++ paren(var ++ colon_sortish) (* : Term -> Prop, Term -> Type, Term -> Term, etc. *)
  >> (fun (s,(v,c)) -> Wp_binder (s,v,c))

let binder_def = 
  commit_head "binder_def" (phrase "let the binder")
  (fun h -> h ++ binder_pattern
   ++ lit_denote
   ++ group "balanced" balanced ++ phantom (a PERIOD)
  )
  >> (fun ((((_,s),_),b),_) -> [s,b])
                   

let enter_namespace = 
  group "enter_namespace" 
    (phrase "we enter the namespace" ++ identifier) >> 
    (fun (_,i) -> [(Wp_namespace i,[])])

let macro_body = 
  type_def    
  ||| function_def function_head
  ||| predicate_def
  ||| classifier_def
  ||| binder_def 
  ||| (let_annotation >> map (fun e -> (Wp_env e,[])))

let macro_bodies = 
  we_record_def 
  ||| enter_namespace
  |||
    (separated_nonempty_list macro_body (a(SEMI) ++ possibly(word "and"))  
     >> flatten)

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
  ||| (function_def function_head)
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
            let ex = flatten (map snd e) in 
            (pair_pos(p,p'),w,ex))

let definition = 
  commit_head "definition" definition_preamble 
  (fun t -> t ++ group "assumption" possibly_assumption
  ++ group "affirm" definition_affirm 
  >> (fun (((p,l),ma),(p',w,ex)) -> Definition (pair_pos (p,p'),l,ma,w,ex)))

let theorem_preamble = 
  (word "theorem" ++ possibly(label) ++ a(PERIOD) >>
     (fun ((t,ls),p) -> (pair_pos(t.pos,p.pos),getlabel ls)))

let theorem = 
  commit_head "theorem" theorem_preamble 
  (fun head -> 
         (head 
          ++ possibly_assumption
          ++ affirm_proof 
          >>
            (fun (((p,l),ls),(p',sts)) -> Theorem(pair_pos (p,p'),l,ls,sts))))

(* XX need to add to the namespace of the given structure *)

let moreover_implements = 
  let except = (function | WORD(_,w) -> not(w = "implement") | _ -> true) in
  let head = word "moreover" ++ comma in
  commit_head "moreover_implements" head 
  (fun head ->
         getpos ++ head ++ balancedB except ++ word "implement" ++ brace_semi
           ++ getpos ++ a PERIOD
  ) 
  >> (fun ((((((p,_),b),_),b'),p'),_) -> 
            Implement (pair_pos(p,p'),RawGeneralType b,List.map (fun t -> RawNodeList t) b'))

(* fiat *)
let fiat_preamble = 
  word "fiat"  ++ label ++ (a PERIOD) 
  >> (fun ((_,ls),_) -> stored_string ls )

let fiat_prim_classifier = 
  classifier_def >> unzip >> fst

let fiat_prim_term_op_controlseq = 
  comma_nonempty_list (binary_symbol_pattern 
                       >> (fun (a,b,c) -> Wp_sympat (a,b,c)))

let fiat_prim_binary_relation_controlseq = 
  comma_nonempty_list (binary_symbol_pattern 
                       >> (fun (a,b,c) -> Wp_sympatP (a,b,c)))

let fiat_prim_propositional_op_controlseq = 
  comma_nonempty_list (binary_symbol_pattern 
                       >> (fun (a,b,c) -> Wp_sympatP (a,b,c)))

let fiat_prim_type_op_controlseq = 
  comma_nonempty_list (binary_symbol_pattern 
                       >> (fun (a,_,_) -> Wp_sympatT a))

let fiat_prim_term_controlseq = 
  comma_nonempty_list symbol_term_pattern

let fiat_prim_type_controlseq = 
  comma_nonempty_list controlseq_type_pattern

let fiat_prim_lambda_binder = 
  comma_nonempty_list binder_pattern

let fiat_prim_pi_binder = 
  comma_nonempty_list binder_pattern

let fiat_prim_binder_prop = 
  comma_nonempty_list binder_pattern

let fiat_prim_adjective = 
  comma_nonempty_list adjective_pattern 

let fiat_prim_adjective_multisubject = 
  comma_nonempty_list adjective_multisubject_pattern 

let fiat_prim_simple_adjective = 
  comma_nonempty_list adjective_pattern 

let fiat_prim_simple_adjective_multisubject = 
  comma_nonempty_list adjective_multisubject_pattern 

let fiat_prim_definite_noun = 
  comma_nonempty_list (inferring function_word_pattern)

let fiat_prim_identifier_term = 
  comma_nonempty_list (inferring identifier_term_pattern)

let fiat_prim_identifier_type = 
  comma_nonempty_list identifier_type_pattern

let fiat_prim_typed_name = fiat_prim_identifier_type

let fiat_prim_possessed_noun = 
  comma_nonempty_list 
    (inferring  function_word_pattern
     ||| identifier_type_pattern
    )

let fiat_prim_verb = 
  comma_nonempty_list verb_pattern

let fiat_prim_verb_multisubject = 
  comma_nonempty_list verb_multisubject_pattern 

let fiat_prim_structure = 
    comma_nonempty_list identifier_type_pattern

let fiat_prim_type_op = 
  comma_nonempty_list symbol_type_pattern

let fiat_prim_type_word = 
    comma_nonempty_list type_word_pattern

let fiat_prim_term_op = 
  comma_nonempty_list (binary_symbol_pattern 
                       >> (fun (a,b,c) -> Wp_sympat (a,b,c)))

let fiat_prim_binary_relation_op = 
  comma_nonempty_list (binary_symbol_pattern  
                       >> (fun (a,b,c) -> Wp_sympatP (a,b,c)))

let fiat_prim_propositional_op = 
  fiat_prim_binary_relation_op
  
let fiat_prim_relation =
  comma_nonempty_list identifier_predicate_pattern 


let fiat_body = 
  function 
  | "prim_classifier" -> fiat_prim_classifier
  | "prim_term_op_controlseq" -> fiat_prim_term_op_controlseq
  | "prim_binary_relation_controlseq" -> fiat_prim_binary_relation_controlseq
  | "prim_propositional_op_controlseq" -> fiat_prim_propositional_op_controlseq
  | "prim_type_op_controlseq" -> fiat_prim_type_op_controlseq
  | "prim_term_controlseq" -> fiat_prim_term_controlseq
  | "prim_type_controlseq" -> fiat_prim_type_controlseq
  | "prim_lambda_binder" -> fiat_prim_lambda_binder
  | "prim_pi_binder" -> fiat_prim_pi_binder
  | "prim_binder_prop" -> fiat_prim_binder_prop
  | "prim_typed_name" -> fiat_prim_typed_name
  | "prim_adjective" -> fiat_prim_adjective
  | "prim_adjective_multisubject" -> fiat_prim_adjective_multisubject
  | "prim_simple_adjective" -> fiat_prim_simple_adjective
  | "prim_simple_adjective_multisubject" -> fiat_prim_simple_adjective_multisubject
  | "prim_definite_noun" -> fiat_prim_definite_noun
  | "prim_identifier_term" -> fiat_prim_identifier_term
  | "prim_identifier_type" -> fiat_prim_identifier_type
  | "prim_possessed_noun" -> fiat_prim_possessed_noun
  | "prim_verb" -> fiat_prim_verb
  | "prim_verb_multisubject" -> fiat_prim_verb_multisubject
  | "prim_structure" -> fiat_prim_structure
  | "prim_type_op" -> fiat_prim_type_op
  | "prim_type_word" -> fiat_prim_type_word
  | "prim_term_op" -> fiat_prim_term_op
  | "prim_binary_relation_op" -> fiat_prim_binary_relation_op
  | "prim_propositional_op" -> fiat_prim_propositional_op
  | "prim_relation" -> fiat_prim_relation
  | _ -> failwith "fiat_body:illegal case"

let fiat = 
  dependent_plus fiat_preamble fiat_body ++ a PERIOD >> fst 
  >> (fun x -> Fiat x)

let text = 
  group "text" 
    (
      (group "section_preamble" section_preamble)
      ||| (group "instruction" instruction)
      ||| (group "axiom" axiom)
      ||| (definition)
      ||| (group "theorem" theorem)
      ||| (group "fiat" fiat)
      ||| (group "macro" macro)
      ||| (group "synonym_statement" synonym_statement)
      ||| (group "moreover_implements" moreover_implements)
      ||| (group "namespace" namespace >> (fun _ -> Namespace))
    )

let text_to_period = 
  getpos ++ many(some (function |PERIOD -> false | _ -> true)) ++ getpos ++ a PERIOD 
  >>
    (fun (((p1,m),p2),p3) -> pair_pos (p1,p2), (m @ [p3]))
                          

(* proof_expr *)

let proof_expr = 
  (a SYMBOL_QED >> discard)
  ||| 
  (paren(a SYMBOL_QED 
         ++ possibly(a COLON ++ the_case_sensitive_word "Proof") >> discard))
  >> (fun _ ->Proof)

(* variables *)

let tvar = 
  var 
  >> (fun v -> TVar (stored_string v,None))
  ||| (annotated_var 
       >> fun (v,ty) -> TVar (stored_string v,Some ty))

(* tightest terms *)

let var_term = var >> fun v -> TVar(stored_string v,None)

let decimal = 
  someX
    (function 
     | { tok = DECIMAL _ ; _ } as t -> true,Decimal (stored_string t)
     | _ -> false,RawUnreachable)

let integer_term = integer >> (fun i -> Integer i)

let string_term = 
  someX
    (function 
     | { tok = STRING _; _ } as s -> true, String (stored_string s)
     | _ -> false,RawUnreachable)

let blank_term = a BLANK >> (fun _ -> Blank)


 (* XX to do - hierarchical identifiers *)

let prim_identifier_term = 
  someX(fun t -> prim_identifier_term_exists t.tok,Id(stored_string t,None))

let controlseq1 f = 
  someX
    (function 
     | { tok = CONTROLSEQ s; _ } as t  -> (f s,t) 
     | t -> (false,t))

let controlseq_term f = 
  cs_brace (controlseq1 f) balanced 
  >> (fun (s,bs) -> ControlSeq (stored_string s,(map (fun b -> RawExpr b) bs)))

let paren_term = 
  paren(balanced) >> (fun b -> RawTerm b)

let annotated_term = 
  paren(post_colon_balanced ++ opt_colon_type)
    >> (fun (t,ty) -> Annotated (RawTerm t,ty))

let paren_expr = (* need to know a priori that balanced is an expr *)
  paren(balanced)

let annotated_type = 
  paren(post_colon_balanced ++ a COLON ++ the_case_sensitive_word "Type") 
  >> (fun ((t,_),_) -> RawGeneralType t)

let const_type = 
  someX(fun t -> prim_identifier_type_exists t.tok,TyId (stored_string t))

let var_type = 
  let f v = prim_type_var_exists (stored_string v) in 
  must f var >> (fun v -> TyVar (stored_string v))
  ||| (paren(var ++ a COLON ++ the_case_sensitive_word "Type") 
         >> (fun ((v,_),_) -> TyVar (stored_string v)))

let paren_type = 
  (paren_expr  >> (fun t -> RawGeneralType t))
  ||| annotated_type
  ||| const_type
  ||| var_type

let make_term = 
(*  let make_term_item = 
    var_or_atomic_or_blank ++ opt_colon_type ++ 
      possibly(assign_expr)
    >> fun ((v,ty),ae) -> (v,ty,flatten ae) in 
 *)
  (word "make" ++ optionally paren_type ++ brace_assign 
   >> fun ((_,p),ts) -> Make (p,ts) (* (map (consume brace_assign_item) ts)) *)
  )


let list_term =
  let semisep = balancedB (function | SEMI | COMMA -> false | _ -> true) in
  bracket(separated_nonempty_list semisep (a(SEMI))) 
  >> (fun ts -> List (map (fun t -> RawPlainTerm t) ts))

let commasep = balancedB (function | SEMI | COMMA -> false | _ -> true)

let tuple_term = 
  paren(separated_nonempty_list commasep (a(COMMA))) >>
    (fun ts -> Tuple (map (fun t -> RawPlainTerm t) ts))
  
let set_enum_term = 
  brace(separated_nonempty_list commasep (a(COMMA))) >>
    (fun ts -> SetEnum (map (fun t -> RawPlainTerm t) ts))

let holding_var = (* comma needed for disambiguation *)
  possibly
    (a COMMA ++ paren(word "holding" ++ comma_nonempty_list(var))
     >> (fun (_,(_,vs)) -> map (fun v -> TVar(stored_string v,None)) vs)
    ) >> flatten

let set_comprehension_term = 
  brace(commasep ++ holding_var ++ a MID ++ balanced) 
  >> (fun (((a,b),_),c) -> Comprehension (RawPlainTerm a,b,RawStatement c))

let case_term = 
  let alt_case = 
    a(ALT) ++ post_colon_balanced ++ a(ASSIGN) ++ post_colon_balanced in 
  word "case" ++ plus(alt_case) ++ word "end" 
  >> (fun ((_,bs),_) -> Case (map (fun (((_,b),_),b') -> (RawProp b,RawPlainTerm b')) bs))

let match_term = 
  let csv = separated_nonempty_list (post_colon_balanced >> (fun s -> RawPlainTerm s)) (a COMMA) in
  word "match" ++ csv ++ word "with" 
  ++ plus(a ALT ++ csv ++ a ASSIGN ++ post_colon_balanced
          >> (fun (((_,ss),_),p)-> (ss,RawPlainTerm p)))
  ++ word "end"
  >> (fun ((((_,ss),_),ps),_) -> Match (ss,ps))

let match_function = 
  let csv = separated_nonempty_list (post_colon_balanced >> (fun s -> RawPlainTerm s)) (a COMMA) in
  word "function" ++ args_template ++ opt_colon_type
  ++ plus(a ALT ++ csv ++ a ASSIGN ++ post_colon_balanced
          >> (fun (((_,ss),_),p) -> (ss,RawPlainTerm p)))
  ++ word "end"
  >> (fun ((((_,(s,s')),ty),ps),_) -> MatchFunction (s,s',ty,ps))

let tightest_prefix = 
  decimal 
  ||| integer_term
  ||| string_term
  ||| blank_term 
  ||| var_term
  ||| prim_identifier_term
  ||| (controlseq_term (fun _ -> true))
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

 (* to continue to app_term we need app_args, hence tightest_expr *)

and tightest_terms input = 
  paren(plus(tightest_term)) input

(* tightest types *)

let controlseq_type = 
  let controlseq1 = some(fun t -> prim_type_controlseq_exists t) in
  cs_brace controlseq1 balanced 
  >> fun (t,ts) -> TyControlSeq (stored_string t,map (fun e -> RawExpr e) ts)

let subtype = group "subtype"
  (brace(commasep ++ holding_var ++ a TMID ++ balanced))
  >> (fun (((a,b),_),c) -> Subtype (RawPlainTerm a,b,RawStatement c))


let alt_constructor = a ALT ++ identifier ++ args_template ++ a COLON ++ post_colon_balanced

let opt_alt_constructor = 
  a ALT ++ identifier ++ args_template ++ opt_colon_type
  >> (fun (((_,i),a),o) -> (i,a,o))

let inductive_type = 
  group "inductive_type"
    ((word "inductive" ++ identifier ++ args_template ++ colon_sortish
      ++ many(opt_alt_constructor) ++ word "end")
     >> (fun (((((_,i),a),p),m),_) -> Inductive' (i,a,p,m)))

let mutual_inductive_type = 
  group "mutual_inductive"
    ((word "inductive" ++ comma_nonempty_list(identifier) >> snd) 
     ++ args_template 
     ++ many((word "with" ++ atomic >> snd) ++ args_template 
             ++ colon_sortish ++ many(alt_constructor))
     ++ word "end" 
     >> fst
    ) >> (fun ((i1,i2),i3) -> Mutual' (i1,i2,i3))

let satisfying_preds = brace_semi
                 
let structure = group "structure"
  ((possibly(word "notational") ++ word "structure" 
  ++ possibly(phrase("with parameters")) ++ args_template
  ++ possibly(word "with") ++ brace_semi
  ++ possibly(possibly(lit_with_properties) ++ satisfying_preds >> snd))
  >> (fun ((((_,a),_),b),b') ->  Structure' (a,b,flatten b')))

let tightest_type = 
  group "tightest_type"
    (paren_type
      ||| controlseq_type
      ||| subtype
      ||| inductive_type
      ||| mutual_inductive_type
      ||| structure
    )

(* now start on tightest_prop *) 

let identifier_prop = 
  someX(fun t -> prim_relation_exists t.tok,PRel t)

let precolon_sep = balancedB (function | COLON -> false | _ -> true)

let annotated_prop = 
  paren(precolon_sep ++ a COLON ++ the_case_sensitive_word "Prop" >> (fun ((p,_),_) -> RawProp p))

let tightest_prop = 
  group "tightest_prop" 
    (
      (paren_expr >> (fun t -> PStatement' t))
      ||| identifier_prop
      ||| (var >> (fun t -> PVar t))
      ||| annotated_prop 
    )

let tightest_expr = 
  group "tightest_expr"
  (
    (paren_expr >> (fun t -> RawExpr t))
   ||| (tightest_term >> (fun t -> Eterm t))
   ||| (tightest_prop >> (fun t -> Eprop t))
   ||| (tightest_type >> (fun t -> Etyp t))
   ||| (proof_expr >> (fun _ -> Eproof))
  )

let app_args = 
  (possibly(brace_assign) >> flatten) ++ many(tightest_expr)

let app_term = tightest_term ++ app_args 
             >> (fun (t,(r,e)) -> App (t,r,e))

let tightest_arg = 
  (tightest_expr >> (fun e -> [e]))
  ||| (paren(var_or_atomics ++ colon_sortish_meta) 
       >> (fun (ts,ts') -> 
                 map (fun t -> ETightest(t,ts')) ts)) 

let tightest_args = 
  brace_noassign ++ (many(tightest_arg) >> flatten)


let term_op = 
  let prim_term_op = 
    some(fun t -> prim_term_op_exists (stored_string_token t)) in 
  (prim_term_op >> (fun t -> RawTermOp t))
  ||| (controlseq_term prim_term_op_controlseq_exists)

let term_ops = plus(term_op) 

let tdop_term = (* cannot be pure op *)
  ((possibly(app_term) 
    ++ (plus(term_ops ++ app_term) 
        >> (fun ts -> map (fun (t,a) -> t@ [a]) ts)
        >> flatten)  
    ++ (possibly(term_ops) >> flatten))
   >> (fun ((a,b),c) -> RawTdop (a @ b @ c))
  ) 
  ||| ((app_term ++ (possibly(term_ops) >> flatten)) >> (fun (t,ts) -> RawTdop (t :: ts)))
  ||| (term_ops >> (fun _ -> failwith "tdop_term: term must contain at least one non-symbol"))

let tdop_terms = comma_nonempty_list tdop_term

let prim_lambda_binder =
  some(fun t -> prim_lambda_binder_exists (stored_string_token t))

(* For simplicity, we end the 'let-assignment' clause with the first 
   occurence of the word 'in'.  This means that an 'in' within the 
   assignment must always be wrapped in matching delimiters. 
   However, delimiters are not needed in the opentail:
   let x1 := t1 in let x2 := t2 in Q.
   let x1 := (let x2 := t2 in Q1) in Q2.

   Similar comments apply to 'if-then-else'.

 *)

let in_balanced = 
  balancedB (function | COLON | SEMI | PERIOD | WORD(_,"in") -> false | _ -> true)
  ++ phantom(word "in") >> fst

let then_balanced = 
  balancedB (function | COLON | SEMI | PERIOD | WORD(_,"then") -> false | _ -> true)
  ++ phantom(word "then") >> fst 

let else_balanced = 
  balancedB (function | COLON | SEMI | PERIOD | WORD(_,"else") -> false | _ -> true)
  ++ phantom(word "else") >> fst


(* big recursion of opentail_terms *)

let rec lambda_term input = 
  (prim_lambda_binder ++ (tightest_args ++ lit_binder_comma >> fst) ++ opentail_term
  >> (fun ((i,j),k) -> Lambda (stored_string i,j,k))) input

and mapsto_term input = 
  ((tdop_term ++ a MAPSTO >> fst) ++ opentail_term >> (fun (i,j) -> MapsTo (i,j) )) input

and lambda_fun input = 
  ((word "fun" ++ tightest_args >> snd) ++ (opt_colon_type ++ a ASSIGN >> fst) ++ opentail_term
  >> (fun ((i,j),k) -> LambdaFun (i,j,k))) input 

and let_term input = 
  (word "let" ++ tightest_prefix ++ a ASSIGN ++ in_balanced ++ word "in" ++ opentail_term
  >> (fun (((((_,t),_),i),_),t') ->  Let (t,RawTerm i,t')))  input 

and if_then_else_term input = 
  (word "if" ++ then_balanced ++ word "then" ++ else_balanced ++ word "else" ++ opentail_term 
  >> (fun ((((((_,b),_),b'),_),t)) -> IfThenElse (RawProp b,RawTerm b',t))) input
  
and  opentail_term input = 
  (lambda_term
   ||| lambda_fun
   ||| let_term
   ||| if_then_else_term
   ||| mapsto_term
   ||| tdop_term ) input 

(* end recursion *)

let where_suffix = brace_assign

let where_term = 
  opentail_term ++ possibly where_suffix >> (fun (i,w) -> Where (i,flatten w))

(* now return to props *)

let binary_relation_op = 
  let prim_binary_relation_op = 
    some(fun t -> prim_binary_relation_exists (stored_string_token t)) in 
  (prim_binary_relation_op >> (fun t -> RawTermOp t))
  ||| (controlseq_term prim_binary_relation_controlseq_exists)


let tdop_rel_prop = (* x,y < z < w < u becomes x < z and y < z and z < w and w < u *)
  tdop_terms ++ plus(binary_relation_op ++ tdop_term) >>
    (fun (ls,bts) -> 
           match bts with
           | [] -> failwith "tdop_rel_prop: unreachable"
           | (b,r)::bts' -> 
               let tt = map (fun l -> (l,b,r)) ls in 
               let tt' = snd(List.fold_left (fun (l,acc) (b,t) -> (t,(l,b,t)::acc)) (r,[]) bts') in 
               Ptdopr' (tt @ List.rev tt')
    )

let app_prop = 
  tightest_prop ++ app_args 
  >> (fun (i,(j,k)) -> PApp' (i,j,k))

let lambda_predicate = 
  word "fun" ++ tightest_args ++ (a COLON ++ the_case_sensitive_word "Prop" ++ a ASSIGN) ++ tightest_prop 
  >> (fun (((_,(a,a')),_),p) -> PLambda' (a,a',p))

let prim_binder_prop = 
    some(fun t -> prim_binder_prop_exists (stored_string_token t))

let rec binder_prop input = 
  (app_prop 
  ||| tdop_rel_prop 
  ||| lambda_predicate
  ||| (prim_binder_prop ++ args_template ++ lit_binder_comma ++ binder_prop
      >> (fun (((b,(t,t')),_),p) -> PBinder' (b,t,t',p))
      )
  ) input

let prop_op = 
  let prim_propositional_op = 
    some(fun t -> prim_propositional_op_exists (stored_string_token t)) in 
  (prim_propositional_op >> (fun t -> RawTermOp t))
  ||| (controlseq_term prim_propositional_op_controlseq_exists)

let prop_ops = plus(prop_op) >> (fun ts -> P_ops' ts)

let tdop_prop = (* say it must be infix: that is, start and end with a binder_prop *)
  (possibly(lit_classifier) ++ binder_prop >> snd) 
  ++ (many(prop_ops ++ binder_prop >> (fun (i,j) -> [i;j])) >> flatten)
  >> (fun (b,ts) -> Ptdop' (b :: ts))

let prop = tdop_prop

(* return to loose types *)

let over_args = 
  (word "over" ++ brace_assign >> snd >> (fun t -> Some t,None,None))  (* record *)
  ||| (word "over" ++ tightest_expr >> snd >> (fun t -> None,Some t,None))
  ||| (paren(word "over" 
            ++ comma_nonempty_list 
                 (balancedB (function | COMMA | SEMI | PERIOD -> false | _ -> true)) >> snd)
      >> (fun t -> None,None,Some t))

let overstructure_type = 
  some(fun t -> prim_structure_exists t) 
  ++ app_args 
  ++ possibly(over_args) 
  >> (fun ((t,(a,a')),o) -> Over' (t,a,a',o))

let app_type = 
  (tightest_type ++ app_args >> (fun (i,(a,a')) -> TyApp' (i,a,a')))
  ||| overstructure_type  

let prim_pi_binder = 
    some(fun t -> prim_pi_binder_exists (stored_string_token t))

let rec binder_type input = 
  (app_type 
   ||| (prim_pi_binder ++ tightest_args ++ lit_binder_comma ++ binder_type 
        >> (fun (((b,(a,a')),_),t) -> TyBinder' (b,a,a',t))
       )
  ) input

let agda_dependent_vars = 
  (plus(annotated_vars >> map (fun (v,o) -> TVar(stored_string v,Some o))) >> flatten)

let type_operand = 
  binder_type 
  ||| (agda_dependent_vars >> (fun x -> TyAgda x))

let prim_type_op = 
    some(fun t -> prim_type_op_exists (stored_string_token t))

let prim_type_op_controlseq = 
    some(fun t -> prim_type_op_controlseq_exists (stored_string_token t))

let controlseq_type_op  = 
  cs_brace prim_type_op_controlseq balanced 
  >> fun (t,ts) -> TyControlSeq (stored_string t,map (fun e -> RawExpr e) ts)

let type_op = 
  (prim_type_op >> (fun n -> TyOp' n))
  ||| controlseq_type_op

let binop_type = 
  (possibly(brace_noassign) >> flatten)
    ++ many(type_operand ++ type_op >> (fun (a,b)-> [a;b])) ++ binder_type 
  >> (fun ((r,ts),t) -> TyBinop' (r,flatten ts @ [t]))

(* prim pattern parsing *)


 (* we need to supply term and type parsers *)
let rec apply_pattern (tm,ty) pat input =
  match pat with 
  | Pat_var_term -> (tm >> fun t -> Pat_term t) input 
  | Pat_var_type -> (ty >> fun t -> Pat_type t) input
  | Pat_var_prop -> (prop >> fun t -> Pat_prop t) input
  | Pat_proof -> (proof_expr >> fun _ -> Pat_proof) input 
  | Pat_option p -> (possibly(apply_pattern (tm,ty) p) >> fun t -> Pat_sequence t) input
  | Pat_sequence ps -> 
      (parse_all (map (apply_pattern (tm,ty)) ps) >> fun t -> Pat_sequence t) input
  | Pat_word s -> let (_,rest) = (word s) input in (pat,rest)
  | Pat_symbol s -> let (_,rest) = (the_symbol s) input in (pat,rest) 
  | Pat_controlseq (s,ps) -> 
      let (_,bs),rest = (the_controlseq s ++ many(brace(balanced))) input in 
      if (List.length bs = List.length ps)  
      then 
        let r (p,b) = (apply_pattern (tm,ty) p ++ finished >> fst) b in 
        Pat_sequence (fst(unzip(map r (zip ps bs)))),rest
      else 
        let err = Printf.sprintf "apply_pattern: control seq %s: expected %d braced args" 
                    s (List.length ps) in 
        raise (Nocatch (TrString err))
  | Pat_var_names ->
      (possibly(comma_nonempty_list tm) 
       >> flatten 
       >> fun ts -> Pat_names (map (fun t -> Pat_term t) ts)) input
  | p -> (p,(TrEmpty,input));;



let prim_classifier =
    some(fun t -> (prim_classifier_exists (stored_string_token t)))

let prim_simple_adjective =
    some(fun t -> 
               let s = stored_string_token t in 
               (prim_simple_adjective_exists s)) >> (fun t -> PredPrimSimpleAdj (stored_string t))

let prim_simple_adjective_multisubject =
    some(fun t -> (prim_simple_adjective_multisubject_exists (stored_string_token t)))
    >> (fun t -> PredPrimSimpleAdjMulti (stored_string t))

let prim_pattern tbl (tm,ty) input = 
  let (w,_) = anyword input in 
  let key = stored_string w in 
  let ps = map prim_pattern (prim_find_all_inscope tbl key) in
  let ps = map (apply_pattern (tm,ty)) ps in 
  parse_some ps input

let prim_verb = prim_pattern prim_verb_tbl

let prim_verb_multisubject = prim_pattern prim_verb_multisubject_tbl

let prim_possessed_noun = prim_pattern prim_possessed_noun_tbl

let prim_definite_noun = prim_pattern prim_definite_noun_tbl

let prim_adjective = prim_pattern prim_adjective_tbl

let prim_adjective_multisubject = prim_pattern prim_adjective_multisubject_tbl
  
let prim_typed_name = prim_pattern prim_typed_name_tbl

(* *)


let comma_and = a COMMA ++ word "and";;

let comma_or = a COMMA ++ word "or" ;;

let filler = possibly(phrase_list_filler) 




let any_of_node n = 
  match (stored_string n) with
  | "every"
  | "each"
  | "some and every"
  | "any" 
  | "all" -> QAll
  | "no" -> QNo
  | "some" -> QSome 
  | _ -> failwith "any_of_node : unreachable state"

let lit_any = 
  (someword "every each all any some no" >> any_of_node) |||
    (phrase "some and every" >> (fun _ -> QAll))

let any_arg = (var >> (fun t -> [(t,RawPostColon [])])) ||| annotated_vars

let attribute left (x : node list -> 'a parsed) right =
  many(left) ++ x ++ possibly(right)
  >> (fun ((l,x),r) -> (l,x,r))

(* now the final big recursion *)


let rec statement (input : node list) : statement parsed = 
  (head_statement ||| chain_statement ) input 

and head_statement input : statement parsed = 
  (
    (word "for" ++ and_comma_nonempty_list(any_name) ++ a COMMA ++ statement 
    >> (fun (((_,ts),_),s) -> StateForAny (ts,s)))
    ||| (word "if" ++ statement ++ (a COMMA ++ word "then") ++ statement
        >> (fun (((_,s),_),s') -> StateIfThen (s,s')))
    ||| (lit_its_wrong ++ statement >> (fun (_,s) -> StateNot s))
  ) input
  
and chain_statement input : statement parsed = 
  (
    and_or_chain 
    ||| (paren(and_or_chain) ++ word "iff" ++ statement  
        >> (fun ((p,_),s) -> StateIff (p,s)))
  )
    input 

and and_or_chain input : statement parsed =
  (
    and_chain
    ||| or_chain
    ||| primary_statement
  ) input 

and and_chain input : statement parsed = 
  (separated_nonempty_list primary_statement comma_and ++ comma_and ++ head_primary
   >> (fun ((ts,_),s) -> StateAnd (ts @ [s])))
  input 

and or_chain input : statement parsed = 
  (separated_nonempty_list primary_statement comma_or ++ comma_or ++ head_primary 
   >> (fun ((ts,_),s) -> StateOr (ts @ [s])))
  input 

and head_primary input : statement parsed = 
  (head_statement 
  ||| primary_statement )
  input

and primary_statement input : statement parsed = 
  (simple_statement
   ||| there_is_statement
   ||| (filler ++ symbol_statement >> snd)
   ||| (filler ++ const_statement >> snd)
  ) input

and simple_statement input : statement parsed = 
  (terms ++ separated_nonempty_list  does_pred (word "and") 
   >> (fun (ts,ds) -> StateSimple (ts,ds) ))
    input 

and there_is_statement input : statement parsed =
  (phrase "there exist" ++ (has_possibly(word "no") ++ pseudoterms) >> snd 
   >> (fun (b,ps) -> StateThereExist (b,ps))
  ) input 

and const_statement input : statement parsed = 
  ((possibly(word "the") ++ word "thesis" >>  (fun _ -> StateTrue))
   ||| (possibly(word "the") ++ word "contrary" >>  (fun _ -> StateFalse))
   ||| (lit_a ++ word "contradiction" >> (fun _ -> StateFalse)))
    input 

and symbol_statement input : statement parsed = 
  ((word "forall" ++ predicate_pseudoterm ++ lit_binder_comma ++ symbol_statement 
    >> (fun (((_,b),_),d) -> StateForall(b,d)))
   ||| (word "exists" ++ predicate_pseudoterm ++ lit_binder_comma ++ symbol_statement
        >> (fun (((_,p),_),s) -> StateExist (p,s)))
   ||| (word "not" ++ symbol_statement >> snd >> (fun t -> StateNot t))
   ||| (paren statement)
   ||| (prop >> (fun t -> StateProp t))
  ) input

(* predicates *)
and does_pred input : predicate parsed = 
  ((possibly(lit_do) ++ has_possibly(word "not") ++ prim_verb (term,general_type)
   >> fun ((_,b),v) -> 
            let t = PredPrimVerb v in 
            if b then PredNeg t else t
   )
   ||| (possibly(lit_do) ++ has_possibly(word "not") ++ prim_verb_multisubject (term,general_type)
       >> fun ((_,b),m) -> 
                let t = PredPrimVerbMulti m in 
                if b then PredNeg t else t 
       )
   ||| (lit_has ++ has_pred >> snd )
   ||| (lit_is ++ and_comma_nonempty_list(is_pred) >> fun (_,t) -> PredIs t)
   ||| (lit_is ++ and_comma_nonempty_list(is_aPred) >> fun (_,t) -> PredIsA t)
  ) input 

 (* XX not pairwise vs pairwise not *)
and is_pred input : predicate parsed = 
  ((has_possibly (word "not") ++ prim_adjective (term,general_type)
   >> (fun (b,p) -> 
             let t = PredPrimAdj p in
             if b then PredNeg t else t)
   )
   ||| (has_possibly (word "not") ++ has_possibly(word "pairwise") 
        ++ prim_adjective_multisubject (term,general_type)
        >> 
          (fun ((n,p),a) -> 
                 let t = PredPrimAdj a in 
                 let t = if p then PredPairwise t else t in 
                 let t = if n then PredNeg t else t in 
                 t
          )
       )
   ||| (lit_with ++ has_pred >> fun (_,h) -> PredWith h)
  ) input 

and is_aPred input : predicate parsed = 
  ((has_possibly (word "not") ++ possibly(lit_a) ++ general_type
   >> fun ((b,_),g) -> 
            let t = PredType g in 
            if b then PredNeg t else t 
   )
   ||| (has_possibly(word "not") ++ definite_term 
        >> fun (b,d) -> 
                 let t = PredNoun d in 
                 if b then PredNeg t else t 
       )
  ) input

and has_pred input : predicate parsed = 
  ((and_comma_nonempty_list(article ++ possessed_noun >> snd) 
   >> fun ts -> PredPossessed ts
   )
   ||| (word "no" ++ possessed_noun >> (fun (_,p) -> PredNeg(PredPossessed [p])))
  ) input

and possessed_noun input : term parsed = 
  (attribute left_attribute (prim_possessed_noun (term,general_type)) right_attribute 
   >> fun (l,x,r) -> TPossessedNoun (l,RawTermPattern x,r)) input 

(* terms *)

and definite_term input : term parsed =
  (where_term
   ||| (possibly(word "the") ++ prim_definite_noun (term,general_type) 
        >> fun (_,p) -> TPat p
       )
  ) input 

and term input : term parsed = 
  ((possibly(prim_classifier) ++ definite_term >> snd)
   ||| (any_name >> fun t -> TAnyName t)
  ) input 

and terms input : (term list) parsed = 
  and_comma_nonempty_list term input 

and plain_term input : term parsed = term input 

and quotient_type input : typ parsed = 
  ((word "quotient" ++ possibly(word "of")) ++ general_type ++ word "by" ++ term
   >> fun (((_,g),_),t) -> TyQuotient (g,t)
  )
  input 

and coercion_type input : typ parsed = 
  (a COERCION ++ term >> fun (_,t) -> TyCoerce t) input

and opentail_type input = 
  (binop_type
   ||| quotient_type
   ||| coercion_type
  ) input 

and general_type input : typ parsed = 
  (attribute left_attribute opentail_type right_attribute
   >> fun (l,x,r) -> TyGeneral (l,x,r))
    input 

and left_attribute input : predicate parsed = 
  ((has_possibly(word "non") ++ prim_simple_adjective
   >> fun (b,p) -> if b then PredNeg p else p 
   )
   ||| (prim_simple_adjective_multisubject)
  ) input 

and right_attribute input : predicate parsed = 
  ((and_comma_nonempty_list is_pred >> fun t -> PredRight t)
   ||| (word "that" ++ and_comma_nonempty_list(does_pred) >> fun (_,ls) -> PredRightThat ls)
   ||| (phrase "such that" ++ statement >> fun (_,ls) -> PredRightStatement ls)
  ) input 

and attribute_pseudoterm input : predicate parsed = 
  (attribute left_attribute typed_name_without_attribute right_attribute 
  >> fun (l,x,r) -> PredAttributePseudo (l,x,r)) input

and typed_name_without_attribute input : term parsed = 
  ((prim_typed_name (term,general_type)
   >> fun p -> TPrimTypedName (RawTermPattern p)
  )
   ||| (tvar)
   ||| (prim_classifier ++ tvar >> snd)
   ||| (var ++ (lit_with ++ the_case_sensitive_word "Type") ++ opentail_type
       >> (fun ((v,_),ty) -> TVar (stored_string v,Some ty))
       )
   ||| paren(typed_name_without_attribute)
  ) input

and predicate_pseudoterm input : predicate parsed = 
  (attribute left_attribute plain_pred_pseudoterm right_attribute
  >> (fun (l,(p,ts),r) -> PredPseudo (l,p,ts,r)) 
  ) input

and plain_pred_pseudoterm input : (prop * term list) parsed = 
  opt_paren(tdop_rel_prop ++ holding_var) input

and pseudoterm input : predicate parsed = 
  (attribute_pseudoterm 
   ||| predicate_pseudoterm
  ) input 

and pseudoterms input : (predicate list) parsed = 
  and_comma_nonempty_list (possibly(lit_a) ++ pseudoterm >> snd) input

and any_name input : predicate parsed = 
  ((lit_any 
    ++ 
      (comma_nonempty_list (any_arg) >> List.flatten
      >> (List.map (fun (n,ty) -> TVar (stored_string n,Some ty)))
      )
    >> fun (a,c) -> PredAnyArg (a,c))
   ||| (lit_any ++ pseudoterm
       >> (fun (a,c) -> PredAnyPseudo (a,c))
       )
   ||| (lit_any ++ general_type
       >> (fun (a,c) -> PredAnyGeneral (a,c))
       )
  ) input 

(* end of massive recursion *)
