open Lexer_cnl
open Type
open Lib


(* synonyms *)

let string_sort = List.sort_uniq String.compare

let _ = string_sort ["the";"THE";"that";"a";"z"]


let synonym = Hashtbl.create 200;;

let find_all_syn key = 
  Hashtbl.find_all synonym key 


 (* Now, only single-word synonyms are allowed.

   DEPRECATED: canonical forms of synonyms are stored by hyphenating words together.

   let hyphen = String.concat "-"  
  *)

let syn_add1 (key,value) = (* key = string , value = canonical , was [remaining],hyphen-canonical *)
  let ls = Hashtbl.find_all synonym key in 
(*  let ls = List.filter (fun l -> fst value = fst l) ls in *)
  if not(ls = [])
  then warn true 
(*         (not(snd(List.hd ls') = snd value))  *)
         (Printf.sprintf "synonym %s already exists for %s" (List.hd ls)   key)
  else Hashtbl.add synonym key value

let syn_add ts = 
  let hs = (* List.map hyphen *) List.map singularize ts in 
  let hs = List.sort_uniq String.compare hs in
  if hs = [] then ()
  else 
    let value = List.hd hs in
    ignore (List.map (fun key -> syn_add1 (key,value)) hs)

(*
  let benign = 
    not(Hashtbl.mem synonym key) || 

     (value = value') || failwith ("synonym already declared "^key^" "^value')) in 
      if benign then 
 *)

(* 
let expanded_word s input = 
  let u = find_syn (singularize s) in 
  try 
    let (a,(_,rest)) = some_nodeX 
      (function 
       | WORD (w,wu) as w' -> 
           let wsyns = find_all_syn wu in
           if wsyns = [] then (
           ((wsyn = u),WORD (w,wsyn))
       | VAR v -> 
           let (b,v') = is_word v in 
           (b && v'=u),WORD(v,v')
       | t -> (false,t)) input in 
    (a,(TrString ("matched:"^u),rest))
  with 
    Noparse _ -> raise (Noparse (TrGroup (("expected:"^u),trPos input)))
 *)

(*
let rec match_syn f ss input = (* syn expanded word *)
  if ss = [] then (f,(TrEmpty,input))
  else 
    let ss_null,ss_pos = partition (fun t -> fst t = []) ss in 
    let f = (match ss_null with 
             | [] -> f
             | (_,r):: _ -> Some r) in 
    let ({ tok = w'; _ },(_,input')) = anyword input in 
    match w' with 
    | WORD (_,v') ->
        let ss'_pos = List.filter (fun (ws,_) -> v' = List.hd ws) ss_pos in 
        if ss'_pos = [] then (f,(TrEmpty,input))
        else match_syn f (map (fun (ws,h) -> List.tl ws,h) ss'_pos) input' 
    | _ -> (f,(TrEmpty,input))
 *)



(* scope *)

let current_scope = ref [""]

let inscope scope = 
  let curscope = !current_scope in
  let is = List.length scope in
  let ic = List.length curscope in 
  if is > ic then false
  else 
    let curscope' = snd(chop_list (ic - is) curscope) in 
    (scope = curscope')

let string_of_scope = 
  String.concat "." (List.rev !current_scope)

let is_scope_end =
  function
  | "endsection" | "endsubsection" | "endsubsubsection" | "enddivision" -> true
  | _ -> false

let (doc_scope,sec_scope,sub_scope,subsub_scope,div_scope) = (0,1,2,3,4)

let scope_level = 
  function
  | "document" | "article" -> doc_scope
  | "section" | "endsection" -> sec_scope
  | "subsection" | "endsubsection" -> sub_scope
  | "subsubsection" | "endsubsubsection" -> subsub_scope
  | "division" | "subdivision" | "enddivision" | "endsubdivision" -> div_scope
  | s -> failwith ("bad scope_level "^s)

let set_current_scope (label,scope_end,new_level) = 
    if not(scope_end) then 
      if (div_scope <= new_level) then 
        current_scope := label :: !current_scope
      else 
        current_scope := label :: (pad (new_level) "" !current_scope)
    else (* scope_end *) 
      if (div_scope <= new_level) then 
        current_scope := List.tl (cutat (fun s -> (label="" || label = s)) !current_scope)
      else 
        let _ = new_level < List.length !current_scope || 
                  failwith "ending division that was not started" in 
        let ts = pad (new_level + 1) "" !current_scope in 
        let _ = (List.hd ts = label) || failwith "ending division does not match start" in
        current_scope := List.tl ts 



(* primitives *)

(* let prim_tbl = Hashtbl.create 1000 *) (* global table *)

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
  | Prim_type_word (scope,_,_,_) -> scope
  | Prim_term_op (scope,_,_,_) -> scope
  | Prim_binary_relation_op (scope,_,_,_) -> scope
  | Prim_propositional_op (scope,_,_,_,_,_ ) -> scope
  | Prim_relation (scope,_,_,_ ) -> scope
  | Prim_term_var (scope,_,_) -> scope
  | Prim_type_var (scope,_) -> scope 
  | Prim_prop_var (scope,_) -> scope

let prim_find_all_inscope tbl key = 
  let vs = Hashtbl.find_all tbl key in
  List.filter (fun v -> inscope (prim_scope v)) vs

let prim_add tbl (key,value) = 
  warn (not (prim_find_all_inscope tbl key = []))
    ("primitive already declared: "^key); 
    Hashtbl.add tbl key value

let prim_node = function
  | Prim_term_op_controlseq (_,node,_,_,_,_,_) -> node
  | Prim_binary_relation_controlseq (_,node,_,_,_,_) -> node
  | Prim_propositional_op_controlseq (_,node,_,_,_,_,_ ) -> node
  | Prim_type_op_controlseq (_,node,_,_,_) -> node
  | Prim_term_controlseq (_,node,_,_,_ ) -> node
  | Prim_type_controlseq (_,node,_,_,_ ) -> node
  | Prim_lambda_binder (_,node,_ ) -> node
  | Prim_pi_binder (_,node,_ ) -> node
  | Prim_binder_prop (_,node,_ ) -> node
  | Prim_identifier_term (_,node,_,_) -> node
  | Prim_identifier_type (_,node,_,_) -> node
  | Prim_type_op (_,node,_,_) -> node
  | Prim_binary_relation_op (_,node,_,_) -> node
  | Prim_propositional_op (_,node,_,_,_,_ ) -> node
  | _ -> failwith "prim_node: node expected" 

let prim_string = function
  | Prim_term_var (_,s,_) -> s
  | Prim_type_var (_,s) -> s
  | Prim_prop_var (_,s) -> s
  | _ -> failwith "prim_string: string expected" 

let prim_pattern = function 
  | Prim_typed_name (_,pattern,_,_) -> pattern
  | Prim_relation (_,pattern,_,_ ) -> pattern
  | Prim_adjective_multisubject (_,pattern,_,_) -> pattern
  | Prim_simple_adjective (_,pattern,_,_) -> pattern
  | Prim_simple_adjective_multisubject (_,pattern,_,_) -> pattern
  | Prim_definite_noun (_,pattern,_,_) -> pattern
  | Prim_possessed_noun (_,pattern,_,_) -> pattern
  | Prim_verb (_,pattern,_,_ ) -> pattern
  | Prim_verb_multisubject (_,pattern,_,_) -> pattern
  | Prim_structure (_,pattern,_,_ ) -> pattern
  | Prim_term_op (_,pattern,_,_) -> pattern
  | _ -> failwith "prim_pattern: pattern expected"

let prim_node_in_scope tbl key =
  List.mem key 
  (List.map prim_node (prim_find_all_inscope tbl key))

let prim_string_in_scope tbl key = 
  List.mem key
  (List.map prim_string (prim_find_all_inscope tbl key))

(*  key=node for prim_node primitives *)

let prim_identifier_term_tbl = Hashtbl.create 200

let prim_identifier_term_exists key =
  prim_node_in_scope prim_identifier_term_tbl key

let prim_identifier_type_tbl = Hashtbl.create 50

let prim_identifier_type_exists key =
  prim_node_in_scope prim_identifier_type_tbl key

let prim_structure_tbl = Hashtbl.create 50

let prim_structure_exists key =
  prim_node_in_scope prim_structure_tbl key

let prim_relation_tbl = Hashtbl.create 50

let prim_relation_exists key =
  prim_node_in_scope prim_relation_tbl key

let prim_type_controlseq_tbl = Hashtbl.create 100

let prim_type_controlseq_exists key = 
  prim_node_in_scope prim_type_controlseq_tbl key

let prim_term_var_tbl = Hashtbl.create 200

let prim_term_var_exists key = 
  prim_string_in_scope prim_term_var_tbl key

let prim_type_var_tbl = Hashtbl.create 100

let prim_type_var_exists key = 
  prim_string_in_scope prim_type_var_tbl key

let prim_prop_var_tbl = Hashtbl.create 100

let prim_prop_var_exists key = 
  prim_string_in_scope prim_prop_var_tbl key

let prim_lambda_binder_tbl = Hashtbl.create 100

let prim_lambda_binder_exists key = 
  prim_string_in_scope prim_lambda_binder_tbl key

let prim_term_op_tbl = Hashtbl.create 100

let prim_term_op_exists key = 
  prim_string_in_scope prim_term_op_tbl key

let prim_term_op_controlseq_tbl = Hashtbl.create 100

let prim_term_op_controlseq_exists key = 
  prim_string_in_scope prim_term_op_controlseq_tbl key

let prim_binary_relation_tbl = Hashtbl.create 100

let prim_binary_relation_exists key = 
  prim_string_in_scope prim_binary_relation_tbl key

let prim_binary_relation_controlseq_tbl = Hashtbl.create 100

let prim_binary_relation_controlseq_exists key = 
  prim_string_in_scope prim_binary_relation_controlseq_tbl key

let prim_propositional_op_tbl = Hashtbl.create 100

let prim_propositional_op_exists key = 
  prim_string_in_scope prim_propositional_op_tbl key

let prim_propositional_op_controlseq_tbl = Hashtbl.create 100

let prim_propositional_op_controlseq_exists key = 
  prim_string_in_scope prim_propositional_op_controlseq_tbl key

let prim_binder_prop_tbl = Hashtbl.create 100

let prim_binder_prop_exists key = 
  prim_string_in_scope prim_binder_prop_tbl key

let prim_pi_binder_tbl = Hashtbl.create 100

let prim_pi_binder_exists key = 
  prim_string_in_scope prim_pi_binder_tbl key

let prim_type_op_tbl = Hashtbl.create 100

let prim_type_op_exists key = 
  prim_string_in_scope prim_type_op_tbl key

let prim_type_op_controlseq_tbl = Hashtbl.create 100

let prim_type_op_controlseq_exists key = 
  prim_string_in_scope prim_type_op_controlseq_tbl key

let prim_classifier_tbl = Hashtbl.create 100

let prim_classifier_exists key = 
  prim_string_in_scope prim_classifier_tbl key

let prim_simple_adjective_tbl = Hashtbl.create 100

let prim_simple_adjective_exists key = 
  prim_string_in_scope prim_simple_adjective_tbl key

let prim_simple_adjective_multisubject_tbl = Hashtbl.create 100

let prim_simple_adjective_multisubject_exists key = 
  prim_string_in_scope prim_simple_adjective_multisubject_tbl key

let prim_verb_tbl : (string,prim) Hashtbl.t  = Hashtbl.create 100

let prim_verb_multisubject_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_possessed_noun_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_definite_noun_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_adjective_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_adjective_multisubject_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_typed_name_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let frozen = 
  List.map syn_add (List.map (fun t -> [t]) 
[
"a";"an";"all";"and";"any";"are";"as";"assume";"be";"by";
"case";"classifier";
"coercion";"conjecture";"contradiction";"contrary";"corollary";"def";
"define";"defined";"definition";"denote";"division";"do";"document";
"does";"dump";"each";"else";"end";"enddivision";"endsection";
"endsubdivision";"endsubsection";"endsubsubsection";"equal";
"equation";"error";"enter";"every";"exhaustive";"exist";"exit";
"false";"fix";"fixed";"for";"forall";"formula";"fun";"function";"has";"have";
"having";"hence";"holding";"hypothesis";"if";"iff";"in";"inferring";
"indeed";"induction";"inductive";"introduce";"is";"it";"left";"lemma";
"let";"library";"make";"map";"match";"moreover";"namespace";
"no";"not";"notational";"notation";
"notationless";"obvious";"of";"off";"on";"only";"ontored";"or";"over";
"pairwise";"parameter";"precedence";"predicate";"printgoal";
"proof";"prop";"property";"prove";"proposition";
"propped";"qed";"quotient";"read";"record";"register";"recursion";"right";
"said";"say";"section";"show";"some";"stand";"structure";"subdivision";
"subsection";"subsubsection";"such";"suppose";"synonym";"take";"that";
"the";"then";"theorem";"there";"therefore";"thesis";"this";"timelimit";
"to";"total";"trivial";"true";"type";"unique";"us";
"warning";"we";"well";"welldefined";"well_defined";"well_propped";
"where";"with";"write";"wrong";"yes";

(* plural handled by desing "classifiers"; "exists";"implement";
   "parameters";"properties";"propositions";"synonyms";"types";
 *)
])

let phrase_list_transition_words =
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
     ]

let preposition_list = 
[
  "aboard";"about";"above";"according\\~to"; "across"; "against"; "ahead\\~of";
  "along";"alongside";"amid";"amidst";"among";"around";"at";"atop";"away\\~from";
  "before";
  "behind";"below";"beneath";"beside";"between";"beyond";"by";"concerning";"despite";
  "except";"except\\~at";"excluding";"following";
  "from";"in";"in\\~addition\\~to";"in\\~place\\~of";"in\\~regard\\~to";
  "inside";"instead\\~of";"into";"near";"next\\~to";"of";
  "off";"on";"on\\~behalf\\~of";"on\\~top\\~of";"onto";"opposite";"out";"out\\~of";
  "outside";"outside\\~of";
  "over";"owing\\~to";"per";"prior\\~to";"regarding";"save";"through";
  "throughout";"till";"to";"towards";"under";"until";
  "up";"up\\~to";"upon";"with";"with\\~respect\\~to";"within";"without"

(* 
   "for"; "as"; "like"; "after"; "round"; "plus"; "since"; "than"; "past"; 
   "during"; 

  synonyms with\~respect\~to/wrt 

 *)
]

let prim_list = 
  [
  "prim_classifier";
  "prim_term_op_controlseq";
  "prim_binary_relation_controlseq";
  "prim_propositional_op_controlseq";
  "prim_type_op_controlseq";
  "prim_term_controlseq";
  "prim_type_controlseq";
  "prim_lambda_binder";
  "prim_pi_binder";
  "prim_binder_prop";
  "prim_typed_name";
  "prim_adjective";
  "prim_adjective_multisubject";
  "prim_simple_adjective";
  "prim_simple_adjective_multisubject";
  "prim_definite_noun";
  "prim_identifier_term";
  "prim_identifier_type";
  "prim_possessed_noun";
  "prim_verb";
  "prim_verb_multisubject";
  "prim_structure";
  "prim_type_op";
  "prim_type_word";
  "prim_term_op";
  "prim_binary_relation_op";
  "prim_propositional_op";
  "prim_relation"
  ]
