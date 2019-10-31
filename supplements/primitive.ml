open Type
open Lib

let string_sort = List.sort_uniq String.compare

let _ = string_sort ["the";"THE";"that";"a";"z"]

let synonym = Hashtbl.create 200;;

let syn_add1 (key,value) = 
  let benign = 
    not(Hashtbl.mem synonym key) || 
    (let value' = Hashtbl.find synonym key in 
     (value = value') || failwith ("synonym already declared "^key^" "^value')) in 
      if benign then Hashtbl.add synonym key value

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

let scope_level = 
  function
  | "document" | "article" -> 0
  | "section" | "endsection" -> 1
  | "subsection" | "endsubsection" -> 2
  | "subsubsection" | "endsubsubsection" -> 3
  | "division" | "subdivision" | "enddivision" | "endsubdivision" -> 4
  | s -> failwith ("bad scope_level "^s)


 (* multiword synonyms are stored by hyphenating words together *)

let hyphen = String.concat "-"

let syn_add ts = 
  let ts' = List.map hyphen ts in 
  let ts' = string_sort (List.map String.lowercase_ascii ts') in
  if ts' = [] then ()
  else 
    let value = List.hd ts' in
    ignore (List.map (fun t -> syn_add1 (t,value)) ts')

let find_syn key = 
  try Hashtbl.find synonym key 
  with Not_found -> key


let frozen = 
  List.map syn_add (List.map (fun t -> [[t]]) 
[
"a";"an";"all";"and";"any";"are";"as";"assume";"be";"by";
"case";"classifier";"classifiers"; 
"coercion";"conjecture";"contradiction";"contrary";"corollary";"def";
"define";"defined";"definition";"denote";"division";"do";"document";
"does";"dump";"each";"else";"end";"enddivision";"endsection";
"endsubdivision";"endsubsection";"endsubsubsection";"equal";
"equation";"error";"enter";"every";"exhaustive";"exist";"exists";"exit";
"false";"fix";"fixed";"for";"forall";"formula";"fun";"function";"has";"have";
"having";"hence";"holding";"hypothesis";"if";"iff";"implements";"in";"inferring";
"indeed";"induction";"inductive";"introduce";"is";"it";"left";"lemma";
"let";"library";"make";"map";"match";"moreover";"namespace";
"no";"not";"notational";"notation";
"notationless";"obvious";"of";"off";"on";"only";"ontored";"or";"over";
"pairwise";"parameter";"parameters";"precedence";"predicate";"printgoal";
"proof";"prop";"properties";"property";"prove";"proposition";"propositions";
"propped";"qed";"quotient";"read";"record";"register";"recursion";"right";
"said";"say";"section";"show";"some";"stand";"structure";"subdivision";
"subsection";"subsubsection";"such";"suppose";"synonyms";"take";"that";
"the";"then";"theorem";"there";"therefore";"thesis";"this";"timelimit";
"to";"total";"trivial";"true";"type";"types";"unique";"us";
"warning";"we";"well";"welldefined";"well_defined";"well_propped";
"where";"with";"write";"wrong";"yes";
])

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
  | Prim_term_op (scope,_,_,_) -> scope
  | Prim_binary_relation_op (scope,_,_,_) -> scope
  | Prim_propositional_op (scope,_,_,_,_,_ ) -> scope
  | Prim_relation (scope,_,_,_ ) -> scope
  | Prim_term_var (scope,_,_) -> scope
  | Prim_type_var (scope,_) -> scope 
  | Prim_prop_var (scope,_) -> scope

let prim_find_inscope tbl key = 
  let vs = Hashtbl.find_all tbl key in
  List.filter (fun v -> inscope (prim_scope v)) vs

let prim_add tbl (key,value) = 
  warn (not (prim_find_inscope tbl key = []))
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

let prim_node_in_scope tbl key =
  List.mem key 
  (List.map prim_node (prim_find_inscope tbl key))

let prim_string_in_scope tbl key = 
  List.mem key
  (List.map prim_string (prim_find_inscope tbl key))

(*  key=node for prim_node primitives *)

let prim_identifier_term_tbl = Hashtbl.create 200

let prim_identifier_term_exists key =
  prim_node_in_scope prim_identifier_term_tbl key

let prim_identifier_type_tbl = Hashtbl.create 50

let prim_identifier_type_exists key =
  prim_node_in_scope prim_identifier_type_tbl key

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

