open Lexer_cnl
open Type
open Lib


(* synonyms *)

let string_sort = List.sort_uniq String.compare

let _ = string_sort ["the";"THE";"that";"a";"z"]


let synonym = Hashtbl.create 200;;

let find_all_syn key = 
  Hashtbl.find_all synonym key 

let find_head_syn key = 
  match find_all_syn key with
  | [] -> key
  | s :: _ -> s

 (* Now, only single-word synonyms are allowed.

   DEPRECATED: canonical forms of synonyms are stored by hyphenating words together.

   let hyphen = String.concat "-"  
  *)

let syn_add1 (key,value) = (* key = string , value = canonical , was [remaining],hyphen-canonical *)
  let ls = Hashtbl.find_all synonym key in 
  if not(ls = [])
  then warn true 
         (Printf.sprintf "synonym %s already exists for %s" (List.hd ls)   key)
  else Hashtbl.add synonym key value

let syn_add ts = 
  let hs = (* List.map hyphen *) List.map singularize ts in 
  let hs = List.sort_uniq String.compare hs in
  if hs = [] then ()
  else 
    let value = List.hd hs in
    ignore (List.map (fun key -> syn_add1 (key,value)) hs)

(* scope *)

let mk_meta =
  let i = ref 0 in 
  fun () ->
        let () = i := !i + 1 in 
         (!i)

let scope_current = ref []

let get_current_scope() = !scope_current 

(* Length of scope is the length of this list.
   With no document, the list is empty. 
   When the document starts, the first item is the doc name and the list has length 1.
   When a section starts, the head is the section name, and the list has length 2.

  divisions and subdivisions can be at any position of the list, and are treated
  as a special case with "length" -1. 

  An import is placed in the document (that is global) scope. 
  Nonglobal defs from import are flattened (placed at the section scope no matter their actual scope).

  We haven't implemented namespaces in a significant way.
 *)

let scope_length = List.length !scope_current

 (* an object of given scope is "inscope" if it is a tailing sub scope of scope_current. 
  *)

let inscope scope = 
  let sc = !scope_current in
  let is = List.length scope in
  let ic = scope_length in 
  (is <= ic) && (scope = snd(chop_list (ic - is) sc))

let string_of_scope = 
  String.concat "." (List.rev !scope_current)

let is_scope_end s = 
  String.sub s 0 3 = "end"

let (doc_scope,sec_scope,sub_scope,subsub_scope,div_scope) = (1,2,3,4,-1)

let scope_length = 
  (function
  | "document" | "article" -> doc_scope
  | "section" | "endsection" -> sec_scope
  | "subsection" | "endsubsection" -> sub_scope
  | "subsubsection" | "endsubsubsection" -> subsub_scope
  | "division" | "subdivision" | "enddivision" | "endsubdivision" -> div_scope
  | s -> failwith ("bad scope_length "^s))

let set_end_unlabeled_scope_current new_length = 
  try 
    if (new_length < 0) then 
      scope_current := List.tl !scope_current
    else 
      scope_current := pad (new_length) "" !scope_current 
  with Failure _ -> failwith ("set_end_unlabeled_scope_current:tl")

let set_end_labeled_scope_current (label,new_length) = 
  try 
    if (new_length < 0) then (* div_scope *)
      scope_current := List.tl (cutat (fun s -> (label = s)) !scope_current)
    else (* both label and new_length must match *)
      let ts = pad (new_length + 1) "" !scope_current in 
      let _ = (List.hd ts = label) || failwith "ending division does not match start" in
      scope_current := List.tl ts 
  with Failure _ -> failwith ("set_end_labeled_scope_current:tl " ^ string_of_scope)

let set_start_scope_current (new_length,label) = (* label becomes hd of list of new length *)
  if (div_scope < 0) then 
    scope_current := label :: !scope_current
  else 
    let new_scope = label :: (pad (new_length - 1) "" !scope_current) in 
    let _ = List.length new_scope = new_length || failwith "set_start_scope_current:length" in
    scope_current := new_scope

let set_scope_current(is_end,new_length,label) = 
  match (is_end,label) with
  | (true,"") -> set_end_unlabeled_scope_current new_length 
  | (true,_) -> set_end_labeled_scope_current(label,new_length)
  | (false,_) -> set_start_scope_current(new_length,label)

(* primitives *)

(* let prim_tbl = Hashtbl.create 1000 *) (* global table *)

(* let prim_add (key,value) =  *)
  

let prim_scope = 
  function 
  | Prim_classifier (scope,_) -> scope 
  | Prim_term_op_controlseq (scope,_,_,_,_) -> scope
  | Prim_binary_relation_controlseq (scope,_,_,_) -> scope
  | Prim_propositional_op_controlseq (scope,_,_,_,_) -> scope
  | Prim_type_op_controlseq (scope,_,_,_) -> scope
  | Prim_term_controlseq (scope,_,_) -> scope
  | Prim_type_controlseq (scope,_,_) -> scope
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
  | Prim_propositional_op (scope,_,_,_,_) -> scope
  | Prim_relation (scope,_,_,_ ) -> scope
  | Prim_term_var (scope,_,_) -> scope
  | Prim_type_var (scope,_) -> scope 
  | Prim_prop_var (scope,_) -> scope
  | Prim_field_prop_accessor(scope,_) -> scope
  | Prim_field_term_accessor(scope,_,_) -> scope
  | Prim_field_type_accessor(scope,_) -> scope

let prim_string = function
  | Prim_term_op_controlseq (_,Pat_controlseq(string,_),_,_,_) -> string
  | Prim_binary_relation_controlseq (_,Pat_controlseq(string,_),_,_) -> string
  | Prim_propositional_op_controlseq (_,Pat_controlseq(string,_),_,_,_) -> string
  | Prim_type_op_controlseq (_,Pat_controlseq(string,_),_,_) -> string
  | Prim_term_controlseq (_,Pat_controlseq(string,_),_) -> string
  | Prim_type_controlseq (_,Pat_controlseq(string,_),_) -> string
  | Prim_lambda_binder (_,string,_ ) -> string
  | Prim_pi_binder (_,string,_ ) -> string
  | Prim_binder_prop (_,string,_ ) -> string
  | Prim_identifier_term (_,string,_,_) -> string
  | Prim_identifier_type (_,string,_,_) -> string
  | Prim_type_op (_,string,_,_) -> string
  | Prim_binary_relation_op (_,string,_,_) -> string
  | Prim_propositional_op (_,string,_,_,_) -> string
  | Prim_term_var (_,s,_) -> s
  | Prim_type_var (_,s) -> s
  | Prim_prop_var (_,s) -> s
  | _ -> failwith "prim_string: string expected" 

let prim_pattern = function 
  | Prim_term_op_controlseq (_,pattern,_,_,_) -> pattern
  | Prim_binary_relation_controlseq (_,pattern,_,_) -> pattern
  | Prim_propositional_op_controlseq (_,pattern,_,_,_) -> pattern
  | Prim_type_op_controlseq (_,pattern,_,_) -> pattern
  | Prim_term_controlseq (_,pattern,_) -> pattern
  | Prim_type_controlseq (_,pattern,_) -> pattern

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

let prim_find_all_inscope tbl key = 
  let vs = Hashtbl.find_all tbl key in
  List.filter (fun v -> inscope (prim_scope v)) vs

let prim_add tbl (key,value) = 
  warn (not (prim_find_all_inscope tbl key = []))
    ("prim_add failure; primitive already declared: "^key); 
    Hashtbl.add tbl key value

let prim_add_force tbl (key,value) = 
  Hashtbl.add tbl key value

(*
let prim_string_in_scope tbl key =
  List.mem key 
  (List.map prim_string (prim_find_all_inscope tbl key))
 *)

(* string = key, so there is no need to double check *)

let prim_string_in_scope tbl key =
  opt_list(prim_find_all_inscope tbl key)

let prim_string_in_scope_exists tbl key =
  prim_find_all_inscope tbl key <> []

let prim_identifier_term_tbl : (string,prim) Hashtbl.t = Hashtbl.create 200

let prim_identifier_term_option key =
  prim_string_in_scope prim_identifier_term_tbl key

let prim_identifier_type_tbl : (string,prim) Hashtbl.t = Hashtbl.create 50

let prim_identifier_type_option key =
  prim_string_in_scope prim_identifier_type_tbl key

let prim_structure_tbl : (string,prim) Hashtbl.t = Hashtbl.create 50

let prim_structure_option key =
  prim_string_in_scope prim_structure_tbl key

let prim_relation_tbl : (string,prim) Hashtbl.t = Hashtbl.create 50

let prim_relation_option key =
  prim_string_in_scope prim_relation_tbl key

let prim_term_controlseq_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_term_controlseq_option key = 
  prim_string_in_scope prim_term_controlseq_tbl key

let prim_type_controlseq_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_type_controlseq_option key = 
  prim_string_in_scope prim_type_controlseq_tbl key

let prim_term_var_tbl : (string,prim) Hashtbl.t = Hashtbl.create 200

let prim_term_var_option key = 
  prim_string_in_scope prim_term_var_tbl key

let prim_type_var_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_type_var_exist key = 
  prim_string_in_scope_exists prim_type_var_tbl key

let prim_prop_var_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_prop_var_option key = 
  prim_string_in_scope prim_prop_var_tbl key

let prim_lambda_binder_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_lambda_binder_option key = 
  prim_string_in_scope prim_lambda_binder_tbl key

let prim_term_op_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_term_op_option key = 
  prim_string_in_scope prim_term_op_tbl key

let prim_term_op_controlseq_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_term_op_controlseq_option key = 
  prim_string_in_scope prim_term_op_controlseq_tbl key

let prim_binary_relation_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_binary_relation_option key = 
  prim_string_in_scope prim_binary_relation_tbl key

let prim_binary_relation_controlseq_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_binary_relation_controlseq_option key = 
  prim_string_in_scope prim_binary_relation_controlseq_tbl key

let prim_propositional_op_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_propositional_op_option key = 
  prim_string_in_scope prim_propositional_op_tbl key

let prim_propositional_op_controlseq_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_propositional_op_controlseq_option key = 
  prim_string_in_scope prim_propositional_op_controlseq_tbl key

let prim_binder_prop_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_binder_prop_option key = 
  prim_string_in_scope prim_binder_prop_tbl key

let prim_pi_binder_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_pi_binder_option key = 
  prim_string_in_scope prim_pi_binder_tbl key

let prim_type_op_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_type_op_option key = 
  prim_string_in_scope prim_type_op_tbl key

let prim_type_op_controlseq_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_type_op_controlseq_option key = 
  prim_string_in_scope prim_type_op_controlseq_tbl key

let prim_classifier_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_classifier_find_all_inscope key = 
  prim_find_all_inscope prim_classifier_tbl key

let prim_simple_adjective_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_simple_adjective_option key = 
  prim_string_in_scope prim_simple_adjective_tbl key

let prim_simple_adjective_multisubject_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_simple_adjective_multisubject_option key = 
  prim_string_in_scope prim_simple_adjective_multisubject_tbl key

let prim_field_term_accessor_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_field_term_accessor_option key = 
  prim_string_in_scope prim_field_term_accessor_tbl key

let prim_field_type_accessor_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_field_type_accessor_option key = 
  prim_string_in_scope prim_field_type_accessor_tbl key

let prim_field_prop_accessor_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_field_prop_accessor_option key = 
  prim_string_in_scope prim_field_type_accessor_tbl key



let prim_verb_tbl : (string,prim) Hashtbl.t  = Hashtbl.create 100

let prim_verb_multisubject_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_possessed_noun_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_definite_noun_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_adjective_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_adjective_multisubject_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_typed_name_tbl : (string,prim) Hashtbl.t = Hashtbl.create 100

let prim_used_tbl : (string,unit) Hashtbl.t = Hashtbl.create 500

let prim_defined_tbl : (string,unit) Hashtbl.t = Hashtbl.create 500


let frozen_list = 
[
"a";"an";"all";"and";"any";"are";"as";"assume";"be";"by";
"case";"classifier";
"coercion";"conjecture";"contradiction";"contrary";"corollary";"declare";
"def";
"define";"defined";"definition";"denote";"division";"do";"document";
"does";"dump";"each";"else";"end";"enddivision";"endsection";
"endsubdivision";"endsubsection";"endsubsubsection";"equal";
"equation";"error";"enter";"every";"exhaustive";"exist";"exit";
"false";"fix";"fixed";"for";"forall";"formula";"fun";"function";"has";"have";
"having";"hence";"holding";"hypothesis";"if";"iff";"in";"inferring";
"indeed";"induction";"inductive";"introduce";"is";"it";"left";"lemma";
"let";"library";"make";"map";"match";"moreover";"mutual";"namespace";
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
]

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
  "aboard";"about";"above";"according to"; "across"; "against"; "ahead of";
  "along";"alongside";"amid";"amidst";"among";"around";"at";"atop";"away from";
  "before";
  "behind";"below";"beneath";"beside";"between";"beyond";"by";"concerning";"despite";
  "except";"except at";"excluding";"following";
  "from";"in";"in addition to";"in place of";"in regard to";
  "inside";"instead of";"into";"near";"next to";"of";
  "off";"on";"on behalf of";"on top of";"onto";"opposite";"out";"out of";
  "outside";"outside of";
  "over";"owing to";"per";"prior to";"regarding";"save";"through";
  "throughout";"till";"to";"towards";"under";"until";
  "up";"up to";"upon";"with";"with respect to";"wrt";"within";"without"

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
  "prim_field_term_accessor";
  "prim_field_type_accessor";
  "prim_field_prop_accessor";
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


let syn_add_frozen() = 
  List.map syn_add (List.map (fun t -> [t]) frozen_list);;

let add_used_word ls = 
  let m = List.map (fun s -> String.split_on_char ' ' s) ls in
  let m = List.flatten m in 
  let _ = List.map (fun t -> Hashtbl.add prim_defined_tbl t ()) m in 
  ()

let add_master_list() = 
    add_used_word (frozen_list @ preposition_list)

let rec process_wp_syn = 
  function 
  | Wp_synonym ss :: tl -> 
      let _ = List.length ss > 1 || 
                failwith ("process_wp_syn: at least 2 words needed in "^String.concat " " ss) in 
      syn_add ss; process_wp_syn tl
  | Wp_list (c,ws) :: tl -> 
      let ws' = process_wp_syn ws in 
      let tl' = process_wp_syn tl in 
      if ws' = [] then tl' else Wp_list(c,ws') :: tl'
  | t :: tl  -> t :: process_wp_syn tl
  | [] -> []

let get_fiat_var = 
  function
  | Wp_var (CVar(s,ExNone,false)) -> 
      (match s with
      | "t" -> Pat_var_term
      | "T" -> Pat_var_type
      | "p" -> Pat_proof
      | "P" -> Pat_var_prop
      | _ -> failwith ("get_fiat_var:bad variable name "^s)
      ) 
  | wp -> failwith ("get_fiat_var:bad variable pattern "^show_wordpattern wp)

let convert_fiat_pat = 
  function 
  | Wp_var _ as w -> get_fiat_var w 
  | Wp_list(Wpc_cs,Wp_csname s :: w) -> 
      let w' = List.map get_fiat_var w in 
      Pat_controlseq(s,w')
  | _ -> failwith ("convert_fiat_pat")

(* The patterns are set up currently to start at the very
   beginning, including material before the key. *)
let process_fiat wp = 
  let prim_pat tl = 
    Pat_sequence (List.map convert_fiat_pat tl) in     
  let rec first_csname  = 
    function 
    | Wp_list(_,Wp_csname s :: _)::_ -> s 
    | _ :: tl -> first_csname tl 
    | [] -> failwith "first_csname not found" in 
  let rec first_word  = 
    function 
    | Wp_wd s :: _ -> s 
    | _ :: tl -> first_word tl 
    | [] -> failwith "first_word not found" in 

  
  let sc = get_current_scope() in 
  let wps' = process_wp_syn [wp] in
  if wps' = [] then () 
  else match List.hd wps' with 
  | Wp_classifier cs -> 
      ignore(List.map 
               (fun ss -> 
                      let s = String.concat " " (safetail ss) in 
                      let _ = ss <> [] || failwith ("empty classifier "^s) in 
                      prim_add_force prim_classifier_tbl (List.hd ss,Prim_classifier(sc,s))) 
               cs)
  | Wp_list(Wpc_term_op_controlseq,Wp_prec(i,p)::tl) ->
      let pat = prim_pat tl in 

      prim_add prim_term_op_controlseq_tbl (first_csname tl,Prim_term_op_controlseq(sc,pat,(i,p),Blank,[]))
  | Wp_list(Wpc_binary_relation_controlseq,tl) -> 
      let pat = prim_pat tl in 
      prim_add prim_binary_relation_controlseq_tbl
        (first_csname tl,Prim_binary_relation_controlseq(sc,pat,PNone,[]))
  | Wp_list(Wpc_propositional_op_controlseq,Wp_prec(i,p)::tl) ->
      let pat = prim_pat tl in 
      prim_add prim_propositional_op_controlseq_tbl
        (first_csname tl,Prim_propositional_op_controlseq(sc,pat,(i,p),PNone,[]))
  | Wp_list(Wpc_type_op_controlseq,tl) ->
      let pat = prim_pat tl in 
      prim_add prim_type_op_controlseq_tbl
        (first_csname tl,Prim_type_op_controlseq(sc,pat,Blank,[]))
  | Wp_list(Wpc_term_controlseq,Wp_prec(_,_) :: tl) -> 
      let tl' = List.map convert_fiat_pat tl in 
      prim_add prim_term_controlseq_tbl
      (first_csname tl,Prim_term_controlseq(sc,Pat_sequence tl',Blank))
  | Wp_list(Wpc_type_controlseq,tl) -> 
      let tl' = List.map convert_fiat_pat tl in 
      prim_add prim_type_controlseq_tbl
      (first_csname tl,Prim_type_controlseq(sc,Pat_sequence tl',TyNone))
  | Wp_list(Wpc_lambda_binder,tl) -> 
      let s = first_csname tl in
      prim_add prim_lambda_binder_tbl (s,Prim_lambda_binder(sc,s,Blank))
  | Wp_list(Wpc_pi_binder,tl) -> 
      let s = first_csname tl in
      prim_add prim_pi_binder_tbl (s,Prim_pi_binder(sc,s,TyNone))
  | Wp_list(Wpc_binder_prop,tl) -> 
      let s = first_csname tl in
      prim_add prim_binder_prop_tbl (s,Prim_binder_prop(sc,s,PNone))
  | Wp_list(Wpc_adj,tl) ->
      let s = first_word tl in 
      let pat = prim_pat tl in 
      prim_add prim_adjective_tbl (s,Prim_adjective(sc,pat,PNone,[]))
  | Wp_list(Wpc_adjM,tl) ->
      let s = first_word tl in 
      let pat = prim_pat tl in 
      prim_add prim_adjective_multisubject_tbl 
        (s,Prim_adjective_multisubject(sc,pat,PNone,[]))
  | Wp_list(Wpc_simple_adj,tl) ->
      let s = first_word tl in 
      let pat = prim_pat tl in 
      prim_add prim_simple_adjective_tbl 
        (s,Prim_simple_adjective(sc,pat,PNone,[]))
  | Wp_list(Wpc_simple_adjM,tl) ->
      let s = first_word tl in 
      let pat = prim_pat tl in 
      prim_add prim_simple_adjective_multisubject_tbl 
        (s,Prim_simple_adjective_multisubject(sc,pat,PNone,[]))
  | Wp_list(Wpc_definite_noun,tl) -> 
      let s = first_word tl in 
      let pat = prim_pat tl in 
      prim_add prim_definite_noun_tbl 
        (s,Prim_definite_noun(sc,pat,Blank,[]))
  | Wp_identifier(Wpc_identifier_term,s,_,_) -> 
      prim_add prim_identifier_term_tbl 
        (s,Prim_identifier_term(sc,s,Blank,[]))
  | Wp_identifier(Wpc_identifier_type,s,_,_) -> 
      prim_add prim_identifier_type_tbl 
        (s,Prim_identifier_type(sc,s,TyNone,[]))

(*  
  | Wp_list(Wpc_typed_name,tl)
  | Wp_list(Wpc_possessed_noun,tl)
 *)


(*
  | Wp_list(Wpc_verb,tl) -> 
  | Wp_list(Wpc_verbM,) ->
  | Wp_list(Wpc_structure,tl)
  | Wp_list(Wpc_type_op,tl)
  | Wp_list(Wpc_type_word,tl)
  | Wp_list(Wpc_term_op,tl)
  | Wp_list(Wpc_binary_relation_op,tl)
  | Wp_list(Wpc_propositional_op,tl)
  | Wp_list(Wpc_prim_relation,tl) *)

  | Wp_list(c,_) -> failwith ("process_fiat not installed "^show_wordpattern_class c)
  | _ -> ()

