open Lexer_cnl

type term = 
  | TVar of node*(typ option)
  | TVarAtomic of node*(typ option)
  | Annotated of term*typ 
  | Decimal of node
  | Error'
  | Integer of int (* XX should be BigInt *)
  | String of node
  | Blank
  | Id of node* typ option
  | Unparsed' of node list
  | ControlSeq of node*expr list
  | Make of (node * typ * node list) list 
  | Plain' of node list
  | List of term list
  | Tuple of term list
  | SetEnum of term list
  | Case of (prop*term) list
  | Match of (term list) * (term list * term) list
  | MatchFunction of (node list list) * (node * typ) list * typ * (term list * term) list
  | Comprehension of term * term list * statement
  | FieldAccessor of term * node 
  | ApplySub of term * term list

and typ = 
  | TyVar of node
  | Colon' of node list 
  | Type' of node list
  | TyControlSeq of node * node list list
  | TyId of node 
  | Subtype of term * term list * statement 
  | Structure' of ((node list list * (node * typ) list) * node list list * node list list) 

and prop = 
  | PVar of node
  | Prop' of node list

and statement = 
  | Statement' of node list 
  | LetAnnotation' of node list
  | StateIfThen of statement*statement
  | StateAnd of statement list
  | StateOr of statement list
  | StatePrimary' of node list
  | StateWrong of statement
  | StateIff of statement*statement

and proof = 
  | Proof

and expr = 
  | Eterm of term
  | Etyp of typ
  | Eprop of prop
  | Expr' of node list
[@@deriving show]

type associativity = 
  | AssocLeft
  | AssocRight
  | AssocNone
[@@deriving show]

type this_adj =
  | ThisUnique
  | ThisCanonical
  | ThisWelldefined
  | ThisWellpropped
  | ThisTotal
  | ThisExhaustive 
  | ThisRecursion
  | ThisExist
[@@deriving show]


type wordpattern = 
  | Wp_wd of node
  | Wp_sym of node 
  | Wp_syn of node list 
  | Wp_opt of node 
  | Wp_var of node* typ
  | Wp_fix of node* typ
  | Wp_ty_word of wordpattern list
  | Wp_bin_cs of wordpattern*(node*(wordpattern list))*wordpattern
  | Wp_ty_identifier of node* node list list * ((node * typ) list)
  | Wp_ty_cs of node * wordpattern list
  | Wp_cs of node * wordpattern list
  | Wp_sympat of wordpattern list * int option  * associativity
  | Wp_sympatP of wordpattern list * int option  * associativity
  | Wp_sympatT of wordpattern list
  | Wp_identifier of node* node list list * ((node * typ) list)
  | Wp_identifierP of node* node list list * ((node * typ) list)
  | Wp_fun_word of wordpattern list
  | Wp_notion of wordpattern list
  | Wp_adj of wordpattern list
  | Wp_adjM of wordpattern list
  | Wp_verb of wordpattern list
  | Wp_verbM of wordpattern list
  | Wp_inferring of wordpattern * node list * typ
  | Wp_classifier of node list list
  | Wp_record of node list list * this_adj list
  | Wp_implement of node list * node list list
  | Wp_namespace of node
[@@deriving show]

type scope = string list (* hierarchical identifiers *)
[@@deriving show]

type pos = (Lexing.position * Lexing.position) [@opaque]
[@@deriving show]

(* let show_pos _ _ = "" *)


type prim = 

  | Prim_classifier of scope * string (* phrase *)
  (* all cs primitives are 0-ary or 2-ary *)
  (* cs token, numbraceargs, precedence, assoc, def, free vars  -- always binary *)
  | Prim_term_op_controlseq of scope * token * int * int * associativity * term * term list
  (* cs token, numbraceargs, def, frees *)
  | Prim_binary_relation_controlseq of scope * token * int * prop * term * term list
  (* cs token, numbraceargs, prec, assoc, def, frees *)
  | Prim_propositional_op_controlseq of 
      scope * token * int * int * associativity * prop * term list 
  (* cs token, numbraceargs, def, frees -- alway binary, fixed prec, right assoc. *)
  | Prim_type_op_controlseq of scope * token * int * term * term list
  (* cs token, numbraceargs, def, frees -- only brace args *)
  | Prim_term_controlseq of scope * token * int * term * term list 
  (* cs token, numbraceargs, def, frees *)
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

  (* variables *)
  | Prim_type_var of scope * string 
  | Prim_term_var of scope * string * typ option
  | Prim_prop_var of scope * string 
[@@deriving show]

type text_node = 
  | Section_preamble of pos*int*string (* new current section *)
  | Instruction of pos*string (* keyword *)
  | Axiom of pos*string*string * (statement list)*statement  (* kind,label,statements,conclusion *)
  | Definition of pos*string * (statement list)* (wordpattern * node list) list * this_adj list (* label,statements,conclusion *)
  | Theorem of pos*string * (statement list)*statement  (* label, statements,conclusion *)
  | Synonym of pos
  | Implement of pos*wordpattern
  | Macro of pos* int * (wordpattern * node list) list
  | Namespace
[@@deriving show]

(* exceptions and positions *)

type trace = 
  | TrMany of trace list
  | TrAdd of trace list
  | TrOr of trace list
  | TrGroup of string * trace
  | TrFail of int*int*token 
  | TrData of token list
  | TrString of string 
  | TrEmpty
[@@deriving show]
