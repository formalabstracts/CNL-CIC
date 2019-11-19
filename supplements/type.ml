open Lexer_cnl

type qany = 
  | QAll
  | QSome
  | QNo
[@@deriving show]

type term_class = 
  | List 
  | Tuple
  | SetEnum
  | DefiniteTerm
  | MapsTo
  | TPrimTypedName 
  | TermPostfix
  | TightestTerm 
  | Let 
  | ApplySub 
  | PlainTerm 

and term = 
  | RawTerm of node list
  | RawTermOp of prim
  | RawTdop of term list
  | RawTermPattern of pattern

  | Decimal of string
  | Integer of int (* XX should be BigInt *)
  | String of string 
  | Blank

  | TermClass of term_class * term list 
  | TVar of string * (typ option)
  | Annotated of term * typ 
  | PrimId of prim 
  | ControlSeq of prim * expr list
  | FieldTermAccessor of prim
  | IfThenElse of prop * term * term
  | Make of typ option * (expr * expr * expr) list
  | Where of term * (expr * expr * expr) list
  | App of term * (expr * expr * expr) list * expr list
  | Comprehension of term * term list * statement
  | Case of (prop * term) list
  | Match of (term list) * (term list * term) list

  | MatchFunction of (expr * expr) list * (string * typ) list * typ * (term list * term) list
  | Lambda of prim * (expr * expr) list * expr list * term
  | LambdaFun of (expr * expr) list * expr list * typ * term

  | TAnyName of predicate 
  | TPossessedNoun of predicate list * term * predicate list 

and typ = 
  | RawGeneralType of node list
  | RawPostColon of node list 

  | TyVar of string
  | TyId of prim
  | TyControlSeq of prim * expr list
  | FieldTypeAccessor of term * prim 
  | Subtype of term * term list * statement 
  | TyQuotient of typ * term
  | TyGeneral of predicate list * typ * predicate list 
  | TyCoerce of term 
  | TyAgda of term list 
  | TyApp of typ * (expr * expr * expr) list * expr list
  | TyBinder of prim * (expr * expr) list * expr list * typ 
  | TyOp of prim
  | TyBinop of (expr * expr) list * typ list 

  | Structure of 
      (expr * expr) list * 
        (string * typ) list *
          (string list * expr * expr * expr) list * 
            prop list

  | Inductive of 
      string * 
        (expr * expr) list * 
          (string * typ) list * 
            expr *
              (string * (expr * expr) list * (string * typ) list * typ) list

  | Mutual of 
      string list * 
        (expr * expr) list * 
          (string * typ) list *

            (string * (expr * expr) list * (string * typ) list * expr *
               (string * (expr * expr) list * (string * typ) list * typ) list) list

  | Over of prim * 
              (expr * expr * expr) list * 
                expr list * 
                  (expr * expr * expr) list

and prop = 
  | RawProp of node list 

  | PVar of string
  | FieldPropAccessor of term * prim
  | PStatement of statement 
  | PRel of prim
  | P_ops of term list 
  | Ptdop of prop list
  | Ptdopr of (term * term * term) list
  | PApp of prop * (expr * expr * expr) list * expr list
  | PLambda of (expr * expr) list * expr list * prop
  | PBinder of prim * (expr * expr) list * (string * typ) list * prop

and context = 
  | CVarb of string * expr * bool (* name, type-expr, fixed? *)
  | CVar of string * expr (* name, type-expr *)

and statement_op =
  | StateAndList
  | StateNot
  | StateIfThen
  | StateIff 
  | StateTrue
  | StateFalse 
  | StateOrList

and statement_quantifier =
  | StateForAny
  | StateForall
  | StateExist 

and statement = 
  | RawStatement of node list 

  | LetAnnotation of context list 
  | StateCombination of statement_op * statement list 
  | StateQuantifier of statement_quantifier * predicate list * statement 
  | StateThereExist of bool * predicate list  
  | StateProp of prop
  | StateSimple of term list * predicate list

and predicate_class = 
  | PredPrimAdj 
  | PredPrimAdjMulti
  | PredPrimVerb 
  | PredPrimVerbMulti
  | PredPrimSimpleAdj
  | PredPrimSimpleAdjMulti
  | PredNeg 
  | PredPairwise
  | PredWith
  | PredIsA
  | PredIs
  | PredRightThat
  | PredRight

and predicate = 
  | RawPredPattern of pattern

  | PredPrim of prim 
  | PredCombination of predicate_class * predicate list
  | PredType of typ 
  | PredNoun of term
  | PredPossessed of term list
  | PredRightStatement of statement
  | PredAnyArg of qany * term list
  | PredAnyPseudo of qany * predicate 
  | PredAnyGeneral of qany * typ
  | PredAttributePseudo of predicate list * term * predicate list 
  | PredPseudo of predicate list * prop * term list * predicate list 

and proof = 
  | Proof

and expr = 
  | RawExpr of node list 
  | Raw_Colon_Sortish of node list  

  | Eterm of term
  | Etyp of typ
  | Eprop of prop
  | Eproof
  | EVar of string * expr (* var, its type info *)

and pattern = (* what wordpattern get translated to for parsing *)
  | Pat_var_term
  | Pat_var_type 
  | Pat_var_prop
  | Pat_var_names 
  | Pat_term of term 
  | Pat_type of typ
  | Pat_prop of prop
  | Pat_proof
  | Pat_option of pattern 
  | Pat_sequence of pattern list
  | Pat_word of string
  | Pat_symbol of string 
  | Pat_controlseq of string * pattern list 
  | Pat_names of pattern list

and instruct = 
  | InstructSyn of string list list 
  | InstructCommand of string
  | InstructString of string * string
  | InstructBool of string * bool
  | InstructInt of string * int 

and associativity = 
  | AssocLeft
  | AssocRight
  | AssocNone

and scope = string list (* hierarchical identifiers *)

and precedence = int 

and numbracearg = int 

and prim = 
  (*
    the string field = the key = name of primitive word, controlseq, or symbol. 
    the first int = number of controlseq braceargs
    the second int = precedence level 

    all cs primitives are 0-ary or 2-ary (as infix, not referring to braceargs) 

    Ordering of fields is as follows:
    scope, cs string, numbraceargs, precedence, assoc, def (term,typ,etc. as appropriate), free vars  

   *)

  (* -- controlseq *)
  | Prim_classifier of scope * string (* phrase *)
  | Prim_term_op_controlseq of scope * string * numbracearg * precedence * associativity * term * term list
  | Prim_binary_relation_controlseq of scope * string * numbracearg * prop * term * term list
  | Prim_propositional_op_controlseq of 
      scope * string * numbracearg * precedence * associativity * prop * term list 
  (* -- 2-ary fixed prec, right assoc. *)
  | Prim_type_op_controlseq of scope * string * numbracearg * term * term list
  (* -- 0-ary, i.e. only brace args *)
  | Prim_term_controlseq of scope * string * numbracearg * term * term list 
  | Prim_type_controlseq of scope * string * numbracearg * typ * expr list 

  (* -- binders *)
  | Prim_lambda_binder of scope * string * term 
  | Prim_pi_binder of scope * string * typ 
  | Prim_binder_prop of scope * string * prop 

  (* -- fields *)
  | Prim_field_term_accessor of scope * string * typ
  | Prim_field_type_accessor of scope * string 
  | Prim_field_prop_accessor of scope * string 

  (* -- non cs *)
  (* scope, pattern, def, frees *)                             
  | Prim_typed_name of scope * pattern * typ * expr list 
  | Prim_adjective of scope * pattern * prop * expr list
  | Prim_adjective_multisubject of scope * pattern * prop * expr list
  | Prim_simple_adjective of scope * pattern * prop * expr list
  | Prim_simple_adjective_multisubject of scope * pattern * prop * expr list
  | Prim_definite_noun of scope * pattern * term * expr list
  | Prim_identifier_term of scope * string * term * expr list
  | Prim_identifier_type of scope * string * typ * expr list
  | Prim_possessed_noun of scope * pattern * term * expr list
  | Prim_verb of scope * pattern * prop * expr list 
  | Prim_verb_multisubject of scope * pattern * prop * expr list
  | Prim_structure of scope * pattern * typ * expr list 
  | Prim_type_op of scope * string * typ * typ list
  | Prim_type_word of scope * pattern * typ * typ list
  | Prim_term_op of scope * pattern * term * expr list
  | Prim_binary_relation_op of scope * string * prop * (term * term)
  | Prim_propositional_op of scope * string * int * associativity * prop * prop list 
  | Prim_relation of scope * pattern * typ * term list 

  (* context variables *)
  | Prim_type_var of scope * string
  | Prim_term_var of scope * string * typ option
  | Prim_prop_var of scope * string 
[@@deriving show]




type this_adj =
  | This of string 
 (* 
  [Unique;Canonical;Welldefined;Wellpropped;Total;Exhaustive;Recursion;Exist]    
  *)
[@@deriving show]


type wordpattern = 
  | Wp_wd of string
  | Wp_option of string
  | Wp_synonym of string list 
  | Wp_var of context
  | Wp_symbol of string
  | Wp_ty_word of wordpattern list
  | Wp_bin_cs of wordpattern * string * wordpattern list * wordpattern
  | Wp_ty_identifier of string * (expr * expr) list * ((string * typ) list)
  | Wp_ty_cs of string * wordpattern list
  | Wp_cs of string * wordpattern list
  | Wp_symbolpat of wordpattern list * int option  * associativity
  | Wp_symbolpatP of wordpattern list * int option  * associativity
  | Wp_symbolpatT of wordpattern list
  | Wp_identifier of string * (expr * expr) list * ((string * typ) list)
  | Wp_identifierP of string * (expr * expr) list * ((string * typ) list)
  | Wp_fun_word of wordpattern list
  | Wp_notion of wordpattern list
  | Wp_adj of wordpattern list
  | Wp_adjM of wordpattern list
  | Wp_verb of wordpattern list
  | Wp_verbM of wordpattern list
  | Wp_classifier of string list list
  | Wp_namespace of string
  | Wp_inferring of wordpattern * wordpattern list
  | Wp_record of expr list * this_adj list
  | Wp_binder of wordpattern * wordpattern (* wp, wp_var *)
[@@deriving show]

type pos = (Lexing.position * Lexing.position) [@opaque]
[@@deriving show]

(* let show_pos _ _ = "" *)

(* strings here are labels *)

type raw = (* raw unprocessed data *)
  | RawNodeList of node list
[@@deriving show]

type text_item = 
  | Section_preamble of pos * bool * int * string (* new current section *)
  | Instruction of pos * instruct (* keyword *)
  | Axiom of pos * string * string * statement list * statement list  (* kind,label,assumptions,conclusions *)
  | Definition of pos * string * statement list * (wordpattern * expr) list * this_adj list (* label, assumptions, word patterns and definitions.  *)
  | Theorem of pos * string * statement list * statement list  (* label, assumptions ,conclusions *)
  | Synonym of pos * instruct
  | Fiat of wordpattern list
  | Implement of pos * typ * raw list 
  | Macro of pos * int * (wordpattern * expr) list
  | Namespace
[@@deriving show]

(* exceptions and positions *)

type trace = 
  | TrMany of trace list
  | TrAdd of trace list
  | TrOr of trace list
  | TrGroup of string * trace
  | TrFail of int * int * token 
  | TrData of token list
  | TrString of string 
  | TrEmpty
[@@deriving show]

type 'a parsed = 'a * (trace * node list)

