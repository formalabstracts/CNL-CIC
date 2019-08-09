 (*

Grammar for CNL-CIC.
Thomas C. Hales, 2019.

 *)

(* documentation *)

 (* 

  Beware of conflicting terminology between Forthel and Lean,
  including variables, attributes, sections, proof, pattern,
  names vs. namespaces. if,then vs. if-then-else.

  Forthel notions become Lean types. 

  The global outline of the file is given by
  start of line comments (regexp "^(").

  TDOP top down operator precedence terms and formulas are
  identified by the grammar but not reduced. They should be handled
  by a separate parser. 

  ioption = menhir inlined option. This is needed in a few
  places to remove reduce/reduce conflicts. 

  Fake symbols PA0, PA1,... PL1, PL2 will be removed from the
  final grammar.  The have been inserted to reduced the number
  of ambiguities in trying to write this as an LR(1) grammar.
  The implemented verion will use Parsec will arbitrary lookahead. 

  I have lost track of keeping parentheses to a necessary minimum.  
 *)

 (*

  Features not implemented: 

  Specific lines of the file are marked NOT_IMPLEMENTED.
  Unification hints. 

  *)

%{

 (*
  The grammar of expressions should fall into four sorts:
  props, proofs, types, and terms.  Here a term is an expression that
  is not any of the other three.  

  The parser of the langauge is not expected do to Lean-style type checking,
  but it should do type checking according to these four sorts. 

  In particular, parsing of primitives should read expressions of the right
  sort into the slots of the primitive. 
  *)


type exp_t =
| prop_t
| proof_t
| type_t
| term_t
[@@deriving show]

 %}



%start <string> program

%%

 (* These are not used, but they document the lexical structure. 
  delimiter : L_PAREN | R_PAREN | L_BRACK | R_BRACK | L_BRACE | R_BRACE {}
  separator : COMMA | SEMI | PERIOD {}
  punctuation : delimiter | separator {}
  lexeme : NUMERIC | identifier | FIELD_ACCESSOR | SYMBOL | punctuation {}
 *)


(* parametrized nonterminals *)

paren(X) : L_PAREN x = X R_PAREN { x }
bracket(X) : L_BRACK x = X R_BRACK { x }
brace(X) : L_BRACE x = X R_BRACE { x }
brace_semi(X) : brace(separated_nonempty_list(SEMI,X) {}) {}
opt_paren(X) : X | paren(X) {}

comma_nonempty_list(X) : separated_nonempty_list(COMMA,X) {}
comma_list(X) : separated_list(COMMA,X) {}
opt_comma_nonempty_list(X) : separated_nonempty_list(option(COMMA),X) {}
sep_list(X) : separated_list(sep_and_comma,X) {}

cs_brace(X) : X list(brace(expr) {}) {} (* control sequence args *)

 (* from phrase_lists.txt. These will need to be
    expanded in the working parser. *)
phrase_list_transition : PL1 {}
phrase_list_filler : PL2 {}
phrase_list_proof_statement : PL3 {}

(* literals *)

lit_a : LIT_A | LIT_AN {}
article : lit_a | LIT_THE {}
sep_and_comma : LIT_AND | COMMA {}
identifier : ATOMIC_IDENTIFIER | HIERARCHICAL_IDENTIFIER {}
lit_binder_comma : COMMA {}

lit_defined_as : LIT_SAID LIT_TO LIT_BE
| LIT_DEFINED LIT_AS
| LIT_DEFINED LIT_TO LIT_BE {}
lit_iff : LIT_IFF | LIT_IF LIT_AND LIT_ONLY LIT_IF {}
lit_denote : LIT_STAND LIT_FOR | LIT_DENOTE {}
lit_do : LIT_DO | LIT_DOES {}
lit_is : LIT_IS | LIT_ARE | option(LIT_TO) LIT_BE {}
lit_equal : LIT_EQUAL LIT_TO {}
lit_has : LIT_HAS | LIT_HAVE {}
lit_with : LIT_WITH | LIT_OF | LIT_HAVING {}
lit_true : LIT_ON | LIT_TRUE | LIT_YES {}
lit_false : LIT_OFF | LIT_FALSE | LIT_NO {}
lit_its_wrong : LIT_IT LIT_IS LIT_WRONG LIT_THAT {}
lit_we_record :  (* for instances / type class mechanism *)
| option(LIT_WE) LIT_RECORD option(LIT_THAT) 
| option(LIT_WE) LIT_REGISTER option(LIT_THAT) {}
lit_any : (* can extend: finitely many, almost all, etc. *)
| LIT_EVERY
| LIT_EACH
| LIT_EACH LIT_AND LIT_EVERY
| LIT_ALL
| LIT_ANY
| LIT_SOME
| LIT_NO {}
lit_exist : LIT_EXIST | LIT_EXISTS {}
lit_lets : LIT_LET option(LIT_US) | LIT_WE option(LIT_CAN) {}
lit_fix : LIT_FIX | LIT_LET {}
lit_assume : LIT_ASSUME | LIT_SUPPOSE {}
lit_then : LIT_THEN | LIT_THEREFORE | LIT_HENCE {}
lit_choose : LIT_TAKE | LIT_CHOOSE {}
lit_prove : LIT_PROVE | LIT_SHOW {}
lit_we_say : LIT_WE LIT_SAY option(LIT_THAT) {}
lit_left : LIT_LEFT | LIT_RIGHT | LIT_NO {}
lit_field_key : 
| LIT_OVER
| LIT_TYPE
| LIT_MAP {}
lit_qed : LIT_END | LIT_QED | LIT_OBVIOUS | LIT_TRIVIAL {}
lit_document :
| LIT_DOCUMENT
| LIT_ARTICLE
| LIT_SECTION
| LIT_SUBSECTION
| LIT_SUBSUBSECTION 
| LIT_SUBDIVISION {}
lit_enddocument :
| LIT_ENDSECTION
| LIT_ENDSUBSECTION
| LIT_ENDSUBSUBSECTION
| LIT_ENDSUBDIVISION
{}
lit_def : LIT_DEF | LIT_DEFINITION {}
lit_axiom : LIT_AXIOM | LIT_CONJECTURE | LIT_HYPOTHESIS | LIT_EQUATION | LIT_FORMULA {}
lit_theorem :
| LIT_PROPOSITION
| LIT_THEOREM
| LIT_LEMMA
| LIT_COROLLARY {}
lit_location :
| lit_document 
| lit_theorem 
| lit_axiom
| lit_def {}
lit_sort : LIT_TYPE | LIT_PROP {}
lit_classifier : LIT_CLASSIFIER | LIT_CLASSIFIERS {}

label : ATOMIC_IDENTIFIER {}

(* stub rules suppress errors for unused nonterminals. *)
stub_nonterminal :
| NOT_DEBUGGED
| make_term_opt_colon_type (* NOT_IMPLEMENTED instance declaration *)
{}

(* primitives *)

 (* Users must have a way to extend each primitive nonterminal
  with new production rules. A special statement or macro is
  a nonterminal that is used to create a new production rule. 

  Some primitives are derived primitives that are automatically
  generated from other primitives. 

  We give a comment for each primitive to indicate the 
  special or macro (or autogeneration) from which it gets extended.
  *)

 (* from classifier_def *)
prim_classifier : PA0 {} 
  (* meta-type words like "function" "element" "object" 
   "number" "quotient" "dependent function" "thing" "class" 
   "map" "structure" "term" "binary relation" "relation" 
   "operator" "binary operator" "pair" "pairs" "result" 
   *)

 (* from function_def.binary_controlseq_pattern, prec > 0 *)
prim_term_op_controlseq : PA1 {}

 (* from predicate_def.binary_controlseq_pattern, binary, prec=0 or none *)
prim_binary_relation_controlseq : PA1a {}

 (* from predicate_def.binary_controlseq_pattern, prec < 0 *)
prim_propositional_op_controlseq : PA1b {}

 (* from type_head.binary_controlseq_pattern, binary prec < 0 *)
prim_type_op_controlseq : PA1c {}

 (* from function_def.controlseq_pattern, no prec *)
prim_term_controlseq : PA1d {}

 (* from type_head.controlseq_pattern, no prec *)
prim_type_controlseq : PA2 {}

 (* from NOT_IMPLEMENTED *)
prim_lambda_binder : PA3 {} (* term binders *)
prim_pi_binder : PA4 {} (* type binders *)
prim_binder_prop : PA5 {}


 (* from declarations of structures, quotients, 
    inductive types, mutual inductive types  *) 
prim_typed_name : PA6 {} 

 (* from NOT_IMPLEMENTED. Forthel: primClassRelation
    Forthel uses this for quantifier scoping. 
    This might not be needed in the end. 
    prim_free_predicate : PA7 {} 
  *)

 (* from adjective_pattern *)
prim_adjective : PA8 {} 

 (* from adjective_multisubject_pattern *)
prim_adjective_multisubject : PA9 {} 

 (* derived from prim_adjective as in Forthel. *)
prim_simple_adjective : PA10 {} 

 (* derived from prim_adjective_multisubject as in Forthel *)
prim_simple_adjective_multisubject : PA11 {} 

 (* from function_def *)
prim_definite_noun : PA12 {} (* functions and terms *)

 (* from function_def *)
prim_identifier_term : PA13 {} (* all identifiers that are terms *)

 (* from type_def *)
prim_identifier_type : PA7 {} (* all identifiers that are terms *)

 (* derived as in Forthel *)
prim_possessed_noun : PA15 {} 

 (* from verb_pattern *)
prim_verb : PA16 {}

 (* from verb_multiset_pattern *)
prim_verb_multisubject : PA17 {} 

 (* from type_def *)
 (* NOT_IMPLEMENTED. 
    This is a stub for Cabarete mode structure notation, such as a
    vector_space over R.  Note that LIT_OVER is used to debundle parameters. 
    The syntax of prim_structures should be something like this:
    prim_structure_literal names option(LIT_OVER debundled_args) {}
 *)

prim_structure : PA18 {} 

 (* from type_def, when infix with precedence *)
prim_type_op : PA18a {} (* A + B, A * B on types, etc.  *)

 (* from function_head.symbol_pattern *)
prim_term_op : PA19 {} (* + - * / etc. *)

 (* from predicate_def.symbol_pattern, binary infix with prec=0 or none  *)
prim_binary_relation_op : PA20 {} (* = < > etc. *)

 (* from predicate_def.symbol_pattern, with prec < 0 *)
prim_propositional_op : PA21 {} (* logical connectives *)

 (* from predicate_def.identifier_pattern *)
prim_relation : PA22 {} (* prop-valued *)

(* sections test:Sections *)

section_preamble : section_tag option(label) PERIOD {}
  section_tag : 
  | lit_document
  | lit_enddocument {}

(* namespaces  *)

namespace : NOT_IMPLEMENTED {}

(* instructions - 
   See Naproche-SAD github Instr.hs. Test:Instructions  
 *)

instruction :
| instruct_command
| instruct_synonym
| instruct_string
| instruct_bool
| instruct_int {}

  instruct_keyword_command : LIT_EXIT {}
  instruct_keyword_int : LIT_TIMELIMIT {}
  instruct_keyword_bool : LIT_PRINTGOAL | LIT_DUMP | LIT_ONTORED {}

  instruct_keyword_string : LIT_READ | LIT_LIBRARY {}

  instruct_command : bracket( instruct_keyword_command) {}
  instruct_int : bracket(instruct_keyword_int NUMBER {}) {}
  bool_tf : lit_true | lit_false {}
  instruct_bool : bracket(instruct_keyword_bool bool_tf {}){}
  instruct_string : bracket(instruct_keyword_string STRING {}) {}
  instruct_sep : SLASH | SLASHDASH {}
  instruct_synonym : bracket(LIT_SYNONYM
    separated_nonempty_list (instruct_sep,nonempty_list(TOKEN)) {}) {}

(* variables *)

 (* Variables can be given the modifier LIT_INFERRED
 This is a blanket term for Lean style parameters, [type class inference],
 implicit arguments, etc.  

 When a variable is inferred, it can appear on the right-hand side of
 macros and definitions without appearing explicitly on the left-hand side. 

 For example, we can write
 We say that X is finite iff ... where (inferred alpha : Type) (X : set over alpha).
 Then the alpha is not required to appear to the left of the iff.

 For example, 
 Let x in X stand for C.notation_in x X where (inferred C : has_in).

 If a section introduces an inferred variable into the context, then 
 the "CNL elaborator" inserts the corresponding where clause into every
 macro and definition that contains that variable on the right-hand side
 but not the left. 

  *)

var_modifier : option(LIT_INFERRED) {}
annotated_var : paren(var_modifier VAR opt_colon_type {}) {}
annotated_vars : paren(var_modifier nonempty_list(VAR) opt_colon_type {}) {}

tvar : VAR | annotated_var {}

record_assign_term :
  brace_semi(var_or_atomic opt_colon_type ASSIGN expr {})  {}

app_args :  (* can be empty *)
  option( record_assign_term {}) list(tightest_expr) {}


(* function and binder parameters. *)

args : option( opt_args {}) required_args {}

         (* A brace_semi as the first argument to a function is
            ambiguious: it could be either a opt_args or a required
            arg that takes the form of a make_term.  The convention is
            that the principal interpretation is opt_args.  But if it
            contains a field name that is not an optional argument,
            then it is interpreted as a make_term. *)

  opt_args : 
    brace_semi(var_or_atomics opt_colon_type {}) {}

  required_args : list(required_arg) {}

  (* convention - all types are the same within parentheses *)
    required_arg :
    | paren(var_or_atomics opt_colon_type {})
    | var_or_atomic {}


  var_or_atomic : VAR | ATOMIC_IDENTIFIER {}
  var_or_atomics : nonempty_list(var_or_atomic) {}   

 (*
  Consider 

  lambda t, f

  All variables x of t are extracted
  as well as the field names of structures mentioned in t.

  Then the translation becomes roughly (lambda x, let t := x in f), with
  pattern matching on t.  The pattern t should be exhaustive as
  in a structure or tuple. 

  For example,
  lambda ({ carrier ; op } : group), f 

  *)

 (* unambiguous boundaries of terms needed here *)
 (* This allows too much. We should restrict further to admissible patterns. *)
generalized_args : opt_args list(generalized_arg) {}

  generalized_arg : 
  | tightest_term
  | paren(var_or_atomic var_or_atomics opt_colon_type {})
  {}  

 (*
  LIT_HOLDING Forthel extracts the variables from certain expressions, then 
  creates a binder over those variables. 
  For example, 
    'forall x + y > 3, we have x + y > 0' translates to
    'forall x y, x + y > 3 -> x + y > 0'. 
  Sometimes, some variables should be held from binder. In
    'forall 1 < i < j < n, we have ...' 
  we might want to hold the variable n from binding so that translation gives
    'forall i j, 1 < i < j < n -> ...' 
  We need a purely syntactic rule to hold n.
    'forall 1 < i < j < n (holding n), we have ...'

  Forthel addresses this issue by putting strong restrictions on the form
  of quantifiedNotion.  It quantifiers only over variables in the "name" position
  of the quantifiedNotion. We wish to relax these restrictions. 

  In HOL-LIGHT, the extended set comprehension syntax does something related. 
  There are two vertical bars:    { s | t | P }.
  The term t in the middle is used to determine
  which of the free variables of s are to be bound by the comprehension. 
 *)

holding_var : option(paren(LIT_HOLDING comma_nonempty_list(VAR) {})) {}

(* expressions *)

expr : general_type | term | prop | proof_expr | sort_expr {}

tightest_expr : (* what is allowed as an arg to function calls *)
| tightest_term
| tightest_prop
| tightest_type
| proof_expr
{}

(* sorts *)

sort_expr : (* Side condition: args should be nonempty *)
| option(args ARROW {}) lit_sort 
{}

colon_sort : COLON sort_expr {}

opt_colon_sort : option(colon_sort) {}

(* types *)

tightest_type :
| paren_type
| annotated_type
| controlseq_type
| const_type
| var_type
| subtype
| inductive_type
| mutual_inductive_type
| structure
| prim_structure 
{}

paren_type : paren(general_type) {}

annotated_type : paren(general_type COLON LIT_TYPE {}) {}

controlseq_type : cs_brace(prim_type_controlseq) {}

const_type : prim_identifier_type {}

(* if not annotated here, the VAR should be previously annotated in the context. *)
var_type : 
| VAR 
| paren(VAR COLON LIT_TYPE {}) {}

subtype :  brace(term holding_var LIT_SUBTYPEMID statement {}) {}

app_type : tightest_type app_args {}

binder_type : 
| app_type 
| prim_pi_binder generalized_args lit_binder_comma binder_type 
{}

(** binop_type *)
 (* for product types A * B, sum types A + B, 
    including arrows A -> B,
    including Agda style dependent arrows (x:A) -> B x.
    all type operators are right assoc with the same precedence
    N.B. binder_types is tighter than binop_type, which might be non-intuitive.  *)
binop_type : 
| list(type_operand type_op {}) binder_type  {}

  type_op :
  | prim_type_op 
  | cs_brace(prim_type_op_controlseq)
  {}

  type_operand :
  | binder_type
  | dependent_vars {} (* for Agda style dependent typing *)

  dependent_vars : option( opt_args {})
    nonempty_list(annotated_vars) {}

opentail_type :
| binop_type
| quotient_type
| coercion_type
| overstructure_type
{}

 (* agda_pi_type : nonempty_list(annotated_vars) ARROW opentail_type {} *)

quotient_type : LIT_QUOTIENT option(LIT_OF) general_type LIT_BY term {}

coercion_type : 
| COERCION term (* explicit coercion *)
{}

overstructure_type :
| prim_structure LIT_OVER over_args {}

  over_args :
  | brace_semi(var_or_atomic ASSIGN term {})
  | comma_nonempty_list(tightest_expr) {}


general_type : opentail_type {}




colon_type : COLON general_type {}

opt_colon_type : option(colon_type) {}



(** inductive types *)

inductive_type : LIT_INDUCTIVE identifier args 
  opt_colon_sort list(opt_alt_constructor) LIT_END {}

  opt_alt_constructor : ALT identifier args opt_colon_type {}
  alt_constructor : ALT identifier args colon_type {}

mutual_inductive_type : LIT_INDUCTIVE
  comma_nonempty_list(identifier) args 
  list(LIT_WITH ATOMIC_IDENTIFIER args colon_type
       list(alt_constructor) {}) 
  LIT_END{}

(** structure *)

 (* we require at least one field.  Later, if needed
    we can create prop_structure, without fields. 
  *)

structure : option(LIT_NOTATIONAL) LIT_STRUCTURE 
  option(lit_param) args
  option(LIT_WITH) brace_semi(field)
  option(LIT_SATISFYING satisfying_preds {}) {}

  lit_param : LIT_WITH LIT_PARAMETERS {}

  (* If the field identifier is a var, satisfaction ignores its name.
   If the field identifier is a prim_structure, it becomes embedded. 
   *)

  field : field_prefix field_identifier option(field_suffix) {}
  field_identifier : 
  | prim_structure
  | var_or_atomic opt_colon_type {}

  field_prefix : ALT list(lit_field_key) {}
  field_suffix : LIT_WITHOUT LIT_NOTATION | field_assign {}
  field_assign : ASSIGN expr {}

  satisfying_preds : brace_semi(satisfying_pred) {}
  satisfying_pred : ALT option(ATOMIC_IDENTIFIER COLON {}) prop {}

(* proof expressions (distinct from proof scripts). *)

proof_expr : 
| SYMBOL_QED 
| paren(proof_expr)
{}

(* tightest terms *)

tightest_term : 
| tightest_prefix
| tightest_term FIELD_ACCESSOR
| tightest_term APPLYSUB tightest_terms
{}

  tightest_terms : paren(nonempty_list(tightest_term)) {}

  tightest_prefix :
  | NUMERIC
  | STRING
  | DECIMAL
  | BLANK 
  | VAR
  | prim_identifier_term
  | controlseq_term
  | delimited_term
  | alt_term
  {}


controlseq_term : cs_brace(prim_term_controlseq) {}

(** delimited term *)

delimited_term :
| paren(term)
| annotated_term
| make_term
| list_term
| tuple_term
| set_enum_term
| set_comprehension_term
{}

annotated_term : paren(term colon_type {}) {}

make_term : brace_semi(var_or_atomic_or_blank
  option(ASSIGN term {}) {}) {}

  var_or_atomic_or_blank : 
  | var_or_atomic
  | BLANK {}

list_term : bracket(separated_list(SEMI,term) {}) {}

tuple_term : paren(term COMMA comma_nonempty_list(term) {}) {}

set_enum_term : brace(comma_list(term) {}) {}

set_comprehension_term : brace(term holding_var LIT_MID statement {}) {} 

(** alt_term *)

alt_term : (* These bind tightly because of terminating END *)
| case_term
| match_term
| lambda_function
{}

(*
 Case statements generate an obligation for exhaustive cases. 
 Matches must also be exhaustive. 
 *)
case_term : LIT_CASE term LIT_OF 
  nonempty_list(alt_case) LIT_END {}
  alt_case : ALT prop ASSIGN term {}

match_term : LIT_MATCH match_seq LIT_WITH 
  nonempty_list(ALT match_pats ASSIGN term {}) LIT_END {}

  match_seq : comma_nonempty_list(term) {}
  match_pats : comma_nonempty_list(match_pat) {}
  match_pat : term {} (* Variables that do not appear in match_seq are assumed fresh. *)

lambda_function : LIT_FUNCTION identifier args 
  opt_colon_type nonempty_list(ALT match_pats ASSIGN term {})
  LIT_END {}

(* term *)

app_term : tightest_term app_args {}

opentail_term : 
| lambda_term
| lambda_fun
| let_term
| if_then_else_term
| tdop_term
{}

lambda_term : 
| prim_lambda_binder generalized_args lit_binder_comma opentail_term
| generalized_arg MAPSTO opentail_term 
{}

lambda_fun : LIT_FUN identifier generalized_args 
  opt_colon_type ASSIGN opentail_term {}

  (* let includes destructuring*)
let_term : LIT_LET term ASSIGN term LIT_IN opentail_term {}

if_then_else_term : LIT_IF prop LIT_THEN term LIT_ELSE opentail_term {}

symbolic_term : opentail_term option(where_suffix) {}

definite_term : 
| symbolic_term
| option(LIT_THE) prim_definite_noun
| paren(option(LIT_THE) prim_definite_noun {}) {}

 (* where (Haskell style) *)
  where_suffix : LIT_WHERE brace_semi(tvar opt_colon_type ASSIGN term {}) {}

term : 
| definite_term 
| any_name {}  

terms : sep_list(term) {}

(** plain term *)
 (*
  Following Forthel 1.3.3,
  a plain_term contains no quantifier of the form lit_any inside.  
  We implement this with a separate check that the term is plain,
  rather than build plain terms as a separate nonterminal. 
  We write plain(term) as a term that passes the plainness check. 
  We require plain terms on the right-hand-side of definitions.
  Also, in dependent types, the terms should be plain.
  *)

 (* NOT_IMPLEMENTED 
    When implemented, this should give a check 
    that the term is plain *) 
plain_term : plain(term) {}
  plain(X) : X {} 

(* loose ends *)


(** make. *)
make_term_opt_colon_type : make_term opt_colon_type {}

(* TDOP symbolic terms and formulas *)
 (* top down operator precedence formulas. 

    There are three general precedence categories built into 
    the grammar.  
    * prop operators; (precedence < 0) 
    * binary relation operators such as "="; (precedence=0)
    * term operators.  (precedence > 0).
    This allows us to distinguish terms from props and types.
  *)

 (* prec > 0 *)
tdop_term : 
| ioption(app_term) term_ops
    list(app_term term_ops {}) option(app_term)
| app_term
{}

  term_ops : nonempty_list(term_op) {}

  term_op : 
  | prim_term_op
  | cs_brace(prim_term_op_controlseq) {}

 (* prec = 0 *)
 (* We allow x,y < z < w. The first arg can be a list. *)
tdop_rel_prop : 
  tdop_terms
  nonempty_list(binary_relation_op tdop_term {}) {}

  binary_relation_op :
  | prim_binary_relation_op
  | cs_brace(prim_binary_relation_controlseq)
  {}

  tdop_terms : comma_nonempty_list(tdop_term) {}

 (* prec < 0 *)
tdop_prop :
  option(binder_prop) prop_ops
  list(binder_prop prop_ops {}) option(binder_prop) {}

  prop_ops : nonempty_list(prop_op) {}

  prop_op :
  | prim_propositional_op
  | cs_brace(prim_propositional_op_controlseq)
  {}

(* props *)

tightest_prop :
| paren(statement)
| identifier_prop 
| VAR
| annotated_prop
{}

identifier_prop : identifier {}

annotated_prop : paren(prop COLON LIT_PROP {}) {}

app_prop : tightest_prop app_args {} 

binder_prop :
| app_prop
| tdop_rel_prop 
| prim_binder_prop args lit_binder_comma binder_prop {}

prop : 
| binder_prop
| tdop_prop
{}


(* statements *)

(** predicates *)
does_pred : option(lit_do) option(LIT_NOT) prim_verb {}
| option(lit_do) option(LIT_NOT) prim_verb_multisubject
| lit_has has_pred
| lit_is sep_list(is_pred)
| lit_is sep_list(is_aPred {}) {}

is_pred : option(LIT_NOT) prim_adjective {}
| option(LIT_NOT) option(LIT_PAIRWISE) prim_adjective_multisubject
| lit_with has_pred {}

is_aPred : option(LIT_NOT) option(lit_a) general_type {}
| option(LIT_NOT) definite_term {}

has_pred : 
| sep_list(article possessed_noun {}) {}
| LIT_NO possessed_noun {}

  possessed_noun : attribute(prim_possessed_noun) {}


(** attributes (forthel style) *)

attribute(X) : list(left_attribute) X option(right_attribute) {}

  left_attribute : 
  | prim_simple_adjective 
  | prim_simple_adjective_multisubject {}

  right_attribute : 
  | sep_list(is_pred) {}
  | LIT_THAT sep_list(does_pred) {}
  | LIT_SUCH LIT_THAT statement {}

typed_name : attribute(typed_name_without_attribute) {}
  typed_name_without_attribute : 
  | prim_typed_name
  | tvar
  | prim_classifier tvar 
  | VAR lit_with LIT_TYPE general_type
  | paren(typed_name_without_attribute)
  {}

named_terms : sep_list(option(lit_a) named_term {}) {}

named_term : typed_name | free_predicate {} (* general_type *)

 (* 
  The relevant variables for binding in the first case are all unheld vars. 
  In the case of a binary relation only vars2 enter the binder. 
  *)
free_predicate : attribute(free_predicate_without_attribute) {} 
  free_predicate_without_attribute : 
  (* N.B. This might allow too much. It was prim_free_predicate, rather than prop. *)
  | opt_paren(prop holding_var {}) 
  | vars2 binary_relation_op tdop_term {}
  vars2 : tvar comma_nonempty_list(tvar) {}

(** statement *)

statement : head_statement | chain_statement {}

  head_statement : 
  | LIT_FOR sep_list(any_name {}) option(COMMA) statement {}
  | LIT_IF statement COMMA LIT_THEN statement (* != if-then-else *)
  | lit_its_wrong statement {}

  chain_statement : 
  | and_or_chain
  | paren(and_or_chain) LIT_IFF statement 
  {}

  and_or_chain : and_chain | or_chain | primary_statement {}
  and_chain : separated_nonempty_list(LIT_AND, primary_statement {}) 
    LIT_AND head_primary {}
  or_chain : separated_nonempty_list(LIT_OR, primary_statement {}) 
    LIT_OR head_primary {}
  head_primary : head_statement | primary_statement {}

  any_name : 
  | lit_any comma_nonempty_list(any_arg) 
  | lit_any typed_name
  | lit_any free_predicate
  | lit_any general_type
  {}
  any_arg :
  | VAR
  | annotated_vars {}

(** primary statement *)
primary_statement :
  | simple_statement {}
  | there_is_statement
  | filler symbol_statement
  | filler const_statement {}

  filler : PL2a option(phrase_list_filler) {}

  simple_statement : terms separated_nonempty_list(LIT_AND, does_pred) {}

  there_is_statement : LIT_THERE lit_exist named_terms {}
  | LIT_THERE lit_exist LIT_NO named_term {}

  const_statement : option(LIT_THE) LIT_THESIS {}
  | option(LIT_THE) LIT_CONTRARY
  | lit_a LIT_CONTRADICTION {}

  symbol_statement :
  | LIT_FORALL free_predicate lit_binder_comma
    symbol_statement {}
  | LIT_EXISTS free_predicate lit_binder_comma symbol_statement
  | prim_relation
  | LIT_NOT symbol_statement
  | paren(statement)
  | prop
  {}



(* text *)
text : list(text_item) {}
  text_item : 
    | namespace
    | macro
    | section_preamble
    | declaration
    | instruction {}

(* declaration test:declaration *)
declaration : axiom | definition | theorem  {}

(** axiom *)
axiom : axiom_preamble list(assumption) 
  then_prefix statement PERIOD {}

  axiom_preamble : lit_axiom option(label) PERIOD {}

  assumption :
  | assumption_prefix statement PERIOD
  | let_annotation PERIOD {}
  assumption_prefix : 
  | LIT_LET 
  | lit_lets lit_assume option(LIT_THAT) {}

  then_prefix : option(lit_then) {}


(** theorem *)
theorem : theorem_preamble list(assumption) affirm_proof {}
  theorem_preamble : lit_theorem option(label) PERIOD {}

(** affirm *)
affirm_proof : 
| statement_proof
| goal_proof 
{}
 
  statement_proof : then_prefix statement by_ref PERIOD
    option(proof_script {}) {}

  goal_proof : goal_prefix statement by_ref PERIOD 
    proof_script {}

    goal_prefix : option(lit_lets)
      lit_prove by_method option(LIT_THAT) {}


(** proof scripts *)
proof_script : proof_preamble option(list(canned_prefix proof_body {}) 
  canned_prefix proof_tail {}) lit_qed PERIOD {}

  proof_preamble : LIT_PROOF by_method PERIOD
   | LIT_INDEED {}

  proof_body : proof_tail | assumption {}

  proof_tail : affirm_proof | canned_proof | case | choose  {}

  canned_prefix : sep_list(phrase_list_transition) option(COMMA) {}

  canned_proof : phrase_list_proof_statement {}

  case : LIT_CASE statement PERIOD proof_script {}

  choose : choose_prefix named_terms by_ref PERIOD choose_justify {}
  choose_justify : 
  | option(proof_script {}) {}
  choose_prefix : then_prefix option(lit_lets) lit_choose {}

  by_method : option(LIT_BY proof_method {}) {}
  proof_method : LIT_CONTRADICTION
    | LIT_CASE LIT_ANALYSIS
    | LIT_INDUCTION option(LIT_ON plain_term {}) {}
  by_ref : option(paren(LIT_BY ref_item {})) {}
  ref_item : sep_list(option(lit_location) label {}) {}


(** This exists and is well-defined. *)
this_exists : LIT_THIS
  sep_list(this_directive_pred) PERIOD {}
  this_directive_pred : LIT_IS
    sep_list(this_directive_adjective)
    | this_directive_verb {}
  this_directive_adjective :
    | LIT_UNIQUE
    | LIT_CANONICAL
    | LIT_WELLDEFINED
    | LIT_WELL_DEFINED
    | LIT_WELL LIT_DEFINED
    | LIT_TOTAL
    | LIT_WELL LIT_PROPPED
    | LIT_WELL_PROPPED
    | LIT_EXHAUSTIVE {}
  this_directive_right_attr : LIT_BY LIT_RECURSION {}
  this_directive_verb : LIT_EXISTS option(this_directive_right_attr){}

(** definition *)
definition : definition_preamble list(assumption) 
  definition_affirm {}

  definition_preamble : lit_def option(label) PERIOD {}
  definition_affirm : definition_statement PERIOD 
    option(this_exists) {}

definition_statement :
| classifier_def 
| type_def
| function_def
| predicate_def
{}

  copula : lit_is option(lit_defined_as) | ASSIGN | lit_denote {}
  iff_junction : lit_iff {}
  opt_say : option(lit_we_say) {}
  opt_record : option(lit_we_record) {}
  opt_define : 
  | option(lit_lets) option(LIT_DEFINE)
  | opt_record {}

  classifier_def : LIT_LET class_tokens lit_is lit_a lit_classifier {}
    class_tokens : comma_nonempty_list(TOKEN) {}

  type_def : opt_define type_head copula lit_a general_type {}

    type_head :  
    | type_token_pattern 
    | identifier_pattern 
    | controlseq_pattern 
    | binary_controlseq_pattern (* fixed precedence level, right assoc *)
    {} 

  function_def : opt_define function_head
    copula option(lit_equal) option(LIT_THE) plain_term {}

    function_head :
    | function_token_pattern
    | symbol_pattern option(paren_precedence_level)
    | identifier_pattern  {}

  predicate_def : opt_say predicate_head iff_junction statement {}
    predicate_head :
    | predicate_token_pattern
    | symbol_pattern option(paren_precedence_level)
    | identifier_pattern {}

(*
  structure_def : option(lit_a) identifier_pattern LIT_IS 
    lit_a structure {}

  inductive_def : opt_define inductive_type {}
  
  mutual_inductive_def : opt_define mutual_inductive_type {}
 *)

 (*

  A binary control sequence pattern becomes an infix operator.
  Note that the control sequences in the pattern may
  themselves contain arguments in {}.  This allows interesting nested
  patterns such as infix operators with superscripts and subscripts. 

  According to Paskevich 1.3.6. existing primitives are allowed for
  the purpose of signature extensions. We do not include them. 

  The restrictions of Paskevich 1.3.6 hold for functions.
  * In the head unit, the argument places hold variables.
  * No repeated variables occur in the head. 
  * Every free variable on the right-hand appears on the left. 
  * etc.

  *)

  (*
  Disambiguation:
  predicates use iff_junction.
  predicates token patterns start with tvar.
  identifier patterns start with an identifier.
  symbol patterns contain a symbol.

  Functions use the copula.
  token patterns starts with LIT_THE token ...
  symbol patterns contain a symbol or CONTROLSEQ.
  identifier patterns start with an identifier.

  Types use the copula.
  type_def vs. function_def distinguished by whether the RHS of the copula is
  a type or term. 
  *)

(* macro *)

macro : option(insection) macro_bodies {}

  insection : LIT_IN LIT_THIS section_tag {}

  macro_bodies : macro_body list(SEMI option(LIT_AND) macro_body {}) PERIOD {}

  macro_where : LIT_WHERE nonempty_list(annotated_vars) {}                   

  macro_body : 
  | classifier_def 
  | type_def 
  | function_def option(macro_where) 
  | predicate_def option(macro_where) 
  | let_annotation 
  | we_record_def
  {}

  (* if LIT_FIX is used in let_annotation, it plays the role of a Lean
     parameter, so that it inserted as an implicit parameter in every
     macro where the variable appears on the right-hand side but not
     the left.

     If LIT_LET is used, the type information is associated with the
     variable wherever it is used, but there is no automatic insertion. 

  *)

  let_annotation : lit_fix comma_nonempty_list(annotated_vars) {}

  we_record_def : lit_we_record comma_nonempty_list(plain_term) {}

 (* subsumed by type_def, function_def, etc. 

  type_macro : LIT_LET type_token_pattern
    lit_denote lit_a general_type {}

  function_macro : LIT_LET function_head 
    lit_denote plain_term {}

  predicate_macro :
  | LIT_LET predicate_token_pattern lit_denote statement {}
  | LIT_LET symbol_pattern lit_denote statement {}
 *)

(* pattern *)

 (* restriction: tokens in pattern cannot be a variant of
    "to be", "called", "iff" "a" "stand" "denote"
    cannot start with "the"  *)

token_pattern : tokens list(tvar tokens {}) option(tvar) {}

  tokens : nonempty_list(TOKEN) {}

type_token_pattern : option(lit_a) token_pattern {}

function_token_pattern : LIT_THE token_pattern {}

predicate_token_pattern :
| adjective_pattern 
| adjective_multisubject_pattern
| verb_pattern
| verb_multisubject_pattern 
{}

  adjective_pattern : tvar LIT_IS option(LIT_CALLED) token_pattern {}

  adjective_multisubject_pattern : 
    var_multisubject LIT_ARE option(LIT_CALLED) token_pattern {} 

  verb_pattern : tvar token_pattern {} 

  verb_multisubject_pattern : var_multisubject token_pattern {}

  var_multisubject :
  | tvar COMMA tvar
  | paren(VAR COMMA VAR colon_type {}) {}


identifier_pattern :
| identifier args opt_colon_type {}
| BLANK args opt_colon_type {} (* instance can be anonymous *)

controlseq_pattern : CONTROLSEQ list(brace(tvar)) {} (* subsumed by symbol_pattern *)

binary_controlseq_pattern : tvar controlseq_pattern tvar {} (* subsumed by symbol_pattern *)

symbol_pattern : option(tvar) symbol list(tvar symbol {}) 
  option(tvar) {}

  symbol : SYMBOL | CONTROLSEQ list(brace(tvar)) {} 

paren_precedence_level :
| precedence_level
| paren(precedence_level) {}

  precedence_level :
  | LIT_WITH LIT_PRECEDENCE INTEGER 
    option(LIT_AND lit_left LIT_ASSOCIATIVITY {}) {}

(* program *)

program_text : text | stub_nonterminal { "done" }

program : program_text EOF {}


