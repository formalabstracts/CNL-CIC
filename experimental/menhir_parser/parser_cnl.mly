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
  The implemented verion will use Parsec with arbitrary lookahead. 

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
  props, proofs, types, and terms (and quasiterms).  
  Here a term is an expression that
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

(* Lexing 

  The lexical structure comments are in  lexer_cnl.ml

 *)


(* parametrized nonterminals *)

paren(X) : L_PAREN X R_PAREN { }
bracket(X) : L_BRACK X R_BRACK { }
brace(X) : L_BRACE X R_BRACE { }
brace_semi(X) : brace(separated_nonempty_list(SEMI,X) {}) {}
opt_paren(X) : X | paren(X) {}

comma_nonempty_list(X) : separated_nonempty_list(COMMA,X) {}
comma_list(X) : separated_list(COMMA,X) {}
(* opt_comma_nonempty_list(X) : separated_nonempty_list(option(COMMA),X) {} *)
and_comma_nonempty_list(X) : separated_list(and_comma,X) {}

cs_brace(X) : X list(brace(expr) {}) {} (* control sequence args *)

 (* from phrase_lists.txt. These will need to be
    expanded in the working parser. *)
phrase_list_transition : PL1 {}
phrase_list_filler : PL2 {}
phrase_list_proof_statement : PL3 {}

 (* NOT_IMPLEMENTED.  A case_sensitive_word is a 
  WORD token, but parsed in a case sensitive way, possibly with synonyms *)
case_sensitive_word : PL4 {}
atomic : case_sensitive_word | ATOMIC_IDENTIFIER {}
identifier : atomic | HIERARCHICAL_IDENTIFIER {}


(* literals *)

  (* Each literal LIT_* is a case insensitive WORD token *)

lit_a : LIT_A | LIT_AN {}
article : lit_a | LIT_THE {}
and_comma : LIT_AND | COMMA {}
lit_binder_comma : COMMA {}


lit_defined_as : LIT_SAID LIT_TO LIT_BE
| LIT_DEFINED LIT_AS
| LIT_DEFINED LIT_TO LIT_BE {}
lit_is : LIT_IS | LIT_ARE | option(LIT_TO) LIT_BE {}
lit_iff : 
| LIT_IFF 
| LIT_IF LIT_AND LIT_ONLY LIT_IF
| lit_is option(LIT_THE) LIT_PREDICATE {}
lit_denote : LIT_STAND LIT_FOR | LIT_DENOTE {}
lit_do : LIT_DO | LIT_DOES {}
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
| LIT_SOME LIT_AND LIT_EVERY
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
lit_say : LIT_SAY | LIT_WRITE {}
lit_we_say : option(LIT_WE) lit_say option(LIT_THAT) {}
lit_left : LIT_LEFT | LIT_RIGHT | LIT_NO {}
lit_type : LIT_TYPE | LIT_TYPES {}
lit_proposition : LIT_PROPOSITION | LIT_PROPOSITIONS {}
lit_field_key : 
| LIT_COERCION
| LIT_NOTATIONLESS
| LIT_NOTATION
| LIT_PARAMETER
| LIT_TYPE
| LIT_MAP {}
lit_qed : LIT_END | LIT_QED | LIT_OBVIOUS | LIT_TRIVIAL {}
lit_document :
| LIT_DOCUMENT
| LIT_ARTICLE
| LIT_SECTION
| LIT_SUBSECTION
| LIT_SUBSUBSECTION 
| LIT_DIVISION 
| LIT_SUBDIVISION {}
lit_enddocument :
| LIT_ENDSECTION
| LIT_ENDSUBSECTION
| LIT_ENDSUBSUBSECTION
| LIT_ENDDIVISION
| LIT_ENDSUBDIVISION
{}
lit_def : LIT_DEF | LIT_DEFINITION {}
lit_axiom : LIT_AXIOM | LIT_CONJECTURE | LIT_HYPOTHESIS | LIT_EQUATION | LIT_FORMULA {}
lit_property : LIT_PROPERTY | LIT_PROPERTIES {}                                                                                                 
lit_with_properties : LIT_WITH lit_property {}                                                                                                 

lit_declare_mutual : 
option(LIT_WE) LIT_DECLARE LIT_MUTUAL LIT_INDUCTIVE {}

lit_declare_mutual_inductive_type :
lit_declare_mutual lit_type {}

lit_declare_mutual_inductive_def :
lit_declare_mutual lit_def {}


                                

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

lit_classifier : LIT_CLASSIFIER | LIT_CLASSIFIERS {}

 (*
   Unlike earlier LIT_ and lit_ words, identifiers are case sensitive. 
  *)                                                            

 lit_sort : ID_TYPE | ID_PROP {} (* 'Type' and 'Prop' *)
 label : atomic {}

(* stub rules suppress errors for unused nonterminals. *)
stub_nonterminal :
| NOT_DEBUGGED
(* | make_term_opt_colon_type *) (* NOT_IMPLEMENTED instance declaration *)
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

 (* from function_def.binary_symbol_pattern, prec > 0 *)
prim_term_op_controlseq : PA1 {}

 (* from predicate_def.binary_symbol_pattern, binary, prec=0 or none *)
prim_binary_relation_controlseq : PA1a {}

 (* from predicate_def.binary_symbol_pattern, prec < 0 *)
prim_propositional_op_controlseq : PA1b {}

 (* from type_head.binary_symbol_pattern, binary prec < 0 *)
prim_type_op_controlseq : PA1c {}

 (* from function_def.controlseq_pattern, no prec *)
prim_term_controlseq : PA1d {}

 (* from type_head.controlseq_pattern, no prec *)
prim_type_controlseq : PA2 {}

 (* from binder_def *)
prim_lambda_binder : LAMBDA {} (* lambda term binders *)
prim_pi_binder : PITY {} (* Pi type binders *)
prim_binder_prop : QUANTIFIER {}  (* forall, exists, etc. *)


 (* derived from declarations of prim_identifier_type,  that is (type_def.identifier_type_pattern).
    structures, quotients, 
    inductive types, mutual inductive types  

    Roughly, this should be any wordlike type that can carry a
    "names appositive" and attributes:
    integer x; real numbers y,z; group G; ring R; modules M,N over R.
    The recorded patterns should have names in the first position. 
  *) 
prim_typed_name : PA6 {} 

 (* from NOT_IMPLEMENTED. Forthel: primClassRelation
    Forthel uses this for quantifier scoping. 
    This might not be needed in the end. 
    prim_predicate_pseudoterm : PA7 {} 
  *)

 (* from structure *)
prim_field_term_accessor : PA6a {}

prim_field_type_accessor : PA6b {}

prim_field_prop_accessor : PA6c {}


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

 (* from function_def identifier_term_pattern  *)
prim_identifier_term : PA13 {} (* all identifiers that are terms *)

 (* from type_def *)
prim_identifier_type : PA7 {} (* all identifiers that are types *)

 (* derived from prim_definite_noun or XX as in Forthel *)
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

 (* from type_def symbol pattern  *)
prim_type_op : PA18a {} (* A + B, A * B on types, etc.  *) 

 (* from type_def word pattern  *)
prim_type_word : PA18b {} (* natural numbers etc.  *) 

 (* from function_head.symbol_pattern *)
prim_term_op : PA19 {} (* + - * / etc. *)

 (* from predicate_def.binary_symbol_pattern, binary infix with prec=0 or none  *)
prim_binary_relation_op : 
    | EQUAL
    | PA20 {} (* = < > etc. *)

 (* from predicate_def.binary_symbol_pattern, with prec < 0 *)
prim_propositional_op : PA21 {} (* logical connectives *)

 (* from predicate_def.identifier_pattern *)
prim_relation : 
| LIT_TRUE
| LIT_FALSE {} (* prop-valued *)

(* sections test:Sections *)

section_preamble : section_tag option(label) PERIOD {}
  section_tag : 
  | lit_document
  | lit_enddocument {}


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

  instruct_keyword_string : LIT_READ | LIT_LIBRARY | LIT_ERROR | LIT_WARNING {}

  instruct_command : bracket( instruct_keyword_command) {}
  instruct_int : bracket(instruct_keyword_int INTEGER {}) {}
  bool_tf : lit_true | lit_false {}
  instruct_bool : bracket(instruct_keyword_bool bool_tf {}){}
  instruct_string : bracket(instruct_keyword_string STRING {}) {}
  instruct_sep : SLASH | SLASHDASH {}
(* XX issue: VAR must be included to accommodate /-s plural formation *)
  instruct_synonym : bracket(LIT_SYNONYMS
    separated_nonempty_list (instruct_sep,nonempty_list(WORD)) {}) {}

(* variables *)

 (* Variables can be given the modifier LIT_INFERRING
 This is a blanket term for Lean style parameters, [type class inference],
 implicit arguments, etc.  

 When a variable is inferred, it can appear on the right-hand side of
 macros and definitions without appearing explicitly on the left-hand side. 

 For example, we can write
 We say that (X : set over alpha), (inferring alpha : Type) is finite iff ..., 
 Then the alpha is not required to appear to the left of the iff.

 For example, 
 Let x in X, (inferring C : has_in) stand for C.notation_in x X.

 If a section introduces an inferred variable into the context, then 
 the "CNL elaborator" inserts the corresponding where clause into every
 macro and definition that contains that variable on the right-hand side
 but not the left. 

  *)

(* var_modifier : option(LIT_INFERRING) {} *)
annotated_var : paren(VAR opt_colon_type {}) {()}
annotated_vars : paren(nonempty_list(VAR) opt_colon_type {}) {()}


tvar : VAR | annotated_var {()} 

assign_expr : ASSIGN expr {()} 

brace_assign :
  brace_semi(var_or_atomic (* opt_colon_type - not needed *) assign_expr {})  {()} 

brace_noassign : 
  brace_semi(var_or_atomics opt_colon_type {}) {()}

app_args :  (* can be empty *)
  option( brace_assign {}) list(tightest_expr) {()}


(* function and binder parameters. *)
  
(* rename required_args_template -> annotated_args 
  required_arg_template -> annotated_arg
*)
args_template : option( brace_noassign {}) annotated_args {()}

         (* A brace_semi as the first argument to a function is
            ambiguious: it could be either a brace_noassign or a required
            arg that takes the form of a make_term.  The convention is
            that the principal interpretation is brace_noassign.  But if it
            contains a field name that is not an optional argument,
            then it is interpreted as a make_term. *)

  annotated_args : list(annotated_arg) {}

  (* convention - all types are the same within parentheses *)
    annotated_arg :
    | paren(var_or_atomics opt_colon_type {})
    | var_or_atomic {}


  var_or_atomic : VAR | atomic {()}
  var_or_atomics : nonempty_list(var_or_atomic) {()}   

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

tightest_args : brace_noassign list(tightest_arg) {()}

  tightest_arg : 
  | tightest_expr
  | paren(var_or_atomic var_or_atomics opt_colon_sort {})
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

holding_var : option(COMMA paren(LIT_HOLDING comma_nonempty_list(VAR) {}) {}) {}

(* expressions *)

expr : general_type | term | prop | proof_expr | sort_expr {}

tightest_expr : (* what is allowed as an arg to function calls *)
| tightest_term
| tightest_prop
| tightest_type
| proof_expr
{()}

(* sorts *)

sort_expr : (* Side condition: args should be nonempty *)
| list(binder_type ARROW {}) lit_sort 
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
(* moved to text_item : mutual_inductive_type *)
| structure (* declaration *)
| field_type 
{()}

paren_type : paren(general_type) {()} (* -> paren_expr in implementation *)

annotated_type : paren(general_type COLON ID_TYPE {}) {()}

controlseq_type : cs_brace(prim_type_controlseq) {()}

const_type : prim_identifier_type {()}

field_type : tightest_term prim_field_type_accessor {}

overstructure_type :
| prim_structure app_args option(over_args {}) {()}

  over_args :
  | LIT_OVER brace_semi(var_or_atomic assign_expr {})
  | LIT_OVER tightest_term 
  | paren(LIT_OVER comma_nonempty_list(tightest_expr {}) {})
      {}

 (* if not annotated here, the VAR should be previously annotated in the context. *)
var_type : 
| VAR 
| paren(VAR COLON ID_TYPE {}) {()}

subtype :  brace(term option(holding_var) TMID statement {}) {()}

app_type : 
| tightest_type app_args 
| overstructure_type (* identifier *)
{()}

binder_type : 
| app_type 
| prim_pi_binder tightest_args lit_binder_comma binder_type 
{()}

(** binop_type *)
 (* for product types A * B, sum types A + B, 
    including arrows A -> B,
    including Agda style dependent arrows (x:A) -> B x.
    all type operators are right assoc with the same precedence
    N.B. binder_types is tighter than binop_type, which might be non-intuitive.  *)
binop_type : 
    option( brace_noassign {})
      list(type_operand type_op {}) binder_type  {()}

  type_op :
  | prim_type_op 
  | cs_brace(prim_type_op_controlseq)
  {}

  type_operand :
  | binder_type
  | agda_vars {} (* for Agda style dependent typing *)

  agda_vars : 
    nonempty_list(annotated_vars) {}

opentail_type :
| binop_type
| quotient_type
| coercion_type
{}

 (* agda_pi_type : nonempty_list(annotated_vars) ARROW opentail_type {} *)

quotient_type : LIT_QUOTIENT option(LIT_OF) general_type LIT_BY term {}

coercion_type : 
| COERCION term (* explicit coercion *)
{}

general_type : attribute(opentail_type) {}


 (* extend typing relation to prim_relation
    so that (r:R a) means (r : {x // R a x})
  *)

colon_type :
  | COLON general_type
  | COLON prim_relation app_args
  | COLON coerced_type 
{}

coerced_type :
      coercion_type
      | term {}

opt_colon_type : option(colon_type) {}



(** inductive types *)

inductive_type : LIT_INDUCTIVE identifier args_template 
  opt_colon_sort list(opt_alt_constructor) LIT_END {()}

  opt_alt_constructor : ALT identifier args_template opt_colon_type {}


(* 11/25/2019. Break mutual inductive into separate inductive types.
mutual_inductive_type : LIT_INDUCTIVE
  comma_nonempty_list(identifier) args_template 
  list(LIT_WITH identifier args_template opt_colon_type
       list(alt_constructor) {}) 
  LIT_END
  {()}
*)
  (*  alt_constructor : ALT identifier args_template colon_type {} *)

(** structure *)

 (* we require at least one field.  Later, if needed
    we can create prop_structure, without fields. 
    fields cannot be called a,an,with. 
  *)

structure : option(LIT_NOTATIONAL) LIT_STRUCTURE 
  option(lit_param) args_template
  option(LIT_WITH) brace_semi(extended_field)
  option(option(lit_with_properties) satisfying_preds {}) {}

  lit_param : LIT_WITH LIT_PARAMETERS {}

  (* If the field identifier is a var, satisfaction ignores its name.
   If the field identifier is a prim_structure, it becomes embedded. 
   *)
  extended_field : 
  | field
  | satisfying_pred {}

  field : field_prefix field_identifier option(assign_expr) {}

  field_identifier : 
  | prim_structure
  | var_or_atomic opt_colon_sort
  | var_or_atomic opt_colon_type {}

  field_prefix : option(option(lit_a) 
                   comma_nonempty_list(lit_field_key) {}) {}

  (* We are departing from these rules in Python implementation *)
  
  satisfying_preds : brace_semi(satisfying_pred) {}
  satisfying_pred : option(atomic COLON {}) prop {}

(* proof expressions (distinct from proof scripts). *)

proof_expr : 
| SYMBOL_QED 
| paren(proof_expr)
{()}

(* APPLYSUB handles subscripts coming from a TeX file.

   In brief, 
   x_1 is an identifier.
   x APPLYSUB {1} is equivalent to x 1 and is the deTeXed form of x_{1}.
   x APPLYSUB {i j} is equivalent to x i j.  (This is perhaps a surprise.) 
   x APPLYSUB {(f j)} is equivalent to x (f j).
 *)

(* Terms *)



(** tightest_term was tightest_post_term *)
tightest_suffix :
 | APPLYSUB tightest_subscript
 | prim_field_term_accessor (* FIELD_ACCESSOR *) {}

tightest_term : 
 | tightest_prefix list(tightest_suffix)
{()}

  (* was tightest_terms *)
  tightest_subscript : paren(nonempty_list(tightest_term)) {()}

  tightest_prefix :
  | DECIMAL
  | INTEGER
  | STRING
  | BLANK 
  | VAR
  | prim_identifier_term
  | controlseq_term
  | delimited_term
  | alt_term
  {()}


controlseq_term : cs_brace(prim_term_controlseq) {()}

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

annotated_term : paren(term colon_type {}) {()}

make_term : LIT_MAKE option(tightest_type) brace_semi(var_or_atomic_or_blank opt_colon_type
  option(assign_expr {}) {}) {()}

  var_or_atomic_or_blank : 
  | var_or_atomic
  | BLANK {}

list_term : bracket(separated_list(SEMI,plain_term) {}) {()}

tuple_term : paren(plain_term COMMA comma_nonempty_list(plain_term) {}) {()}

set_enum_term : brace(comma_list(plain_term) {}) {()}

set_comprehension_term : brace(plain_term holding_var MID statement {}) {()} 

(** alt_term *)

alt_term : (* These bind tightly because of terminating END *)
| case_term
| match_term
| match_function
{}

 (*
   Case statements generate an obligation for exhaustive cases. 
   Matches must also be exhaustive. 
  *)
case_term : LIT_CASE (* term LIT_OF  *)
  nonempty_list(alt_case) LIT_END {}
  alt_case : ALT prop ASSIGN plain_term {()}

match_term : LIT_MATCH match_seq LIT_WITH 
  nonempty_list(ALT match_pats ASSIGN plain_term {}) LIT_END {()}

  match_seq : comma_nonempty_list(plain_term) {}
  match_pats : comma_nonempty_list(match_pat) {}
  match_pat : plain_term {} (* Variables that do not appear in match_seq are assumed fresh. *)

match_function : LIT_FUNCTION args_template 
  opt_colon_type nonempty_list(ALT match_pats ASSIGN plain_term {})
  LIT_END {()}

app_term : tightest_term app_args {()}

(** opentail term *)               

opentail_term : 
| lambda_term
| lambda_fun
| let_term
| if_then_else_term
| tdop_term
{()}

lambda_term : 
| prim_lambda_binder tightest_args lit_binder_comma opentail_term
(* MAPSTO takes a single arg, but the argument can be a more general pattern than other
   function specifications.  *)
| tdop_term  MAPSTO opentail_term
{()}

lambda_fun : LIT_FUN tightest_args 
  opt_colon_type ASSIGN opentail_term {()}

  (* let includes destructuring*)
let_term : LIT_LET tightest_prefix ASSIGN plain_term LIT_IN opentail_term {()}

if_then_else_term : LIT_IF prop LIT_THEN plain_term LIT_ELSE opentail_term {()}

where_term : opentail_term option(where_suffix) {()}
 (* where (Haskell style) *)
  where_suffix : LIT_WHERE brace_semi(   (* deprecated option(LIT_INFERRING)  *)
                 VAR opt_colon_type option(assign_expr) {}) {}

definite_term : 
| where_term
| option(LIT_THE) prim_definite_noun {}
    (* | paren(option(LIT_THE) prim_definite_noun {}) {} subsumed by paren(where_term) tightest_term *)

term : 
| option(prim_classifier) definite_term 
| any_name {}  

terms : and_comma_nonempty_list(term) {}

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

 (* Not_IMPLEMENTED 
    When implemented, this should give a check 
    that the term is plain *) 
plain_term : plain(term) {}
  plain(X) : X {} 


(** make. *)
(* make_term_opt_colon_type : make_term opt_colon_type {} *)

(** pseudoterms and attributes (forthel style) *)

attribute(X) : list(left_attribute) X option(right_attribute) {}

  left_attribute : 
  | option(LIT_NON) prim_simple_adjective 
  | prim_simple_adjective_multisubject {}

  right_attribute : 
  | and_comma_nonempty_list(is_pred) {}
  | LIT_THAT and_comma_nonempty_list(does_pred) {}
  | LIT_SUCH LIT_THAT statement {}

(* XX something is wrong here.  We don't want attributes around tvar?? 
   Doch! n that is positive.  G with finite order. *)
attribute_pseudoterm : attribute(typed_name_without_attribute) {}
  typed_name_without_attribute : 
  | prim_typed_name
  | tvar
  | prim_classifier tvar 
  | VAR lit_with ID_TYPE opentail_type (* don't include nested attribute in general_type *)
  | paren(typed_name_without_attribute)
{}

predicate_pseudoterm : attribute(plain_pred_pseudoterm) {}

  plain_pred_pseudoterm : 
  | opt_paren(tdop_rel_prop holding_var {}) {}

 (* A pseudoterm is not a term in the grammar. 
    It is a term-like entity that can be 
    quantified over by extracting the
    free variables from the pseudoterm and
    quantifying over them.
    For example, 'for all x,y < 5'.
  *)      

pseudoterm :
  | attribute_pseudoterm
  | predicate_pseudoterm {} 

pseudoterms : and_comma_nonempty_list(option(lit_a) pseudoterm {}) {}

any_name : 
  | lit_any comma_nonempty_list(any_arg) 
  | lit_any pseudoterm
  | lit_any general_type (* XX remove this? *)
  {}
  any_arg :
  | VAR
  | annotated_vars {}



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
{()}

  term_ops : nonempty_list(term_op) {}

  term_op : 
  | prim_term_op
  | cs_brace(prim_term_op_controlseq) {}

 (* prec = 0 *)
 (* We allow x,y < z < w. The first arg can be a list. *)
tdop_rel_prop : 
  tdop_terms
  nonempty_list(binary_relation_op tdop_term {}) {()}

  binary_relation_op :
  | prim_binary_relation_op
  | cs_brace(prim_binary_relation_controlseq)
  {()}

  tdop_terms : comma_nonempty_list(tdop_term) {}

 (* prec < 0, must be infix (possibly with multiple ops), subsumes binder_prop  *)
tdop_prop :
  binder_prop list(prop_ops binder_prop {}) {()}

(*    
  option(binder_prop) prop_ops
  list(binder_prop prop_ops {}) option(binder_prop) {}
 *)

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
| field_prop 
{()}

identifier_prop : prim_relation {()} (* *)

annotated_prop : paren(prop COLON ID_PROP {}) {()}

field_prop : tightest_term prim_field_prop_accessor {}

app_prop : tightest_prop app_args {()} 

binder_prop :
| app_prop
| tdop_rel_prop 
| lambda_predicate 
| prim_binder_prop args_template lit_binder_comma binder_prop {()}

lambda_predicate : 
| LIT_FUN tightest_args COLON ID_PROP ASSIGN tightest_prop {()}


prop : option(lit_classifier) tdop_prop {()}


(* statements *)

(** predicates *)
does_pred : option(lit_do) option(LIT_NOT) prim_verb {}
| option(lit_do) option(LIT_NOT) prim_verb_multisubject
| lit_has has_pred
| lit_is and_comma_nonempty_list(is_pred)
| lit_is and_comma_nonempty_list(is_aPred {}) {()}

is_pred : option(LIT_NOT) prim_adjective {}
| option(LIT_NOT) option(LIT_PAIRWISE) prim_adjective_multisubject
| lit_with has_pred {()}

is_aPred : option(LIT_NOT) option(lit_a) general_type {}
| option(LIT_NOT) definite_term {()}

has_pred : 
| and_comma_nonempty_list(article possessed_noun {})
| LIT_NO possessed_noun {()}

  possessed_noun : attribute(prim_possessed_noun) {()}



(** statement *)

statement : head_statement | chain_statement {()}

  head_statement : 
  | LIT_FOR and_comma_nonempty_list(any_name {}) COMMA statement {}
  | LIT_IF statement COMMA LIT_THEN statement (* != if-then-else *)
  | lit_its_wrong statement {()}

  chain_statement : 
  | and_or_chain
  | paren(and_or_chain) LIT_IFF statement 
  {()}

  comma_and : COMMA LIT_AND {}
  comma_or : COMMA LIT_OR {}

  and_or_chain : and_chain | or_chain | primary_statement {()}
  and_chain : separated_nonempty_list(comma_and, primary_statement {}) 
    COMMA LIT_AND head_primary {()}
  or_chain : separated_nonempty_list(comma_or, primary_statement {}) 
    COMMA LIT_OR head_primary {()}
  head_primary : head_statement | primary_statement {()}

(** primary statement *)
primary_statement :
  | simple_statement {}
  | there_is_statement
  | filler symbol_statement
  | filler const_statement {()}

  filler : PL2a option(phrase_list_filler) {}

  simple_statement : terms separated_nonempty_list(LIT_AND, does_pred) {()}

  there_is_statement : LIT_THERE lit_exist pseudoterms {}
  | LIT_THERE lit_exist LIT_NO pseudoterm {()}

  const_statement : option(LIT_THE) LIT_THESIS {}
  | option(LIT_THE) LIT_CONTRARY
  | lit_a LIT_CONTRADICTION {()}

  symbol_statement :
  | LIT_FORALL predicate_pseudoterm lit_binder_comma
    symbol_statement {}
  | LIT_EXISTS predicate_pseudoterm lit_binder_comma symbol_statement
  | (* moved to tightest_prop: prim_relation*)
  | LIT_NOT symbol_statement
  | paren(statement)
  | prop
  {()}



(* text *)
text : list(text_item) {}

text_item : 
 | section_preamble
 | instruction
 | declaration
 | macro
 | misc_text_item 
     {}

misc_text_item : 
 | synonym_item 
 | mutual_inductive_type_item 
 | mutual_inductive_def_item 
 | moreover_implements
 | namespace
     {}

synonym_item :
     option(LIT_WE) option(LIT_INTRODUCE) LIT_SYNONYMS 
       separated_nonempty_list(instruct_sep,nonempty_list(WORD)) PERIOD {}

mutual_inductive_type_item :
       lit_declare_mutual_inductive_type
         comma_nonempty_list(WORD) 
         option(lit_param args_template {})
         {}

mutual_inductive_def_item :
       lit_declare_mutual_inductive_def  
         comma_nonempty_list(WORD) 
         option(lit_param args_template {})
         {}

moreover_implements : 
         LIT_MOREOVER COMMA general_type LIT_IMPLEMENTS brace_semi(field) PERIOD {}

namespace : NOT_IMPLEMENTED {}

fiat : NOT_IMPLEMENTED {}



(* declaration test:declaration *)
declaration : axiom | definition | theorem | fiat {}

(** axiom *)
axiom : axiom_preamble list(assumption) 
  then_prefix statement PERIOD 
  list(LIT_MOREOVER statement PERIOD {})
  {}

  axiom_preamble : lit_axiom option(label) PERIOD {}

  assumption :
  | assumption_prefix statement PERIOD
  | let_annotation PERIOD {}
  assumption_prefix : 
(*  | LIT_LET. Remove ambiguity with statement starting with let_term. *)
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
    list(LIT_MOREOVER statement by_ref PERIOD {})
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

  canned_prefix : and_comma_nonempty_list(phrase_list_transition) option(COMMA) {}

  canned_proof : phrase_list_proof_statement {}

  case : LIT_CASE statement PERIOD proof_script {}

  choose : choose_prefix pseudoterms by_ref PERIOD choose_justify {}
  choose_justify : 
  | option(proof_script {}) {}
  choose_prefix : then_prefix option(lit_lets) lit_choose {}

  (* changed slightly in ocaml parser to remove a grammar ambiguity *)
  by_method : option(LIT_BY proof_method {}) {}
  proof_method : LIT_CONTRADICTION
    | LIT_CASE LIT_ANALYSIS
    | LIT_INDUCTION option(LIT_ON plain_term {}) {}
  by_ref : option(paren(LIT_BY ref_item {})) {}
  ref_item : and_comma_nonempty_list(option(lit_location) label {}) {}

(** This exists and is well-defined. *)
this_exists : LIT_THIS
  and_comma_nonempty_list(this_directive_pred) PERIOD {()}

  this_directive_pred : LIT_IS
    and_comma_nonempty_list(this_directive_adjective)
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

(* definition *)
definition : definition_preamble list(assumption) 
  definition_affirm {}

  definition_preamble : lit_def option(label) PERIOD {}
  definition_affirm : definition_statement PERIOD 
    list(this_exists) {}

definition_statement :
| classifier_def 
| type_def
| function_def
| predicate_def
{()}

  copula : lit_is option(lit_defined_as) | lit_denote {}
  function_copula : copula | opt_colon_type ASSIGN {}
  iff_junction : lit_iff {}
  opt_say : option(lit_we_say) {}
  opt_record : option(lit_we_record) {}
  opt_define : 
  | option(lit_lets) option(LIT_DEFINE)
  | opt_record {}
  macro_inferring : paren(LIT_INFERRING nonempty_list(VAR) opt_colon_type {}) {}


  classifier_def : LIT_LET classifier_words lit_is option(lit_a) lit_classifier {}
    classifier_words : comma_nonempty_list(WORD) {}

  type_def : 
  | opt_define type_head COLON ID_TYPE copula option(lit_a) general_type
  | opt_define type_head copula LIT_THE lit_type general_type {}

    type_head :  
    | symbol_pattern (* fixed precedence level, right assoc *)
    | type_word_pattern 
    | identifier_pattern 
    | controlseq_pattern 
    {} 

  function_def : opt_define function_head option(macro_inferring {})
    function_copula option(lit_equal) option(LIT_THE) plain_term {}

    function_head :
    | function_word_pattern
    | symbol_pattern
    | identifier_pattern  {}

  predicate_def : opt_say predicate_head option(macro_inferring {}) iff_junction statement {}
    predicate_head :
    | identifier_pattern (* before word pattern *)
    | predicate_word_pattern
    | symbol_pattern 
        {}

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
  identifier patterns start with an identifier.
  predicate word patterns contains words
  binary symbol patterns contain start with tvar then a symbol.

  Functions use the copula.
  word patterns starts with LIT_THE word ...
  symbol patterns contain a symbol or CONTROLSEQ.
  identifier patterns start with an identifier.

  Types use the copula.
  type_def from function_def distinguished by COLON ID_TYPE
  before the copula. 
  *)

(* macro *)

macro : option(insection) macro_bodies {()}

  insection : LIT_IN LIT_THIS lit_document {}

  macro_bodies : macro_body list(SEMI option(LIT_AND) macro_body {}) PERIOD {}

  macro_body : 
  | classifier_def 
  | type_def 
  | function_def  
  | predicate_def  
  | let_annotation 
  | binder_def 
  | we_record_def
  | enter_namespace
  {}

  (* if LIT_FIX is used in let_annotation, it plays the role of a Lean
     parameter, so that it inserted as an implicit parameter in every
     macro where the variable appears on the right-hand side but not
     the left.

     If LIT_LET is used, the type information is associated with the
     variable wherever it is used, but there is no automatic insertion. 

  *)

  let_annotation_prefix :
  LIT_LET comma_nonempty_list(VAR) LIT_BE option(lit_a) option(LIT_FIXED) {}

  let_annotation : 
  | lit_fix comma_nonempty_list(annotated_vars) {}
  | let_annotation_prefix general_type {} 
  | let_annotation_prefix type_or_prop {}
  type_or_prop : lit_type | lit_proposition {}

  we_record_def : lit_we_record comma_nonempty_list(plain_term) {}

  enter_namespace : LIT_WE LIT_ENTER LIT_THE LIT_NAMESPACE identifier {}                    

  binder_def : LIT_LET LIT_THE LIT_BINDER symbol paren(VAR opt_colon_type {})
                 lit_denote plain_term {}


 (* subsumed by type_def, function_def, etc. 

  type_macro : LIT_LET type_word_pattern
    lit_denote lit_a general_type {}

  function_macro : LIT_LET function_head 
    lit_denote plain_term {}

  predicate_macro :
  | LIT_LET predicate_word_pattern lit_denote statement {}
  | LIT_LET symbol_pattern lit_denote statement {}
 *)

(* pattern *)

 (* restriction: words in pattern cannot be a variant of
    "to be", "called", "iff" "a" "stand" "denote"
    cannot start with "the"  

    Parentheses give optional words in pattern.
    (or word) introduces a synonym to the previous single word. 

    In word, WORDS in parentheses should exclude LIT_OR. 
  *)

 

word_pattern : words list(tvar words {}) option(tvar) {}

  word : 
  | WORD
  | paren(comma_nonempty_list(LIT_OR nonempty_list(WORD) {}) {})
  | paren(WORD) {}

  words : WORD list(word) {}

type_word_pattern : option(lit_a) word_pattern {}

function_word_pattern : LIT_THE word_pattern {}

predicate_word_pattern :
| notion_pattern 
| adjective_pattern 
| adjective_multisubject_pattern
| verb_pattern
| verb_multisubject_pattern 
{}

  notion_pattern : tvar LIT_IS lit_a word_pattern {}

  adjective_pattern : tvar LIT_IS option(LIT_CALLED) word_pattern {}

  adjective_multisubject_pattern : 
    var_multisubject LIT_ARE option(LIT_CALLED) word_pattern {} 

  verb_pattern : tvar word_pattern {} 

  verb_multisubject_pattern : var_multisubject word_pattern {}

  var_multisubject :
  | tvar COMMA tvar
  | paren(VAR COMMA VAR colon_type {}) {}

identifier_pattern :
| identifier args_template {}
| BLANK args_template {} (* instance can be anonymous *)

controlseq_pattern : CONTROLSEQ list(brace(tvar)) {} (* subsumed by symbol_pattern *)

binary_symbol_pattern : tvar symbol tvar option(paren_precedence_level) {} (* subsumed by symbol_pattern *)

symbol_pattern : option(tvar) symbol list(tvar symbol {}) 
  option(tvar) option(paren_precedence_level) {}

  symbol : SLASH | SLASHDASH | SYMBOL | controlseq_pattern {} 

paren_precedence_level :
| precedence_level
| paren(precedence_level) {}

  precedence_level :
  | LIT_WITH LIT_PRECEDENCE INTEGER 
    option(LIT_AND lit_left LIT_ASSOCIATIVITY {}) {}

(* program *)

program_text : text | stub_nonterminal { "done" }

program : program_text EOF {}

