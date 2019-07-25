{-
Author(s): Jesse Michael Han (2019)

Parsing syntactic primitives.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Primitive where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic.Basic
import CNLean.Basic.Token
import CNLean.Basic.State

---- parsePrimClassifier attempts to parse any of the classifier phrases currently in the FState.
parsePrimClassifier :: Parser [Text]
parsePrimClassifier = get >>= parse_any_Lit . clsList

--  (* from function_def.binary_controlseq_pattern, prec > 0 *)
-- prim_term_op_controlseq : PA1 {}
newtype PrimTermOpControlSeq = PrimTermOpControlSeq [[Patt]]

--  (* from predicate_def.binary_controlseq_pattern, binary, prec=0 or none *)
-- prim_binary_relation_controlseq : PA1a {}
newtype PrimBinaryRelationControlSeq = PrimBinaryRelationControlSeq [[Patt]]

--  (* from predicate_def.binary_controlseq_pattern, prec < 0 *)
-- prim_propositional_op_controlseq : PA1b {}
newtype PrimPropositionalOpControlSeq = PrimPropositionalOpControlSeq [[Patt]]

--  (* from type_head.binary_controlseq_pattern, binary prec < 0 *)
-- prim_type_op_controlseq : PA1c {}
newtype PrimTypeOpControlSeq = PrimTypeOpControlSeq [[Patt]]

--  (* from function_def.controlseq_pattern, no prec *)
-- prim_term_controlseq : PA1d {}
newtype PrimTermControlSeq = PrimTermControlSeq [[Patt]]

--  (* from type_head.controlseq_pattern, no prec *)
-- prim_type_controlseq : PA2 {}
newtype PrimTypeControlSeq = PrimTypeControlSeq [[Patt]]

--  (* from NOT_IMPLEMENTED *)
-- prim_lambda_binder : PA3 {} (* term binders *)
newtype PrimLambdaBinder = PrimLambdaBinder [[Patt]]

--  (* from NOT_IMPLEMENTED *)
-- prim_pi_binder : PA4 {} (* type binders *)
newtype PrimPiBinder = PrimPiBinder [[Patt]]

--  (* from NOT_IMPLEMENTED *)
-- prim_binder_prop : PA5 {}
newtype PrimBinderProp = PrimBinderProp [[Patt]]

--  (* from declarations of structures, quotients, 
--     inductive types, mutual inductive types  *) 
-- prim_typed_name : PA6 {}
newtype PrimTypedName = PrimTypedName [[Patt]]

--  (* from NOT_IMPLEMENTED. Forthel: primClassRelation *)
--  (* prim_free_predicate : PA7 {} *) (* used in quantifier scoping *)
newtype PrimFreePredicate = PrimFreePredicate [[Patt]]

--  (* from adjective_pattern *)
-- prim_adjective : PA8 {}
newtype PrimAdjective = PrimAdjective [[Patt]]

--  (* from adjective_multisubject_pattern *)
-- prim_adjective_multisubject : PA9 {}
newtype PrimAdjectiveMultiSubject = PrimAdjectiveMultiSubject [[Patt]]

--  (* derived from prim_adjective as in Forthel. *)
-- prim_simple_adjective : PA10 {}
newtype PrimSimpleAdjective = PrimSimpleAdjective [[Patt]]

--  (* derived from prim_adjective_multiSubject as in Forthel *)
-- prim_simple_adjective_multiSubject : PA11 {}
newtype PrimSimpleAdjectiveMultiSubject = PrimSimpleAdjectiveMultiSubject [[Patt]]

--  (* from NOT_IMPLEMENTED *)
-- prim_definite_noun : PA12 {} (* functions and terms *)
newtype PrimDefiniteNoun = PrimDefiniteNoun [[Patt]]

--  (* from NOT_IMPLEMENTED *)
-- prim_identifier_term : PA13 {} (* all identifiers that are terms *)
newtype PrimIdentifierTerm = PrimIdentifierTerm [[Patt]]

--  (* from NOT_IMPLEMENTED *)
-- prim_prefix_function : PA14 {} (* symbolic functions like sin,cos,exp *)
newtype PrimPrefixFunction = PrimPrefixFunction [[Patt]]

--  (* derived as in Forthel *)
-- prim_possessed_noun : PA15 {}
newtype PrimPossessedNoun = PrimPossessedNoun [[Patt]]

--  (* from verb_pattern *)
-- prim_verb : PA16 {}
newtype PrimVerb = PrimVerb [[Patt]]

--  (* from verb_multiset_pattern *)
-- prim_verb_multisubject : PA17 {}
newtype PrimVerbMultisubject = PrimVerbMultisubject [[Patt]]

--  (* from type_def, when infix with precedence *)
-- prim_type_op : PA18a {} (* A + B, A * B on types, etc.  *)

newtype PrimTypeOp = PrimTypeOp [[Patt]]

--  (* from function_head.symbol_pattern *)
-- prim_term_op : PA19 {} (* + - * / etc. *)
newtype PrimTermOp = PrimTermOp [[Patt]]

--  (* from predicate_def.symbol_pattern, binary infix with prec=0 or none  *)
-- prim_binary_relation_op : PA20 {} (* = < > etc. *)
newtype PrimBinaryRelationOp = PrimBinaryRelationOp [[Patt]]

--  (* from predicate_def.symbol_pattern, with prec < 0 *)
-- prim_propositional_op : PA21 {} (* logical connectives *)
newtype PrimPropositionalOp = PrimPropositionalOp [[Patt]]

--  (* from predicate_def.identifier_pattern *)
-- prim_relation : PA22 {} (* prop-valued *)
newtype PrimRelation = PrimRelation [[Patt]]


data DoesPred = DoesPredDummyConstructor -- TODO(jesse): fix me
