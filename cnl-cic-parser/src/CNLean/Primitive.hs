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
import CNLean.Basic.Pattern

----A primitive classifier phrase is a list [Text], where each item must be parsed verbatim, but during parsing the items may be separated by arbitrary whitespace
newtype PrimClassifier = PrimClassifier [Text] 
  deriving (Show, Eq)

-- note: as opposed to the Naproche-SAD implementation, we do not parse derived primitives by defining a modified parser, but rather produce the derived patterns by modifying the primitive patterns and storing them whenever they are registered (and therefore only use a single pattern parser)

---- parsePrimClassifier attempts to parse any of the classifier phrases currently in the FState.
parsePrimClassifier :: Parser PrimClassifier
parsePrimClassifier = PrimClassifier <$> (get >>= parse_any_Lit . clsList)

--  (* from function_def.binary_controlseq_pattern, prec > 0 *)
-- prim_term_op_controlseq : PA1 {}
newtype PrimTermOpControlSeq = PrimTermOpControlSeq ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimTermOpControlSeq :: Parser PrimTermOpControlSeq
parsePrimTermOpControlSeq = PrimTermOpControlSeq <$> (gets primTermOpControlSeq >>= parse_any_Patts)
  
--  (* from predicate_def.binary_controlseq_pattern, binary, prec=0 or none *)
-- prim_binary_relation_controlseq : PA1a {}
newtype PrimBinaryRelationControlSeq = PrimBinaryRelationControlSeq ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimBinaryRelationControlSeq :: Parser PrimBinaryRelationControlSeq
parsePrimBinaryRelationControlSeq = PrimBinaryRelationControlSeq <$> (gets primBinaryRelationControlSeq >>= parse_any_Patts)


--  (* from predicate_def.binary_controlseq_pattern, prec < 0 *)
-- prim_propositional_op_controlseq : PA1b {}
newtype PrimPropositionalOpControlSeq = PrimPropositionalOpControlSeq ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimPropositionalOpControlSeq :: Parser PrimPropositionalOpControlSeq
parsePrimPropositionalOpControlSeq = PrimPropositionalOpControlSeq <$> (gets primPropositionalOpControlSeq >>= parse_any_Patts)

--  (* from type_head.binary_controlseq_pattern, binary prec < 0 *)
-- prim_type_op_controlseq : PA1c {}
newtype PrimTypeOpControlSeq = PrimTypeOpControlSeq ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimTypeOpControlSeq :: Parser PrimTypeOpControlSeq
parsePrimTypeOpControlSeq = PrimTypeOpControlSeq <$> (gets primTypeOpControlSeq >>= parse_any_Patts)

--  (* from function_def.controlseq_pattern, no prec *)
-- prim_term_controlseq : PA1d {}
newtype PrimTermControlSeq = PrimTermControlSeq ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimTermControlSeq :: Parser PrimTermControlSeq
parsePrimTermControlSeq = PrimTermControlSeq <$> (gets primTermControlSeq >>= parse_any_Patts)

--  (* from type_head.controlseq_pattern, no prec *)
-- prim_type_controlseq : PA2 {}
newtype PrimTypeControlSeq = PrimTypeControlSeq ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimTypeControlSeq :: Parser PrimTypeControlSeq
parsePrimTypeControlSeq = PrimTypeControlSeq <$> (gets primTypeControlSeq >>= parse_any_Patts)

--  (* from NOT_IMPLEMENTED *)
-- prim_lambda_binder : PA3 {} (* term binders *)
newtype PrimLambdaBinder = PrimLambdaBinder ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimLambdaBinder :: Parser PrimLambdaBinder
parsePrimLambdaBinder = PrimLambdaBinder <$> (gets primLambdaBinder >>= parse_any_Patts)

--  (* from NOT_IMPLEMENTED *)
-- prim_pi_binder : PA4 {} (* type binders *)
newtype PrimPiBinder = PrimPiBinder ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimPiBinder :: Parser PrimPiBinder
parsePrimPiBinder = PrimPiBinder <$> (gets primPiBinder >>= parse_any_Patts)

--  (* from NOT_IMPLEMENTED *)
-- prim_binder_prop : PA5 {}
newtype PrimBinderProp = PrimBinderProp ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimBinderProp :: Parser PrimBinderProp
parsePrimBinderProp = PrimBinderProp <$> (gets primBinderProp >>= parse_any_Patts)

--  (* from declarations of structures, quotients, 
--     inductive types, mutual inductive types  *) 
-- prim_typed_name : PA6 {}
newtype PrimTypedName = PrimTypedName ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimTypedName :: Parser PrimTypedName
parsePrimTypedName = PrimTypedName <$> (gets primTypedName >>= parse_any_Patts)

--  (* from NOT_IMPLEMENTED. Forthel: primClassRelation *)
--  (* prim_free_predicate : PA7 {} *) (* used in quantifier scoping *)
newtype PrimFreePredicate = PrimFreePredicate ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimFreePredicate :: Parser PrimFreePredicate
parsePrimFreePredicate = PrimFreePredicate <$> (gets primFreePredicate >>= parse_any_Patts)

--  (* from adjective_pattern *)
-- prim_adjective : PA8 {}
newtype PrimAdjective = PrimAdjective ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimAdjective :: Parser PrimAdjective
parsePrimAdjective = PrimAdjective <$> (gets primAdjective >>= parse_any_Patts)

--  (* from adjective_multisubject_pattern *)
-- prim_adjective_multisubject : PA9 {}
newtype PrimAdjectiveMultiSubject = PrimAdjectiveMultiSubject ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimAdjectiveMultiSubject :: Parser PrimAdjectiveMultiSubject
parsePrimAdjectiveMultiSubject = PrimAdjectiveMultiSubject <$> (gets primAdjectiveMultiSubject >>= parse_any_Patts)

--  (* derived from prim_adjective as in Forthel. *)
-- prim_simple_adjective : PA10 {}
newtype PrimSimpleAdjective = PrimSimpleAdjective ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimSimpleAdjective :: Parser PrimSimpleAdjective
parsePrimSimpleAdjective = PrimSimpleAdjective <$> (gets primSimpleAdjective >>= parse_any_Patts)

--  (* derived from prim_adjective_multiSubject as in Forthel *)
-- prim_simple_adjective_multiSubject : PA11 {}
newtype PrimSimpleAdjectiveMultiSubject = PrimSimpleAdjectiveMultiSubject ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimSimpleAdjectiveMultiSubject :: Parser PrimSimpleAdjectiveMultiSubject
parsePrimSimpleAdjectiveMultiSubject = PrimSimpleAdjectiveMultiSubject <$> (gets primSimpleAdjectiveMultiSubject >>= parse_any_Patts)

--  (* from NOT_IMPLEMENTED *)
-- prim_definite_noun : PA12 {} (* functions and terms *)
newtype PrimDefiniteNoun = PrimDefiniteNoun ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimDefiniteNoun :: Parser PrimDefiniteNoun
parsePrimDefiniteNoun = PrimDefiniteNoun <$> (gets primDefiniteNoun >>= parse_any_Patts)

--  (* from NOT_IMPLEMENTED *)
-- prim_identifier_term : PA13 {} (* all identifiers that are terms *)
newtype PrimIdentifierTerm = PrimIdentifierTerm ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimIdentifierTerm :: Parser PrimIdentifierTerm
parsePrimIdentifierTerm = PrimIdentifierTerm <$> (gets primIdentifierTerm >>= parse_any_Patts)

--  (* from NOT_IMPLEMENTED *)
-- prim_prefix_function : PA14 {} (* symbolic functions like sin,cos,exp *)
newtype PrimPrefixFunction = PrimPrefixFunction ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimPrefixFunction :: Parser PrimPrefixFunction
parsePrimPrefixFunction = PrimPrefixFunction <$> (gets primPrefixFunction >>= parse_any_Patts)

--  (* derived as in Forthel *)
-- prim_possessed_noun : PA15 {}
newtype PrimPossessedNoun = PrimPossessedNoun ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimPossessedNoun :: Parser PrimPossessedNoun
parsePrimPossessedNoun = PrimPossessedNoun <$> (gets primPossessedNoun >>= parse_any_Patts)

--  (* from verb_pattern *)
-- prim_verb : PA16 {}
newtype PrimVerb = PrimVerb ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimVerb :: Parser PrimVerb
parsePrimVerb = PrimVerb <$> (gets primVerb >>= parse_any_Patts)

--  (* from verb_multiset_pattern *)
-- prim_verb_multisubject : PA17 {}
newtype PrimVerbMultiSubject = PrimVerbMultiSubject ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimVerbMultiSubject :: Parser PrimVerbMultiSubject
parsePrimVerbMultiSubject = PrimVerbMultiSubject <$> (gets primVerbMultiSubject >>= parse_any_Patts)

--  (* from type_def, when infix with precedence *)
-- prim_type_op : PA18a {} (* A + B, A * B on types, etc.  *)

newtype PrimTypeOp = PrimTypeOp ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimTypeOp :: Parser PrimTypeOp
parsePrimTypeOp = PrimTypeOp <$> (gets primTypeOp >>= parse_any_Patts)

--  (* from function_head.symbol_pattern *)
-- prim_term_op : PA19 {} (* + - * / etc. *)
newtype PrimTermOp = PrimTermOp ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimTermOp :: Parser PrimTermOp
parsePrimTermOp = PrimTermOp <$> (gets primTermOp >>= parse_any_Patts)

--  (* from predicate_def.symbol_pattern, binary infix with prec=0 or none  *)
-- prim_binary_relation_op : PA20 {} (* = < > etc. *)
newtype PrimBinaryRelationOp = PrimBinaryRelationOp ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimBinaryRelationOp :: Parser PrimBinaryRelationOp
parsePrimBinaryRelationOp = PrimBinaryRelationOp <$> (gets primBinaryRelationOp >>= parse_any_Patts)

--  (* from predicate_def.symbol_pattern, with prec < 0 *)
-- prim_propositional_op : PA21 {} (* logical connectives *)
newtype PrimPropositionalOp = PrimPropositionalOp ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimPropositionalOp :: Parser PrimPropositionalOp
parsePrimPropositionalOp = PrimPropositionalOp <$> (gets primPropositionalOp >>= parse_any_Patts)

--  (* from predicate_def.identifier_pattern *)
-- prim_relation : PA22 {} (* prop-valued *)
newtype PrimRelation = PrimRelation ([ParsedPatt]) 
  deriving (Show, Eq)

parsePrimRelation :: Parser PrimRelation
parsePrimRelation = PrimRelation <$> (gets primRelation >>= parse_any_Patts)

