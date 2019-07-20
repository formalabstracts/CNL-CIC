{-
Author(s): Jesse Michael Han (2019)

Parsing axioms.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Axiom where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic
import CNLean.Token

-- data Axiom = Axiom { preamble :: AxiomPreamble, assumptions :: [Assumption], thenPrefix :: ThenPrefix, statement :: Statement }

data AxiomPreamble = AxiomPreamble Token (Maybe Label) -- parse (Lit AXIOM), maybe a label, optional period.
  deriving (Show, Eq)

parseAxiomPreamble :: Parser AxiomPreamble
parseAxiomPreamble = do
  tk <- ((parseLit_aux AXIOM) <||> (parseLit_aux CONJECTURE) <||> (parseLit_aux HYPOTHESIS) >>= return . Lit)
  maybeLabel <- option (parseLabel)
  try (parsePeriod)
  return $ AxiomPreamble tk maybeLabel

  

-- data Assumption =
--     AssumptionAssumptionPrefix AssumptionPrefix Statement -- parsed with period at end
--   | AssumptionLetAnnotation LetAnnotation -- parsed with period at end
  
-- data AssumptionPrefix =
--     LitLet Token -- LIT_LET
--   | LitLets Token Token Token (Maybe Token) -- lit_lets lit_assume option(LIT_THAT) {}

-- newtype ThenPrefix = ThenPrefix (Maybe Token) --   then_prefix : option(lit_then) {}

-- data Statement =
--     StatementHeadStatement HeadStatement
--   | StatementChainStatement ChainStatement

-- newtype LetAnnotation = LetAnnotation [AnnotatedVars] -- LIT_LET comma_nonempty_list(annotated_vars)

-- data AnnotatedVars = AnnotatedVars {varModifier :: VarModifier, vars :: [Token], maybeColonType :: (Maybe ColonType)}

-- newtype VarModifier = VarModifier (Maybe Token)

-- parseVarModifier :: Parser VarModifier
-- parseVarModifier = option (parseLit_aux FIXED <||> parseLit_aux IMPLICIT <||> parseLit_aux RESOLVED <||> parseLit_aux REMOVE >>= return . Lit) >>= return . VarModifier

-- data ColonType = ColonType {generalType :: GeneralType} -- COLON general_type

-- data GeneralType = GeneralType OpenTailType

-- data OpenTailType =
--   OpenTailTypeBinOpType BinOpType | OpenTailTypeQuotientType QuotientType | CoercionType CoercionType | PrimStructure PrimStructure

-- data BinOpType = BinOpType { head :: TypeOperand, tail :: [(TypeOp,TypeOperand)]}

-- data TypeOp =
--     TypeOpPrimTypeOp Token -- TODO(jesse) fix me
--   | TypeOpDummyConstructor -- TODO(jesse) fix me

-- parseTypeOp :: Parser TypeOp
-- parseTypeOp = empty

-- data TypeOperand =
--     TypeOperandBinderType BinderType
--   | TypeOperandDependentVars DependentVars

-- data BinderType =
--     BinderTypeAppType
--   | BinderTypePrimPiBinder PrimPiBinder GeneralizedArgs LitBinderComma BinderType

-- data GeneralizedArgs = GeneralizedArgs {optArgs :: OptArgs, generalizedArg :: [GeneralizedArg] }

-- data GeneralizedArg =
--     GeneralizedArg TightestTerm
--   | GeneralizedArgVarOrAtomic {varOrAtomic1 :: Token, varOrAtomic2 :: Token, optColonType :: (Maybe ColonType)}

-- data DependentVars = DependentVars {maybeOptArgs :: (Maybe OptArgs), annotatedVars :: [AnnotatedVars]}

-- -- lexically, OptArgs is a brace-enclosed, semicolon separated lists of variables or atomics, optionally annotated with types
-- newtype OptArgs = OptArgs [(Token, Maybe ColonType)]
