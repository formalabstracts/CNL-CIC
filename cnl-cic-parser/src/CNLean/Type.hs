{-# LANGUAGE DataKinds #-} -- might need to remove this later, i'm not really sure what this does yet
{-# LANGUAGE PolyKinds #-} -- might need to remove this later, i'm not really sure what this does yet
{-
Author(s): Jesse Michael Han (2019)

Parsing types and type ascriptions.
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

import CNLean.Basic.Basic
import CNLean.Basic.Token

data ColonType = ColonType {generalType :: GeneralType} -- COLON general_type

parseColonType :: parserColonType
parseColonType = (parseLit_aux COLON) *> parseGeneralType

data GeneralType = GeneralType OpenTailType

parseGeneralType :: parserGeneralType
parseGeneralType = empty

data OpenTailType =
  OpenTailTypeBinOpType BinOpType | OpenTailTypeQuotientType QuotientType | CoercionType CoercionType | PrimStructure PrimStructure

data QuotientType = QuotientType {domain :: GeneralType, eqv :: Term}

-- data Term =
--     TermDefiniteTerm DefiniteTerm
--   | TermAnyName AnyName

data DefiniteTerm =
    DefiniteTermSymbolicTerm SymbolicTerm
  | DefiniteTermNoun PrimDefiniteNoun
  | DefiniteTermParenNoun PrimDefiniteNoun


data SymbolicTerm = SymbolicTerm OpenTailTerm (Maybe WhereSuffix)

data OpenTailTerm =
    LambdaTerm LambdaTerm
  | LambdaFun  LambdaFun
  | LetTerm LetTerm
  | IfThenElseTerm IfThenElseTerm
  | TdopTerm TdopTerm

data LambdaTerm =
    PrimLambdaBinder PrimLambdaBinder GeneralizedArgs OpenTailTerm -- parse binder comma to separate args from tail
  | LambdaTermGeneralizedArg GeneralizedArg OpenTailTerm -- parse mapsto to separate args from tail

data PrimLambdaBinder = DummyConstructor

data BinOpType = BinOpType { head :: TypeOperand, tail :: [(TypeOp,TypeOperand)]}

data TypeOp =
    TypeOpPrimTypeOp Token -- TODO(jesse) fix me
  | TypeOpDummyConstructor -- TODO(jesse) fix me

parseTypeOp :: Parser TypeOp
parseTypeOp = empty

data TypeOperand =
    TypeOperandBinderType BinderType
  | TypeOperandDependentVars DependentVars

data BinderType =
    BinderTypeAppType
  | BinderTypePrimPiBinder PrimPiBinder GeneralizedArgs LitBinderComma BinderType

data GeneralizedArgs = GeneralizedArgs {optArgs :: OptArgs, generalizedArg :: [GeneralizedArg] }

data GeneralizedArg =
    GeneralizedArg TightestTerm
  | GeneralizedArgVarOrAtomic {varOrAtomic1 :: Token, varOrAtomic2 :: Token, optColonType :: (Maybe ColonType)}

data DependentVars = DependentVars {maybeOptArgs :: (Maybe OptArgs), annotatedVars :: [AnnotatedVars]}

-- lexically, OptArgs is a brace-enclosed, semicolon separated lists of variables or atomics, optionally annotated with types
newtype OptArgs = OptArgs [(Token, Maybe ColonType)]


data Term =
    TermDefiniteTerm DefiniteTerm
  | TermAnyName AnyName

data AnyName =
    AnyNameAnyArgs [AnyArg]
  | AnyNameTypedName TypedName
  | AnyNameFreePredicate FreePredicate
  | AnyNameGeneralType GeneralType

data TypedName

data Attribute a :: Attribute [LeftAttribute] a [RightAttribute]

data LeftAttribute =
    LeftAttributeSingleSubject PrimSimpleAdjective
  | LeftAttributeMultiSubject PrimSimpleAdjectiveMultiSubject
