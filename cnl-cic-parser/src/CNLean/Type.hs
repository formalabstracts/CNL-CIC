{-# LANGUAGE DataKinds #-} -- might need to remove this later, i'm not really sure what this does yet
{-# LANGUAGE PolyKinds #-} -- might need to remove this later, i'm not really sure what this does yet
{-
Author(s): Jesse Michael Han (2019)

Parsing types and type ascriptions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Type where

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
import CNLean.Primitive

data TVar =
    TVarVar Var
  | TVarAnnotatedVars AnnotatedVars

data TVarAssignment = TVarAssignment {tvar :: TVar, mct :: Maybe ColonType, term :: Term}
-- to be parsed with an ASSIGN literal between maybeColonType and term

parseTVarAssignment :: Parser TVarAssignment
parseTVarAssignment = do
  tv <- parseTVar
  mct <- option (parseColonType)
  (parseLit ":=")
  t <- parseTerm
  return $ TVarAssignment tv mct t

newtype WhereSuffix  = WhereSuffix [TVarAssignment] -- to be parsed using a brace-enclosed, semicolon-separated list

parseWhereSuffix :: Parser WhereSuffix
parseWhereSuffix = between parseLBrace parseRBracec $ do
  tvas <- (sepby1 parseTVarAssignment (parseLit ";"))
  return $ WhereSuffix tvas

data ColonType = ColonType {generalType :: GeneralType} -- COLON general_type

parseColonType :: parserColonType
parseColonType = (parseLit_aux COLON) *> parseGeneralType

data GeneralType = GeneralType OpenTailType

parseGeneralType :: Parser GeneralType
parseGeneralType = empty

data OpenTailType =
  OpenTailTypeBinOpType BinOpType | OpenTailTypeQuotientType QuotientType | OpenTailTypeCoercionType CoercionType | OpenTailTypePrimStructure PrimStructure

parseOpenTailType :: Parser OpenTailType
parseOpenTailType =
  (parseBinOpType >>= return . OpenTailTypeBinOpType) <||>
  (parseQuotientType >>= return . OpenTailTypeQuotientType) <||>
  (parseCoercionType >>= return . OpenTailTypeCoercionType) <||>
  (parsePrimStructure >>= return . OpenTailTypePrimStructure)

data CoercionType =
    CoercionTypeTerm Term 
  | ImplicitCoercion Term -- note, this is not implemented yet but will be supported in the future.

data PrimStructure = PrimStructure  -- this is a stub for Cabarete-style structure, and is not supported yet

data Term =
    TermDefiniteTerm DefiniteTerm
  | TermAnyName AnyName
  

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
    OpenTailTermLambdaTerm LambdaTerm
  | OpenTailTermLambdaFun  LambdaFun
  | OpenTaiLTermLetTerm LetTerm
  | OpenTaiLTermIfThenElseTerm IfThenElseTerm
  | OpenTailTermTdopTerm TdopTerm

data TdopTerm = DummyConstructor2 -- TODO(jesse): fix me

data LambdaTerm =
    PrimLambdaBinder PrimLambdaBinder GeneralizedArgs OpenTailTerm -- parse binder comma to separate args from tail
  | LambdaTermGeneralizedArg GeneralizedArg OpenTailTerm -- parse mapsto to separate args from tail

data PrimLambdaBinder = DummyConstructor -- TODO(jesse): fix me

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
  | BinderTypePrimPiBinder PrimPiBinder GeneralizedArgs BinderType -- parsed with a binder_comma between generalizedargs and bindertype

data GeneralizedArgs = GeneralizedArgs {optArgs :: OptArgs, generalizedArg :: [GeneralizedArg] }

data VarOrAtomic =
    VarOrAtomicVar Var
  | VarOrAtomicAtomic AtomicId
  
data GeneralizedArg =
    GeneralizedArg TightestTerm
  | GeneralizedArgVarOrAtomic {varOrAtomic1 :: VarOrAtomic, varOrAtomic2 :: VarOrAtomic, optColonType :: (Maybe ColonType)}

data DependentVars = DependentVars {maybeOptArgs :: (Maybe OptArgs), annotatedVars :: [AnnotatedVars]}

-- lexically, OptArgs is a brace-enclosed, semicolon separated lists of variables or atomics, optionally annotated with types
newtype OptArgs = OptArgs [(Token, Maybe ColonType)]

data AnyName =
    AnyNameAnyArgs [AnyArg]
  | AnyNameTypedName TypedName
  | AnyNameFreePredicate FreePredicate
  | AnyNameGeneralType GeneralType

newtype TypedName = TypedName Attribute TypedNameWithoutAttribute

data TypedNameWithoutAttribute = -- note: I removed the extra constructor paren(typed_name_without_attribute) and am instead encoding that into the parser (it will attempt to parse a pair of enclosing parentheses first).
    TypedNamePrimTypedName PrimTypedName
  | TypedNameTVar TVar
  | TypedNamePrimClassifier PrimClassifierTVar
  | TypedNameVar Var GeneralType -- parsed as var "with type" general_type

newtype FreePredicate = FreePredicate Attribute FreePredicateWithoutAttribute

data FreePredicateWithoutAttribute =
    FreePredicateProp Prop HoldingVar -- note, this 
  | FreePredicateVars2 Vars2 BinaryRelationOp TdopTerm

-- TODO(jesse): see if this can be factored out of this mutually recursive mess into a separate file
data Prop =
    PropBinderProp BinderProp
  | PropTdopProp TdopProp

data TdopProp =
    TdopPropHead (Maybe BinderProp) [PropOp]
  | TdopPropTail [(BinderProp,[PropOp])] (Maybe BinderProp)

data PropOp =
    PropOpPrim PrimPropositionalOp
  | PropOpCS (CSBrace PrimPropositionalOpControlSeq)

data BinderProp =
    BinderPropAppProp AppProp
  | BinderPropTdopRelProp TdopRelProp
  | BinderPropPrimBinderProp PrimBinderProp Args BinderProp -- parsed with binder comma between Args and BinderProp

data TdopRelProp = TdopRelProp [TdopTerm] [(BinaryRelationOp, TdopTerm)] -- last list must be empty, tdop_terms is nonempty comma-separated

data AppProp = AppProp TightestProp AppArgs

data AppArgs = AppArgs (Maybe RecordAssignTerm) [TightestExpr]

data TightestExpr =
    TightestExprTerm TightestTerm
  | TightestExprProp TightestProp
  | TightestExprType TightestType
  | TightestExprProof ProofExpr

data TightestTerm =
    TightestTerm TightestPrefix
  | TightestTermFieldAcc TightestTerm FieldAcc
  | TightestTermApplySub [TightestTerm] -- should be parsed by a paren-enclosed nonempty list of tightest terms, preceded by an ApplySub literal

--TODO TightestPrefix  

newtype RecordAssignTerm = RecordAssignTerm [(VarOrAtomic, Maybe ColonType, Expr)] -- parsed as brace-enclosed semicolon-separated list of items, with LIT_ASSIGN between ColonType and Expr

data Expr =
    ExprGeneralType GeneralType
  | ExprTerm Term
  | ExprProp Prop
  | ExprProofExpr ProofExpr
  | ExprSortExpr SortExpr

newtype ProofExpr = ProofExpr Text -- note: removed paren(proof_expr) and moved into parser. this parses a SYMBOL_QED for the Text.

data SortExpr = SortExpr (Maybe Args) LitSort -- Args should be nonempty

data LitSort = LitType | LitProp -- this information should not be discarded, so we need a separate type

parseLitSort' :: Parser LitSort
parseLitSort' = (parseLit "type" >>= return LitType) <||> (parseLit "sort" >>= return LitSort)
  
data Args = Args (Maybe OptArgs) [RequiredArg] -- i think requiredargs are whitespace-separated?

data RequiredArg =
    RequiredArgAnnotated VarOrAtomic (Maybe ColonType) -- note, this must be parsed with enclosing parentheses
  | RequiredArgVarOrAtomic VarOrAtomic

data CSBrace a = CSBrace a [Expr]

data BinaryRelationOp =
    BinaryRelationOpPrimBinaryRelationOp PrimBinaryRelationOp
  | BinaryRelationOpControlSeq (CSBrace PrimBinaryRelationControlSeq)

newtype Vars2 = Vars2 TVar [TVar] -- parse a TVar, then parse a comma-separated list of TVars
  deriving (Show, Eq)

newtype HoldingVar = HoldingVar (Maybe [Var]) -- for the parse to succeed, must be preceded by HOLDING and vars must be in a nonempty comma-separated list

parseHoldingVar :: Parser HoldingVar
parseHoldingVar = option $ parseLit "holding" *> (sepby1 parseVar (parseComma))


data Attribute a :: Attribute [LeftAttribute] a [RightAttribute]

data LeftAttribute =
    LeftAttributeSingleSubject PrimSimpleAdjective
  | LeftAttributeMultiSubject PrimSimpleAdjectiveMultiSubject

data AnyArg =
    AnyArgVar Var
  | AnyArgAnnotatedVars AnnotatedVars

newtype LetAnnotation = LetAnnotation [AnnotatedVars] -- LIT_LET comma_nonempty_list(annotated_vars)
  deriving (Show, Eq)

-- note: we don't need annotatedvar because we can always just parse a singleton list
data AnnotatedVars = AnnotatedVars {varModifier :: VarModifier, vars :: [Var], maybeColonType :: (Maybe ColonType)
                                   } deriving (Show, Eq)

newtype VarModifier = VarModifier (Maybe [Text]) -- this is parsed as an optional LitVarMod
  deriving (Show, Eq)

parseVarModifier :: Parser VarModifier
parseVarModifier = (parseLitVarMod >>= return . VarModifier . Just) <||> return . varModifier $ Nothing

parseAnnotatedVars :: Parser AnnotatedVars
parseAnnotatedVars = between parseLParen parseRParen $
  do varmod <- option parseLitVarMod
     vs     <- (many1' parseVar)
     return $ AnnotatedVars (VarModifier varmod) vs

parseLetAnnotation :: Parser [AnnotatedVars]
parseLetAnnotation = parseLit "let" *> (sepby1 parseAnnotatedVars parseComma)


-- note, we still haven't defined colontypes yet.
