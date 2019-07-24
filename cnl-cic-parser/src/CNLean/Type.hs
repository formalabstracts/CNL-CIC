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

data Statement =
    HeadStatement HeadStatement
  | ChainStatement ChainStatement

data HeadStatement =
    HeadStatementForAny [AnyName] Statement -- list of anyname parsed by a comma-or-LIT_AND separated list
  | HeadStatementIfThen Statement Statement
  | HeadStatementItsWrong Statement

data ChainStatement =
    AndOrChain AndOrChain
  | AndOrChainIff AndOrChain Statement

data AndOrChain =
    AndChain [PrimaryStatement] HeadPrimary
  | OrChain  [PrimaryStatement] HeadPrimary

data HeadPrimary =
    HeadPrimaryHead HeadStatement
  | HeadPrimaryPrimary PrimaryStatement
  
data PrimaryStatement =
    PrimaryStatementSimple SimpleStatement
  | PrimaryStatementThereIs ThereIsStatement
  | PrimarStatementSymbol Filler SymbolStatement
  | PrimaryStatementConst Filler ConstStatement


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

data TightestProp =
    ParenStatement Statement -- parsed with mandatory parentheses
  | TightestPropIdentifierProp IdentifierProp
  | TightestPropVar Var
  | TightestPropAnnotatedProp AnnotatedProp

newtype Identifier =
    IdentifierAtomicId AtomicId
  | IdentifierHierId HierId

parseIdentifier :: Parser Identifier
parseIdentifier = parseHierId >>= return . IdentifierHierId <||> parseAtomicId >>= return . IdentifierAtomicId

newtype IdentifierProp = IdentifierProp Identifier

parseIdentifierProp = parseIdentifier >>= return . IdentifierProp

newtype AnnotatedProp = AnnotatedProp Prop

parseAnnotatedProp :: Parser AnnotatedProp
parseAnnotatedProp = between parseLParen parseRParen parseProp

data AppProp = AppProp TightestProp AppArgs

data AppArgs = AppArgs (Maybe RecordAssignTerm) [TightestExpr]

data TightestExpr =
    TightestExprTerm TightestTerm
  | TightestExprProp TightestProp
  | TightestExprType TightestType
  | TightestExprProof ProofExpr

data TightestType =
    TightestTypeParen ParenType
  | TightestTypeAnnotated AnnotatedType
  | TightestTypeControlSeq ControlSeqType
  | TightestTypeConst ConstType
  | TightestTypeVar VarType
  | TightestTypeSubtype Subtype
  | TightestTypeInductive InductiveType
  | TightestTypeMutualInductive MutualInductiveType
  | TightestTypeStructure Structure

newtype ParenType = ParenType GeneralType
parseParenType = between parseLParen parseRParen parseGeneralType >>= return . ParenType

newtype AnnotatedType = AnnotatedType GeneralType
parseAnnotatedType = between parseLParen parseRParen $ parseGeneralType <* parseColon <* parseLit "type"

newtype ControlSeqType = ControlSeqType (CSBrace PrimTypeControlSeq)

newtype ConstType = ConstType TypeIdentifier
newtype TypeIdentifier = TypeIdentifier Identifier
parseTypeIdentifier = parseIdentifier >>= return . TypeIdentifier
parseConstType = parseTypeIdentifier >>= return . ConstType

data VarType =
    VarTypeVar Var
  | VarTypeAnnotated Var
  
-- note: possibly need an extra sc in the first branch
parseVarType = (between parseLParen parseRParen $ parseVar <* parseLit "Type") >>= return . VarTypeAnnotated <||>  parseVar >>= return . VarTypeVar

data Subtype = Subtype Term HoldingVar Statement

parseSubtype = between parseLBrace parseRBrace $ do
  t <- parseTerm
  hvar <- parseHoldingVar
  parseLit "//" -- TODO(jesse): confirm this interpretation of LIT_
  s <- parseStatement
  return $ Subtype t hvar s

data InductiveType = InductiveType Identifier Args (Maybe ColonSort) [Maybe AltConstructor]

newtype ColonSort = ColonSort SortExpr
parseColonSort = parseColon *> parseSortExpr >>= return . ColonSort

data AltConstructor = AltConstructor Identifier Args ColonType

parseAltConstructor = parseAlt *> do
  id <- parseIdentifier
  args <- parseArgs
  ct <- parseColonType
  return $ AltConstructor id args ct

data MutualInductiveType = MutualInductiveType [Identifier] Args [(AtomicId, Args, ColonType, [AltConstructor])]   -- comma-separated nonempty list of identifiers, second list parsed with mandatory prefix LIT_WITH
-- MutualInductiveType must be parsed with a LIT_END at the end.


{-
structure : option(LIT_NOTATIONAL) LIT_STRUCTURE 
     option(lit_param) args
     option(LIT_WITH) option(brace_semi(field))
     option(LIT_SATISFYING satisfying_preds {}) {}
-}
data Structure = Structure Args (Maybe [Field]) (Maybe SatisfyingPreds) -- [Field] is parsed by brace_semi

parseStructure = do
  args <- option(parseLit "notational") *> parseLit "structure" *> option (parseLitParam) parseArgs
  mfs <- option(parseLit "with") *> option(brace_semi parseField)
  msps <- option (option (parseLit "satisfying") *> parseSatisfyingPreds)
  return $ Structure args mfs msps

data Field = Field FieldPrefix FieldIdentifier (Maybe FieldSuffix)
newtype SatisfyingPreds =SatisfyingPreds [SatisfyingPred]

newtype FieldPrefix = FieldPrefix [[Text]]
parseFieldPrefix = parseAlt *> (many1' parseLitFieldKey) >>= return . FieldPrefix

data FieldIdentifier = FieldIdentifier VarOrAtomic (Maybe ColonType)

parseFieldIndentifier = do
  va <- parseVarOrAtomic
  mct <- option parseColonType
  return $ FieldIdentifier va mct

data FieldSuffix =
    WithoutNotation
  | FieldSuffixFieldAssign FieldAssign

parseFieldSuffix =
  (do parseLit "without"
      parseLit "notation"
      return WithoutNotation) <||>
  parseFieldAssign >>= return . FieldSuffixFieldAssign

newtype FieldAssign = FieldAssign Expr
parseFieldAssign = parseAssign *> parseExpr >>= return . FieldAssign

parseSatisfyingPreds = brace_semi (parseSatisfyingPred) >>= return . SatisfyingPreds

data SatisfyingPred = SatisfyingPred (Maybe AtomicId) Prop

parseSatisfyingPred = do
  mid <- parseAlt *> option(parseAtomicId <* parseColon)
  p <- parseProp
  return $ SatisfyingPred mid p

data TightestTerm =
    TightestTerm TightestPrefix
  | TightestTermFieldAcc TightestTerm FieldAcc
  | TightestTermApplySub [TightestTerm] -- should be parsed by a paren-enclosed nonempty list of tightest terms, preceded by an ApplySub literal

data TightestPrefix =
    TightestPrefixNumeric Numeric
  | TightestPrefixString String
  | TightestPrefixDecimal Decimal
  | TightestPrefixBlank Blank
  | TightestPrefixVar Var
  | TightestPrefixPrimIdentifierTerm PrimIdentifierTerm
  | TightestPrefixPrimPrefixFunction PrimPrefixFunction
  | TightestPrefixControlSeqTerm ControlSeqTerm
  | TightestPrefixDelimitedTerm DelimitedTerm
  | TightestPrefixAltTerm AltTerm

data AltTerm =
    AltTermCaseTerm Term -- parsed as CASE term OF
  | AltTermMatchTerm MatchSeq -- parsed as MATCH match_seq WITH
  | AltTermLambdaFunction LambdaFunction

parseAltTerm :: Parser AltTerm
parseAltTerm =
  parseLit "case" *> parseTerm <* parseLit "of" >>= return . AltTermCaseTerm <||>
  parseMatchSeq >>= return . AltTermMatchTerm <||>
  parseLambdaFunction >>= return . AltTermLambdaFunction

newtype MatchSeq = MatchSeq [Term]  -- parsed as a comma-separated nonempty list

parseMatchSeq = sepby1 parseTerm parseComma >>= return . MatchSeq

data LambdaFunction = LambdaFunction Identifier Args

parseLambdaFunction = parseLit "function" *> do
  id <- parseIdentifier
  args <- parseArgs
  return $ LambdaFunction id args

data ControlSeqTerm = ControlSeqTerm (CSBrace PrimTermControlSeq)

data DelimitedTerm =
    DelimitedTermParen
  | DelimitedTermAnnotated
  | DelimitedTermMake
  | DelimitedTermList
  | DelimitedTermTuple
  | DelimitedTermSetEnum
  | DelimitedTermSetComprehension

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
