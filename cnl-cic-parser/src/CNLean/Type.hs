{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
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
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic.Basic
import CNLean.Primitive
import CNLean.PhraseList

data Statement =
    HeadStatement HeadStatement
  | ChainStatement ChainStatement
  deriving (Show, Eq)

parseStatement :: Parser Statement
parseStatement = (HeadStatement <$> parseHeadStatement) <||> (ChainStatement <$> parseChainStatement)

data HeadStatement =
    HeadStatementForAny [AnyName] Statement
  | HeadStatementIfThen Statement Statement
  | HeadStatementItsWrong Statement
  deriving (Show, Eq)

parseHeadStatement :: Parser HeadStatement
parseHeadStatement =
  (HeadStatementForAny <$> sep_list1 (parseAnyName) <*> parseStatement) <||>
  (HeadStatementIfThen <$> (parseLit "if" *> parseStatement) <*> (parseLit "then" *> parseStatement)) <||>
  (HeadStatementItsWrong <$> (parseLitItsWrong *> parseStatement))

data ChainStatement =
    AndOrChain AndOrChain
  | AndOrChainIff AndOrChain Statement
  deriving (Show, Eq)

parseChainStatement :: Parser ChainStatement
parseChainStatement =
  AndOrChain <$> parseAndOrChain <||>
  AndOrChainIff <$> (parseAndOrChain <* parseLit "iff") <*> parseStatement

data AndOrChain =
    AndChain [PrimaryStatement] HeadPrimary
  | OrChain  [PrimaryStatement] HeadPrimary
  | AndOrChainPrimaryStatement PrimaryStatement
  deriving (Show, Eq)

parseAndOrChain :: Parser AndOrChain
parseAndOrChain =
  AndChain <$> (sepby1 parsePrimaryStatement $ parseLit "and") <*> (parseLit "and" *> parseHeadPrimary) <||>
  OrChain <$> (sepby1 parsePrimaryStatement $ parseLit "or") <*> (parseLit "or" *> parseHeadPrimary) <||>
  AndOrChainPrimaryStatement <$> parsePrimaryStatement
  
data HeadPrimary =
    HeadPrimaryHead HeadStatement
  | HeadPrimaryPrimary PrimaryStatement
  deriving (Show, Eq)

parseHeadPrimary :: Parser HeadPrimary
parseHeadPrimary =
  HeadPrimaryHead <$> parseHeadStatement <||>
  HeadPrimaryPrimary <$> parsePrimaryStatement
  
data PrimaryStatement =
    PrimaryStatementSimple SimpleStatement
  | PrimaryStatementThereIs ThereIsStatement
  | PrimaryStatementSymbol Filler SymbolStatement
  | PrimaryStatementConst Filler ConstStatement
  deriving (Show, Eq)

parsePrimaryStatement :: Parser PrimaryStatement
parsePrimaryStatement =
  PrimaryStatementSimple <$> parseSimpleStatement <||>
  PrimaryStatementThereIs <$> parseThereIsStatement <||>
  PrimaryStatementSymbol <$> parseFiller <*> parseSymbolStatement <||>
  PrimaryStatementConst <$> parseFiller <*> parseConstStatement
  
data SimpleStatement = SimpleStatement Terms [DoesPred]
  deriving (Show, Eq)
-- parse [Term] using parseTerms and parse [DoesPred] using sepby1 parseDoesPred (parseLit "and")

parseSimpleStatement :: Parser SimpleStatement
parseSimpleStatement = SimpleStatement <$> parseTerms <*> (sepby1 parseDoesPred $ parseLit "and")
  
data ThereIsStatement =
    ThereIs [NamedTerm] -- parsed with sep_list (option(lit_a) named_term)
  | ThereIsNo NamedTerm
  deriving (Show, Eq)

parseThereIsStatement =
  ThereIsNo <$> (parseLit "there" *> parseLitExist *> parseLit "no" *> parseNamedTerm) <||>
  ThereIs <$>  ((parseLit "there") *> (parseLitExist) *> (sepby1 parseNamedTerm (option parseLitA)))

newtype ConstStatement = ConstStatement [Text]
  deriving (Show, Eq)

parseConstStatement :: Parser ConstStatement
parseConstStatement = ConstStatement <$> (option(parseLit "the") *> option(parseLit "thesis") *>
  ( option(parseLit "the") *> (rp $ parseLit "contrary" ) <||>
    parseLitA *> (rp $ parseLit "contradiction")))

data SymbolStatement =
    SymbolStatementForall FreePredicate SymbolStatement
  | SymbolStatementExists FreePredicate SymbolStatement
  | SymbolStatementPrimRelation PrimRelation
  | SymbolStatementNot SymbolStatement
  | SymbolStatementParen Statement
  | SymbolStatementProp Prop
  deriving (Show, Eq)

parseSymbolStatement :: Parser SymbolStatement
parseSymbolStatement =
  SymbolStatementForall <$> (parseLit "forall" *> parseFreePredicate <* parseLitBinderComma) <*> parseSymbolStatement <||>
  SymbolStatementExists <$> (parseLit "exists" *> parseFreePredicate <* parseLitBinderComma) <*> parseSymbolStatement <||>
  SymbolStatementPrimRelation <$> parsePrimRelation <||>
  SymbolStatementNot <$> (parseLit "not" *> parseSymbolStatement) <||>
  SymbolStatementParen <$> (between parseLParen parseRParen parseStatement) <||>
  SymbolStatementProp <$> parseProp
  
data NamedTerm =
    NamedTermTypedName TypedName
  | NamedTermPredicate FreePredicate
  deriving (Show, Eq)

parseNamedTerm :: Parser NamedTerm
parseNamedTerm =
  NamedTermTypedName <$> parseTypedName <||>
  NamedTermPredicate <$> parseFreePredicate

newtype PhraseListFiller = PhraseListFiller (Maybe [Text])
  deriving (Show, Eq)

parsePhraseListFiller' :: Parser PhraseListFiller
parsePhraseListFiller' = PhraseListFiller <$> parsePhraseListFiller

newtype Filler = Filler (Maybe PhraseListFiller)
  deriving (Show, Eq)

parseFiller :: Parser Filler
parseFiller = Filler <$> option parsePhraseListFiller'

data TVar =
    TVarVar Var
  | TVarAnnotatedVar AnnotatedVar
  deriving (Show, Eq)

parseTVar :: Parser TVar
parseTVar =
  TVarVar <$> parseVar <||>
  TVarAnnotatedVar <$> parseAnnotatedVar
  
data TVarAssignment = TVarAssignment {tvar :: TVar, mct :: Maybe ColonType, term :: Term}
-- to be parsed with an ASSIGN literal between maybeColonType and term
  deriving (Show, Eq)

parseTVarAssignment :: Parser TVarAssignment
parseTVarAssignment =
  TVarAssignment <$> parseTVar <*> (option parseColonType) <* parseAssign <*> parseTerm

newtype WhereSuffix  = WhereSuffix [TVarAssignment] -- to be parsed using a brace-enclosed, semicolon-separated list
  deriving (Show, Eq)

parseWhereSuffix :: Parser WhereSuffix
parseWhereSuffix =
  WhereSuffix <$> brace_semi parseTVarAssignment

data ColonType = ColonType GeneralType
  deriving (Show, Eq)

parseColonType :: Parser ColonType
parseColonType = ColonType <$> ((parseColon) *> parseGeneralType)

data GeneralType = GeneralType OpenTailType
  deriving (Show, Eq)

parseGeneralType :: Parser GeneralType
parseGeneralType = GeneralType <$> parseOpenTailType

data OpenTailType =
    OpenTailTypeBinOpType BinOpType
  | OpenTailTypeQuotientType QuotientType
  | OpenTailTypeCoercionType CoercionType
  deriving (Show, Eq)

parseOpenTailType :: Parser OpenTailType
parseOpenTailType =
  OpenTailTypeBinOpType <$> parseBinOpType <||>
  OpenTailTypeQuotientType <$> parseQuotientType <||>
  OpenTailTypeCoercionType <$> parseCoercionType
  
data CoercionType =
    CoercionTypeTerm Term 
  | ImplicitCoercion Term -- note, this is not implemented yet but will be supported in the future.
  deriving (Show, Eq)

parseCoercionType :: Parser CoercionType
parseCoercionType =
  CoercionTypeTerm <$> parseTerm <||> -- TODO(jesse): check that this implicit coercion is implemented correctly
  ImplicitCoercion <$> (parseCoercion *> parseTerm)
  
data PrimStructure = PrimStructure  -- this is a stub for Cabarete-style structure, and is not supported yet
  deriving (Show, Eq)

parsePrimStructure :: Parser PrimStructure
parsePrimStructure = empty -- not implemented yet, so for now parsing a prim_structure always fails

newtype Terms = Terms [Term]
  deriving (Show, Eq)

parseTerms :: Parser Terms
parseTerms = Terms <$> sep_list (parseTerm)

data Term =
    TermDefiniteTerm DefiniteTerm
  | TermAnyName AnyName
  deriving (Show, Eq)

parseTerm =
  TermDefiniteTerm <$> parseDefiniteTerm <||>
  TermAnyName <$> parseAnyName

-- parseTerms :: Parser [Term]
-- parseTerms = sep_list parseTerm

data QuotientType = QuotientType {domain :: GeneralType, eqv :: Term}
  deriving (Show, Eq)

parseQuotientType :: Parser QuotientType
parseQuotientType =
  QuotientType <$> (parseLit "quotient" *> (option $ parseLit "of") *> parseGeneralType <* parseLit "by") <*> parseTerm
  
data DefiniteTerm =
    DefiniteTermSymbolicTerm SymbolicTerm
  | DefiniteTermNoun PrimDefiniteNoun
  | DefiniteTermParenNoun PrimDefiniteNoun
  deriving (Show, Eq)

parseDefiniteTerm :: Parser DefiniteTerm
parseDefiniteTerm =
  DefiniteTermNoun <$> ((option $ parseLit "the") *> parsePrimDefiniteNoun) <||>
  DefiniteTermSymbolicTerm <$> parseSymbolicTerm <||>
  DefiniteTermParenNoun <$> between parseLParen parseRParen ((option $ parseLit "the") *> parsePrimDefiniteNoun)
  
data SymbolicTerm = SymbolicTerm OpenTailTerm (Maybe WhereSuffix)
  deriving (Show, Eq)

parseSymbolicTerm :: Parser SymbolicTerm
parseSymbolicTerm =
  SymbolicTerm <$> parseOpenTailTerm <*> (option parseWhereSuffix)
  
data OpenTailTerm =
    OpenTailTermLambdaTerm LambdaTerm
  | OpenTailTermLambdaFun  LambdaFun
  | OpenTaiLTermLetTerm LetTerm
  | OpenTaiLTermIfThenElseTerm IfThenElseTerm
  | OpenTailTermTdopTerm TdopTerm
  deriving (Show, Eq)

parseOpenTailTerm :: Parser OpenTailTerm
parseOpenTailTerm =
  OpenTailTermLambdaTerm <$> parseLambdaTerm <||>
  OpenTailTermLambdaFun <$> parseLambdaFun <||>
  OpenTaiLTermLetTerm <$> parseLetTerm <||>
  OpenTaiLTermIfThenElseTerm <$> parseIfThenElseTerm <||>
  OpenTailTermTdopTerm <$> parseTdopTerm

data TdopTerm =
    TdopTermOps (Maybe AppTerm) TermOps [(AppTerm, TermOps)] (Maybe AppTerm)
  | TdopTermApp AppTerm
  deriving (Show, Eq)

parseTdopTerm :: Parser TdopTerm
parseTdopTerm =
  TdopTermOps <$> (option parseAppTerm) <*> parseTermOps <*>
                    (many' $ (,) <$> parseAppTerm <*> parseTermOps)
                    <*> (option parseAppTerm) <||>
  TdopTermApp <$> parseAppTerm

data AppTerm = AppTerm TightestTerm AppArgs
  deriving (Show, Eq)

parseAppTerm :: Parser AppTerm
parseAppTerm = AppTerm <$> parseTightestTerm <*> parseAppArgs

newtype TermOps = TermOps [TermOp]
  deriving (Show, Eq)

parseTermOps :: Parser TermOps
parseTermOps = TermOps <$> (many1' parseTermOp)

data TermOp =
    TermOpPrimTermOp PrimTermOp
  | TermOpCS (CSBrace PrimTermOpControlSeq)
  deriving (Show, Eq)

parseTermOp :: Parser TermOp
parseTermOp =
  TermOpPrimTermOp <$> parsePrimTermOp <||>
  TermOpCS <$> parseCSBrace parsePrimTermOpControlSeq
  
data IfThenElseTerm = IfThenElseTerm Prop Term OpenTailTerm
  deriving (Show, Eq)

parseIfThenElseTerm =
  (IfThenElseTerm)
    <$> (parseLit "if" *> parseProp) <*>
        (parseLit "then" *> parseTerm) <*>
        (parseLit "else" *> parseOpenTailTerm)

data LambdaFun = LambdaFun Identifier GeneralizedArgs (Maybe ColonType) OpenTailTerm
  deriving (Show, Eq)

parseLambdaFun =
  (LambdaFun) <$>
    ((parseLit "fun") *> parseIdentifier) <*>
    (parseGeneralizedArgs) <*>
    (option parseColonType) <*>
    (parseAssign *> parseOpenTailTerm)


data LetTerm = LitTerm Term Term OpenTailTerm
  deriving (Show, Eq)

parseLetTerm = (LitTerm) <$>
  (parseLit "let" *> parseTerm) <*>
  (parseAssign *> parseTerm) <*>
  (parseLit "in" *> parseOpenTailTerm)

data LambdaTerm =
    LambdaTermPrimLambdaBinder PrimLambdaBinder GeneralizedArgs OpenTailTerm -- parse binder comma to separate args from tail
  | LambdaTermGeneralizedArg GeneralizedArg OpenTailTerm -- parse mapsto to separate args from tail
  deriving (Show, Eq)

parseLambdaTerm :: Parser LambdaTerm
parseLambdaTerm =
  LambdaTermPrimLambdaBinder <$> parsePrimLambdaBinder <*> parseGeneralizedArgs <*> parseOpenTailTerm <||>
  LambdaTermGeneralizedArg <$> parseGeneralizedArg <* parseMapsTo <*> parseOpenTailTerm
  
data BinOpType = BinOpType [(TypeOperand,TypeOp,BinderType)]
  deriving (Show, Eq)

parseBinOpType :: Parser BinOpType
parseBinOpType = BinOpType <$> (many' $ (,,) <$> parseTypeOperand <*> parseTypeOp <*> parseBinderType)
  
data TypeOp =
    TypeOpPrimTypeOp PrimTypeOp
  | TypeOpCSBrace (CSBrace PrimTypeOpControlSeq)
  deriving (Show, Eq)

parseTypeOp :: Parser TypeOp
parseTypeOp =
  TypeOpPrimTypeOp <$> parsePrimTypeOp <||>
  TypeOpCSBrace <$> parseCSBrace parsePrimTypeOpControlSeq

data TypeOperand =
    TypeOperandBinderType BinderType
  | TypeOperandDependentVars DependentVars
  deriving (Show, Eq)

parseTypeOperand :: Parser TypeOperand
parseTypeOperand =
  TypeOperandBinderType <$> parseBinderType <||>
  TypeOperandDependentVars <$> parseDependentVars
  
data BinderType =
    BinderTypeAppType AppType
  | BinderTypePrimPiBinder PrimPiBinder GeneralizedArgs BinderType -- parsed with a binder_comma between generalizedargs and bindertype
  deriving (Show, Eq)

parseBinderType :: Parser BinderType
parseBinderType =
  BinderTypeAppType <$> parseAppType <||>
  BinderTypePrimPiBinder <$> parsePrimPiBinder <*> (parseGeneralizedArgs <* parseLitBinderComma) <*> parseBinderType

data AppType = AppType TightestType AppArgs
  deriving (Show, Eq)

parseAppType :: Parser AppType
parseAppType =
  AppType <$> parseTightestType <*> parseAppArgs
  
data GeneralizedArgs = GeneralizedArgs {optArgs :: OptArgs, generalizedArg :: [GeneralizedArg] }
  deriving (Show, Eq)

parseGeneralizedArgs :: Parser GeneralizedArgs
parseGeneralizedArgs = GeneralizedArgs <$> parseOptArgs <*> many' parseGeneralizedArg
  
data VarOrAtomic =
    VarOrAtomicVar Var
  | VarOrAtomicAtomic AtomicId
  deriving (Show, Eq)

parseVarOrAtomic :: Parser VarOrAtomic
parseVarOrAtomic = (VarOrAtomicVar <$> parseVar) <||> (VarOrAtomicAtomic <$> parseAtomicId)
  
data GeneralizedArg =
    GeneralizedArg TightestTerm
  | GeneralizedArgVarOrAtomic {varOrAtomic :: VarOrAtomic, varOrAtomics :: [VarOrAtomic], optColonType :: (Maybe ColonType)}
  deriving (Show, Eq)

parseGeneralizedArg :: Parser GeneralizedArg
parseGeneralizedArg =
  GeneralizedArg <$> parseTightestTerm <||>
  (paren $ GeneralizedArgVarOrAtomic <$> parseVarOrAtomic <*> (many1' parseVarOrAtomic) <*> option parseColonType)
  
data DependentVars = DependentVars {maybeOptArgs :: (Maybe OptArgs), annotatedVars :: [AnnotatedVars]}
  deriving (Show, Eq)

parseDependentVars :: Parser DependentVars
parseDependentVars =
  DependentVars <$> option (parseLit "at" *> parseOptArgs) <*> many1' parseAnnotatedVars
  
-- lexically, OptArgs is a brace-enclosed, semicolon separated lists of variables or atomics, optionally annotated with types
newtype OptArgs = OptArgs [(VarOrAtomic, Maybe ColonType)]
  deriving (Show, Eq)

parseOptArgs :: Parser OptArgs
parseOptArgs = (brace_semi $ do
  va <- parseVarOrAtomic
  mct <- option parseColonType
  return (va, mct)) >>= return . OptArgs

data AnyName =
    AnyNameAnyArgs [AnyArg]
  | AnyNameTypedName TypedName
  | AnyNameFreePredicate FreePredicate
  | AnyNameGeneralType GeneralType
  deriving (Show, Eq)

parseAnyName :: Parser AnyName
parseAnyName =
  AnyNameAnyArgs <$> (parseLitAny *> sepby1 parseAnyArg parseComma) <||>
  AnyNameTypedName <$> (parseLitAny *> parseTypedName) <||>
  AnyNameFreePredicate <$> (parseLitAny *> parseFreePredicate) <||>
  AnyNameGeneralType <$> (parseLitAny *> parseGeneralType)
  
newtype TypedName = TypedName (Attribute TypedNameWithoutAttribute)
  deriving (Show, Eq)

parseTypedName :: Parser TypedName
parseTypedName = TypedName <$> parseAttribute parseTypedNameWithoutAttribute
  
data TypedNameWithoutAttribute = 
    TypedNamePrimTypedName PrimTypedName
  | TypedNameTVar TVar
  | TypedNamePrimClassifier PrimClassifier TVar
  | TypedNameVar Var GeneralType -- parsed as var "with type" general_type
  | TypedNameParen TypedNameWithoutAttribute
  deriving (Show, Eq)

parseTypedNameWithoutAttribute :: Parser TypedNameWithoutAttribute
parseTypedNameWithoutAttribute =
  TypedNamePrimTypedName <$> parsePrimTypedName <||>
  TypedNameTVar <$> parseTVar <||>
  TypedNamePrimClassifier <$> parsePrimClassifier <*> parseTVar <||>
  TypedNameVar <$> (parseVar <* parseLitWith <* parseLit "type") <*> parseGeneralType <||>
  TypedNameParen <$> paren parseTypedNameWithoutAttribute
  
newtype FreePredicate = FreePredicate (Attribute FreePredicateWithoutAttribute)
  deriving (Show, Eq)

parseFreePredicate :: Parser FreePredicate
parseFreePredicate = FreePredicate <$> parseAttribute parseFreePredicateWithoutAttribute
  
data FreePredicateWithoutAttribute =
    FreePredicateProp Prop HoldingVar -- note, this 
  | FreePredicateVars2 Vars2 BinaryRelationOp TdopTerm
  deriving (Show, Eq)

parseFreePredicateWithoutAttribute :: Parser FreePredicateWithoutAttribute
parseFreePredicateWithoutAttribute =
  opt_paren (FreePredicateProp <$> parseProp <*> parseHoldingVar) <||>
  FreePredicateVars2 <$> parseVars2 <*> parseBinaryRelationOp <*> parseTdopTerm
  
-- TODO(jesse): see if this can be factored out of this mutually recursive mess into a separate file
data Prop =
    PropBinderProp BinderProp
  | PropTdopProp TdopProp
  deriving (Show, Eq)

parseProp :: Parser Prop
parseProp =
  PropBinderProp <$> parseBinderProp <||>
  PropTdopProp <$> parseTdopProp
  
data TdopProp =
    TdopPropHead (Maybe BinderProp) PropOps
  | TdopPropTail [(BinderProp,PropOps)] (Maybe BinderProp)
  deriving (Show, Eq)

parseTdopProp :: Parser TdopProp
parseTdopProp =
  TdopPropHead <$> (option parseBinderProp) <*> parsePropOps <||>
  TdopPropTail <$> (many' $ (,) <$> parseBinderProp <*> parsePropOps) <*> option parseBinderProp
  
data PropOp =
    PropOpPrim PrimPropositionalOp
  | PropOpCS (CSBrace PrimPropositionalOpControlSeq)
  deriving (Show, Eq)

parsePropOp :: Parser PropOp
parsePropOp =
  PropOpPrim <$> parsePrimPropositionalOp <||>
  PropOpCS <$> parseCSBrace parsePrimPropositionalOpControlSeq

newtype PropOps = PropOps [PropOp]
  deriving (Show, Eq)

parsePropOps :: Parser PropOps
parsePropOps = PropOps <$> (many1' parsePropOp)
  
  
data BinderProp =
    BinderPropAppProp AppProp
  | BinderPropTdopRelProp TdopRelProp
  | BinderPropPrimBinderProp PrimBinderProp Args BinderProp -- parsed with binder comma between Args and BinderProp
  deriving (Show, Eq)

parseBinderProp :: Parser BinderProp
parseBinderProp =
  BinderPropAppProp <$> parseAppProp <||>
  BinderPropTdopRelProp <$> parseTdopRelProp <||>
  BinderPropPrimBinderProp <$> parsePrimBinderProp <*> parseArgs <* parseLitBinderComma <*> parseBinderProp
  
data TdopRelProp = TdopRelProp [TdopTerm] [(BinaryRelationOp, TdopTerm)] -- last list must be empty, tdop_terms is nonempty comma-separated
  deriving (Show, Eq)

parseTdopRelProp :: Parser TdopRelProp
parseTdopRelProp = TdopRelProp <$> comma_nonempty_list parseTdopTerm <*> (many1' $ (,) <$> parseBinaryRelationOp <*> parseTdopTerm)
  
data TightestProp =
    ParenStatement Statement -- parsed with mandatory parentheses
  | TightestPropIdentifierProp IdentifierProp
  | TightestPropVar Var
  | TightestPropAnnotatedProp AnnotatedProp
  deriving (Show, Eq)

parseTightestProp :: Parser TightestProp
parseTightestProp =
  paren (ParenStatement <$> parseStatement) <||>
  TightestPropIdentifierProp <$> parseIdentifierProp <||>
  TightestPropVar <$> parseVar <||>
  TightestPropAnnotatedProp <$> parseAnnotatedProp
  
data Identifier =
    IdentifierAtomicId AtomicId
  | IdentifierHierId HierId
  deriving (Show, Eq)

parseIdentifier :: Parser Identifier
parseIdentifier =
  (IdentifierHierId <$> parseHierId) <||>
  (IdentifierAtomicId <$> parseAtomicId)

newtype IdentifierProp = IdentifierProp Identifier
  deriving (Show, Eq)

parseIdentifierProp = parseIdentifier >>= return . IdentifierProp

newtype AnnotatedProp = AnnotatedProp Prop
  deriving (Show, Eq)

parseAnnotatedProp :: Parser AnnotatedProp
parseAnnotatedProp =
  paren $ AnnotatedProp <$> parseProp <* parseColon <* parseLit "prop"
-- parseAnnotatedProp :: Parser AnnotatedProp
-- parseAnnotatedProp = between parseLParen parseRParen parseProp

data AppProp = AppProp TightestProp AppArgs
  deriving (Show, Eq)

parseAppProp :: Parser AppProp
parseAppProp =
  AppProp <$> parseTightestProp <*> parseAppArgs

data AppArgs = AppArgs (Maybe RecordAssignTerm) [TightestExpr]
  deriving (Show, Eq)

parseAppArgs :: Parser AppArgs
parseAppArgs =
  AppArgs <$> (option $ parseLit "at" *> parseRecordAssignTerm) <*> (many' parseTightestExpr)
  
data TightestExpr =
    TightestExprTerm TightestTerm
  | TightestExprProp TightestProp
  | TightestExprType TightestType
  | TightestExprProof ProofExpr
  deriving (Show, Eq)

parseTightestExpr :: Parser TightestExpr
parseTightestExpr =
  TightestExprTerm <$> parseTightestTerm <||>
  TightestExprProp <$> parseTightestProp <||>
  TightestExprType <$> parseTightestType <||>
  TightestExprProof <$> parseProofExpr
  
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
  | TightestTypePrimStructure
  deriving (Show, Eq)

parseTightestType :: Parser TightestType
parseTightestType =
  TightestTypeParen <$> parseParenType <||>
  TightestTypeAnnotated <$> parseAnnotatedType <||>
  TightestTypeControlSeq <$> parseControlSeqType <||>
  TightestTypeConst <$> parseConstType <||>
  TightestTypeVar <$> parseVarType <||>
  TightestTypeSubtype <$> parseSubtype <||>
  TightestTypeInductive <$> parseInductiveType <||>
  TightestTypeMutualInductive <$> parseMutualInductiveType <||>
  TightestTypeStructure <$> parseStructure <||>
  TightestTypePrimStructure <$> parsePrimStructure -- TODO(jesse): fix this
  
newtype ParenType = ParenType GeneralType
  deriving (Show, Eq)

parseParenType = between parseLParen parseRParen parseGeneralType >>= return . ParenType

newtype AnnotatedType = AnnotatedType GeneralType
  deriving (Show, Eq)

parseAnnotatedType :: Parser AnnotatedType
parseAnnotatedType = AnnotatedType <$> (between parseLParen parseRParen $ parseGeneralType <* parseColon <* parseLit "type")

newtype ControlSeqType = ControlSeqType (CSBrace PrimTypeControlSeq)
  deriving (Show, Eq)

parseControlSeqType :: Parser ControlSeqType
parseControlSeqType =
  ControlSeqType <$> parseCSBrace parsePrimTypeControlSeq
  
newtype ConstType = ConstType TypeIdentifier
  deriving (Show, Eq)

parseConstType = parseTypeIdentifier >>= return . ConstType

newtype TypeIdentifier = TypeIdentifier Identifier
  deriving (Show, Eq)

parseTypeIdentifier = parseIdentifier >>= return . TypeIdentifier

data VarType =
    VarTypeVar Var
  | VarTypeAnnotated Var
  deriving (Show, Eq)
  
parseVarType = VarTypeAnnotated <$> (paren $ parseVar <* parseColon <* parseLit "Type") <||> VarTypeVar <$> parseVar

-- test parseVarType "(x : Type)"
-- test parseVarType "x"

data Subtype = Subtype Term HoldingVar Statement
  deriving (Show, Eq)
  
parseSubtype = between parseLBrace parseRBrace $ do
  t <- parseTerm
  hvar <- parseHoldingVar
  parseLit "//"
  s <- parseStatement
  return $ Subtype t hvar s

data InductiveType = InductiveType Identifier Args (Maybe ColonSort) [OptAltConstructor]
  deriving (Show, Eq)

parseInductiveType :: Parser InductiveType
parseInductiveType = (InductiveType <$> (parseLit "inductive" *> parseIdentifier) <*> parseArgs <*> (option parseColonSort) <*> many' parseOptAltConstructor <* parseLit "end")
  
newtype ColonSort = ColonSort SortExpr
  deriving (Show, Eq)

parseColonSort = parseColon *> parseSortExpr >>= return . ColonSort

data OptAltConstructor = OptAltConstructor Identifier Args (Maybe ColonType)
  deriving (Show, Eq)

parseOptAltConstructor :: Parser OptAltConstructor
parseOptAltConstructor = OptAltConstructor <$> (parseAlt *> parseIdentifier) <*> parseArgs <*> option parseColonType

data AltConstructor = AltConstructor Identifier Args ColonType
  deriving (Show, Eq)

parseAltConstructor = AltConstructor <$> (parseAlt *> parseIdentifier) <*> parseArgs <*> parseColonType

data MutualInductiveType = MutualInductiveType [Identifier] Args [(AtomicId, Args, ColonType, [AltConstructor])]   -- comma-separated nonempty list of identifiers, second list parsed with mandatory prefix LIT_WITH
-- MutualInductiveType must be parsed with a LIT_END at the end.
  deriving (Show, Eq)

parseMutualInductiveType :: Parser MutualInductiveType
parseMutualInductiveType = (MutualInductiveType <$> (parseLit "inductive" *> comma_nonempty_list parseIdentifier) <*> parseArgs <*> many' (parseLit "with" *> ((,,,) <$> parseAtomicId <*> parseArgs <*> parseColonType <*> many parseAltConstructor))) <* parseLit "end"

{-
structure : option(LIT_NOTATIONAL) LIT_STRUCTURE 
     option(lit_param) args
     option(LIT_WITH) option(brace_semi(field))
     option(LIT_SATISFYING satisfying_preds {}) {}
-}
data Structure = Structure Args (Maybe [Field]) (Maybe SatisfyingPreds) -- [Field] is parsed by brace_semi
  deriving (Show, Eq)

parseStructure :: Parser Structure
parseStructure = Structure <$>
  (option (parseLit "notational") *> parseLit "structure" *> option (parseLitParam) *> parseArgs) <*>
  ((option $ parseLit "with") *> option (brace_semi parseField)) <*>
  (option $ parseLit "satisfying" *> parseSatisfyingPreds)
-- parseStructure = do
--   args <- option(parseLit "notational") *> parseLit "structure" *> option (parseLitParam) parseArgs
--   mfs <- option(parseLit "with") *> option(brace_semi parseField)
--   msps <- option (option (parseLit "satisfying") *> parseSatisfyingPreds)
--   return $ Structure args mfs msps

data Field = Field FieldPrefix FieldIdentifier (Maybe FieldSuffix)
  deriving (Show, Eq)

parseField :: Parser Field
parseField = Field <$> parseFieldPrefix <*> parseFieldIdentifier <*> option parseFieldSuffix

newtype SatisfyingPreds = SatisfyingPreds [SatisfyingPred]
  deriving (Show, Eq)

parseSatisfyingPreds :: Parser SatisfyingPreds
parseSatisfyingPreds = SatisfyingPreds <$> brace_semi parseSatisfyingPred
  
newtype FieldPrefix = FieldPrefix [[Text]]
  deriving (Show, Eq)

parseFieldPrefix = parseAlt *> (many1' parseLitFieldKey) >>= return . FieldPrefix


data FieldIdentifier = FieldIdentifier VarOrAtomic (Maybe ColonType)
  deriving (Show, Eq)

parseFieldIdentifier = FieldIdentifier <$> parseVarOrAtomic <*> option parseColonType

data FieldSuffix =
    WithoutNotation
  | FieldSuffixFieldAssign FieldAssign
  deriving (Show, Eq)

parseFieldSuffix =
  parseLit "without" *> parseLit "notation" *> return WithoutNotation <||>
  FieldSuffixFieldAssign <$> parseFieldAssign

newtype FieldAssign = FieldAssign Expr
  deriving (Show, Eq)

parseFieldAssign = FieldAssign <$> (parseAssign *> parseExpr)

data SatisfyingPred = SatisfyingPred (Maybe AtomicId) Prop
  deriving (Show, Eq)

parseSatisfyingPred = do
  mid <- parseAlt *> option(parseAtomicId <* parseColon)
  p <- parseProp
  return $ SatisfyingPred mid p

data TightestTerm =
    TightestTermPrefix TightestPrefix
  | TightestTermFieldAcc TightestTerm FieldAcc
  | TightestTermApplySub TightestTerm TightestTerms
  deriving (Show, Eq)

parseTightestTerm :: Parser TightestTerm
parseTightestTerm = do
  pfx <- (TightestTermPrefix <$> parseTightestPrefix)
  rest pfx
  where
    rest pfx = (do fa <- parseFieldAcc
                   rest (TightestTermFieldAcc pfx fa)) <||>
               (do tts <- (parseApplySub *> parseTightestTerms)
                   rest (TightestTermApplySub pfx tts)) <||>
               return pfx

newtype TightestTerms = TightestTerms [TightestTerm]
  deriving (Show, Eq)

parseTightestTerms :: Parser TightestTerms
parseTightestTerms = paren $ (TightestTerms <$> many1' parseTightestTerm)
  
data TightestPrefix =
    TightestPrefixNumeric Numeric
  | TightestPrefixString TkString
  | TightestPrefixDecimal Decimal
  | TightestPrefixBlank Blank
  | TightestPrefixVar Var
  | TightestPrefixPrimIdentifierTerm PrimIdentifierTerm
  | TightestPrefixPrimPrefixFunction PrimPrefixFunction
  | TightestPrefixControlSeqTerm ControlSeqTerm
  | TightestPrefixDelimitedTerm DelimitedTerm
  | TightestPrefixAltTerm AltTerm
  deriving (Show, Eq)

parseTightestPrefix :: Parser TightestPrefix
parseTightestPrefix =
  TightestPrefixNumeric <$> parseNumeric <||>
  TightestPrefixString <$> parseTkString <||>
  TightestPrefixDecimal <$> parseDecimal <||>
  TightestPrefixBlank <$> parseBlank <||>
  TightestPrefixAltTerm <$> parseAltTerm <||>
  TightestPrefixVar <$> parseVar <||>
  TightestPrefixPrimIdentifierTerm <$> parsePrimIdentifierTerm <||>
  TightestPrefixPrimPrefixFunction <$> parsePrimPrefixFunction <||>
  TightestPrefixControlSeqTerm <$> parseControlSeqTerm <||>
  TightestPrefixDelimitedTerm <$> parseDelimitedTerm
  
data AltTerm =
    AltTermCaseTerm Term -- parsed as CASE term OF
  | AltTermMatchTerm MatchSeq -- parsed as MATCH match_seq WITH
  | AltTermLambdaFunction LambdaFunction
  deriving (Show, Eq)

parseAltTerm :: Parser AltTerm
parseAltTerm =
  AltTermCaseTerm <$> (parseLit "case" *> parseTerm <* parseLit "of") <||>
  AltTermMatchTerm <$> (parseLit "match" *> parseMatchSeq <* parseLit "with") <||>
  AltTermLambdaFunction <$> parseLambdaFunction

newtype MatchSeq = MatchSeq [Term]  -- parsed as a nonempty list
  deriving (Show, Eq)

parseMatchSeq = sepby1 parseTerm parseComma >>= return . MatchSeq

data LambdaFunction = LambdaFunction Identifier Args
  deriving (Show, Eq)

parseLambdaFunction = parseLit "function" *> do
  id <- parseIdentifier
  args <- parseArgs
  return $ LambdaFunction id args

data ControlSeqTerm = ControlSeqTerm (CSBrace PrimTermControlSeq)
  deriving (Show, Eq)

parseControlSeqTerm :: Parser ControlSeqTerm
parseControlSeqTerm = ControlSeqTerm <$> parseCSBrace parsePrimTermControlSeq
  
data DelimitedTerm =
    DelimitedTermParen Term
  | DelimitedTermAnnotated AnnotatedTerm
  | DelimitedTermMake MakeTerm
  | DelimitedTermList ListTerm
  | DelimitedTermTuple TupleTerm
  | DelimitedTermSetEnum SetEnumTerm
  | DelimitedTermSetComprehension SetComprehensionTerm
  deriving (Show, Eq)

parseDelimitedTerm :: Parser DelimitedTerm
parseDelimitedTerm =
  DelimitedTermParen <$> paren parseTerm <||>
  DelimitedTermAnnotated <$> parseAnnotatedTerm <||>
  DelimitedTermMake <$> parseMakeTerm <||>
  DelimitedTermList <$> parseListTerm <||>
  DelimitedTermTuple <$> parseTupleTerm <||>
  DelimitedTermSetEnum <$> parseSetEnumTerm <||>
  DelimitedTermSetComprehension <$> parseSetComprehensionTerm

data AnnotatedTerm = AnnotatedTerm Term ColonType
  deriving (Show, Eq)

parseAnnotatedTerm :: Parser AnnotatedTerm
parseAnnotatedTerm = paren $ AnnotatedTerm <$> parseTerm <*> parseColonType

data MakeTerm = MakeTerm [(VarOrAtomic, Maybe Term)]
  deriving (Show, Eq)

parseMakeTerm :: Parser MakeTerm
parseMakeTerm = MakeTerm <$> (brace_semi $ (,) <$> parseVarOrAtomic <*> option (parseAssign *> parseTerm) <* option (parseSemicolon *> parseBlank))

newtype ListTerm = ListTerm [Term]
  deriving (Show, Eq)

parseListTerm :: Parser ListTerm
parseListTerm = bracket $ ListTerm <$> sepby parseTerm parseSemicolon

data TupleTerm = TupleTerm Term [Term]
  deriving (Show, Eq)

parseTupleTerm :: Parser TupleTerm
parseTupleTerm = paren $ TupleTerm <$> parseTerm <* parseComma <*> comma_nonempty_list parseTerm
  
data SetEnumTerm = SetEnumTerm [Term]
  deriving (Show, Eq)

parseSetEnumTerm :: Parser SetEnumTerm
parseSetEnumTerm = brace $ SetEnumTerm <$> sepby parseTerm parseComma
  
data SetComprehensionTerm = SetComprehensionTerm Term HoldingVar Statement
  deriving (Show, Eq)

parseSetComprehensionTerm :: Parser SetComprehensionTerm
parseSetComprehensionTerm = brace $ SetComprehensionTerm <$> parseTerm <*> parseHoldingVar <* parseLit "|" <*> parseStatement
  
newtype RecordAssignTerm = RecordAssignTerm [(VarOrAtomic, Maybe ColonType, Expr)] -- parsed as brace-enclosed semicolon-separated list of items, with LIT_ASSIGN between ColonType and Expr
  deriving (Show, Eq)

parseRecordAssignTerm :: Parser RecordAssignTerm
parseRecordAssignTerm = RecordAssignTerm <$> brace_semi ((,,) <$> parseVarOrAtomic <*> option parseColonType <* parseAssign <*> parseExpr)
  
data Expr =
    ExprGeneralType GeneralType
  | ExprTerm Term
  | ExprProp Prop
  | ExprProofExpr ProofExpr
  | ExprSortExpr SortExpr
  deriving (Show, Eq)

parseExpr :: Parser Expr
parseExpr =
  ExprGeneralType <$> parseGeneralType <||>
  ExprTerm <$> parseTerm <||>
  ExprProp <$> parseProp <||>
  ExprProofExpr <$> parseProofExpr <||>
  ExprSortExpr <$> parseSortExpr
  
data ProofExpr =
    ProofExprQED SymbolQED -- note: removed paren(proof_expr) and moved into parser. this parses a SYMBOL_QED for the Text.
  | ProofExprParen ProofExpr
  deriving (Show, Eq)

parseProofExpr :: Parser ProofExpr
parseProofExpr =
  ProofExprQED <$> parseSymbolQED <||>
  ProofExprParen <$> paren parseProofExpr
  
data SortExpr = SortExpr (Maybe Args) LitSort -- Args should be nonempty TODO(jesse): clarify what this means wrt Args parsing
  deriving (Show, Eq)

parseSortExpr :: Parser SortExpr
parseSortExpr = SortExpr <$> (option $ parseArgs <* parseRArrow) <*> parseLitSort'
  
data LitSort = LitType | LitProp -- this information should not be discarded, so we need a separate type
  deriving (Show, Eq)

parseLitSort' :: Parser LitSort
parseLitSort' = (parseLit "type" *> return LitType) <||> (parseLit "prop" *> return LitProp)
  
data Args = Args (Maybe OptArgs) [RequiredArg] -- i think requiredargs are whitespace-separated?
  deriving (Show, Eq)

parseArgs :: Parser Args
parseArgs = Args <$> (option $ parseLit "at" *> parseOptArgs) <*> (many' parseRequiredArg)

data RequiredArg =
    RequiredArgAnnotated [VarOrAtomic] (Maybe ColonType) -- note, this must be parsed with enclosing parentheses
  | RequiredArgVarOrAtomic VarOrAtomic
  deriving (Show, Eq)

parseRequiredArg :: Parser RequiredArg
parseRequiredArg =
  (paren $ RequiredArgAnnotated <$> (many1' parseVarOrAtomic) <*> option parseColonType )<||>
  RequiredArgVarOrAtomic <$> parseVarOrAtomic
  
data CSBrace a = CSBrace a [TVar] -- TODO(jesse): add data of control sequence
  deriving (Show, Eq)

parseCSBrace :: Parser a -> Parser (CSBrace a)
parseCSBrace p = CSBrace <$> p <*> (many' $ brace parseTVar)
  
data BinaryRelationOp =
    BinaryRelationOpPrimBinaryRelationOp PrimBinaryRelationOp
  | BinaryRelationOpControlSeq (CSBrace PrimBinaryRelationControlSeq)
  deriving (Show, Eq)

parseBinaryRelationOp :: Parser BinaryRelationOp
parseBinaryRelationOp =
  BinaryRelationOpPrimBinaryRelationOp <$> parsePrimBinaryRelationOp <||>
  BinaryRelationOpControlSeq <$> parseCSBrace parsePrimBinaryRelationControlSeq
  
data Vars2 = Vars2 TVar [TVar] -- parse a TVar, then parse a comma-separated list of TVars
  deriving (Show, Eq)

parseVars2 :: Parser Vars2
parseVars2 = Vars2 <$> parseTVar <*> comma_nonempty_list parseTVar
  
newtype HoldingVar = HoldingVar (Maybe [Var]) -- for the parse to succeed, must be preceded by HOLDING and vars must be in a nonempty comma-separated list
  deriving (Show, Eq)

parseHoldingVar :: Parser HoldingVar
parseHoldingVar = HoldingVar <$> (option $ parseLit "holding" *> (sepby1 parseVar (parseComma)))

data Attribute a = Attribute [LeftAttribute] a (Maybe RightAttribute)
  deriving (Show, Eq)

parseAttribute :: Parser a -> Parser (Attribute a)
parseAttribute p = Attribute <$> (many' parseLeftAttribute) <*> p <*> option parseRightAttribute
  
data LeftAttribute =
    LeftAttributeSingleSubject PrimSimpleAdjective
  | LeftAttributeMultiSubject PrimSimpleAdjectiveMultiSubject
  deriving (Show, Eq)

parseLeftAttribute :: Parser LeftAttribute
parseLeftAttribute =
  LeftAttributeSingleSubject <$> parsePrimSimpleAdjective <||>
  LeftAttributeMultiSubject <$> parsePrimSimpleAdjectiveMultiSubject
  
data RightAttribute =
    RightAttributeIsPred [IsPred]
  | RightAttributeDoesPred [DoesPred]
  | RightAttributeStatement Statement
  deriving (Show, Eq)

parseRightAttribute :: Parser RightAttribute
parseRightAttribute =
  RightAttributeIsPred <$> sep_list (parseIsPred) <||>
  RightAttributeDoesPred <$> sep_list (parseDoesPred) <||>
  RightAttributeStatement <$> (parseLit "such" *> parseLit "that" *> parseStatement)
  
data AnyArg =
    AnyArgVar Var
  | AnyArgAnnotatedVars AnnotatedVars
  deriving (Show, Eq)

parseAnyArg :: Parser AnyArg
parseAnyArg =
  AnyArgVar <$> parseVar <||>
  AnyArgAnnotatedVars <$> parseAnnotatedVars
  
newtype LetAnnotation = LetAnnotation [AnnotatedVars] -- LIT_LET comma_nonempty_list(annotated_vars)
  deriving (Show, Eq)

parseLetAnnotation :: Parser LetAnnotation
parseLetAnnotation = LetAnnotation <$> (parseLit "let" *> comma_nonempty_list parseAnnotatedVars)
  
data AnnotatedVar = AnnotatedVar VarModifier Var (Maybe ColonType)
  deriving (Show, Eq)

parseAnnotatedVar :: Parser AnnotatedVar
parseAnnotatedVar = paren $ AnnotatedVar <$> parseVarModifier <*> (parseVar) <*> option parseColonType

data AnnotatedVars = AnnotatedVars VarModifier [Var] (Maybe ColonType)
  deriving (Show, Eq)

parseAnnotatedVars :: Parser AnnotatedVars
parseAnnotatedVars = paren $ AnnotatedVars <$> parseVarModifier <*> (many1' parseVar) <*> option parseColonType
                     
newtype VarModifier = VarModifier (Maybe [Text]) -- this is parsed as an optional LitVarMod
  deriving (Show, Eq)

parseVarModifier :: Parser VarModifier
parseVarModifier = VarModifier <$> option parseLitVarMod

data DoesPred =
    DoesPredPrimVerb PrimVerb
  | DoesPredPrimVerbMultiSubject PrimVerbMultiSubject
  | DoesPredHasPred HasPred
  | DoesPredIsPreds [IsPred]
  | DoesPredIsAPreds [IsAPred]
  deriving (Show, Eq)

parseDoesPred :: Parser DoesPred
parseDoesPred =
  DoesPredPrimVerb <$> (option parseLitDo *> (option $ parseLit "not") *> parsePrimVerb) <||>
  DoesPredPrimVerbMultiSubject <$> (option parseLitDo *> (option $ parseLit "not") *> parsePrimVerbMultiSubject) <||>
  DoesPredHasPred <$> (parseLitHas *> parseHasPred) <||>
  DoesPredIsPreds <$> (parseLitIs *> sep_list parseIsPred) <||>
  DoesPredIsAPreds <$> (parseLitIs *> sep_list parseIsAPred)
  
data HasPred =
    HasPredArticle [PossessedNoun]
  | HasPredNo PossessedNoun
  deriving (Show, Eq)

parseHasPred :: Parser HasPred
parseHasPred =
  HasPredArticle <$> sep_list (parseArticle *> parsePossessedNoun) <||>
  HasPredNo <$> (parseLit "no" *> parsePossessedNoun)
  
data PossessedNoun = PossessedNoun (Attribute PrimPossessedNoun)
  deriving (Show, Eq)

parsePossessedNoun :: Parser PossessedNoun
parsePossessedNoun = PossessedNoun <$> parseAttribute parsePrimPossessedNoun

data IsPred =
    IsPredPrimAdjective (Maybe Text) PrimAdjective
  | IsPredPrimAdjectiveMultiSubject (Maybe Text) (Maybe Text) PrimAdjectiveMultiSubject
  | IsPredHasPred HasPred
  deriving (Show, Eq)

parseIsPred :: Parser IsPred
parseIsPred =
  IsPredPrimAdjective <$> (option $ parseLit "not") <*> (parsePrimAdjective) <||>
  IsPredPrimAdjectiveMultiSubject <$> (option $ parseLit "not") <*> (option $ parseLit "pairwise") <*> parsePrimAdjectiveMultiSubject <||>
  IsPredHasPred <$> (parseLitWith *> parseHasPred)
  
data IsAPred =
    IsAPredGeneralType GeneralType
  | IsAPredNot DefiniteTerm
  deriving (Show, Eq)

parseIsAPred :: Parser IsAPred
parseIsAPred =
  IsAPredGeneralType <$> ((option $ parseLit "not") *> (option parseLitA) *> parseGeneralType) <||>
  IsAPredNot <$> ((option $ parseLit "not") *> parseDefiniteTerm)
