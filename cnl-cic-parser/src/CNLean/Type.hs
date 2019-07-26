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
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Applicative (liftA2)

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
    HeadStatementForAny [AnyName] Statement -- list of anyname parsed by a comma-or-LIT_AND separated list
  | HeadStatementIfThen Statement Statement
  | HeadStatementItsWrong Statement
  deriving (Show, Eq)

parseHeadStatement :: Parser HeadStatement
parseHeadStatement =
  (HeadStatementForAny <$> sep_list (parseAnyName) <*> parseStatement) <||>
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
  deriving (Show, Eq)

parseAndOrChain :: Parser AndOrChain
parseAndOrChain =
  AndChain <$> (sepby1 parsePrimaryStatement $ parseLit "and") <*> (parseLit "and" *> parseHeadPrimary) <||>
  OrChain <$> (sepby1 parsePrimaryStatement $ parseLit "or") <*> (parseLit "or" *> parseHeadPrimary)

  
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
  
data SimpleStatement = SimpleStatement [Term] [DoesPred]
  deriving (Show, Eq)
-- parse [Term] using parseTerms and parse [DoesPred] using sepby1 parseDoesPred (parseLit "and")

parseSimpleStatement :: Parser SimpleStatement -- TODO(jesse): fix me
parseSimpleStatement = liftA2 SimpleStatement parseTerms (sepby1 parseDoesPred $ parseLit "and")
  
data ThereIsStatement =
    ThereIs [NamedTerm] -- parsed with sep_list (option(lit_a) named_term)
  | ThereIsNo NamedTerm
  deriving (Show, Eq)

parseThereIsStatement =
  ThereIsNo <$> (parseLit "there" *> parseLitExist *> parseLit "no" *> parseNamedTerm) <||>
  ThereIs <$>  ((parseLit "there") *> (parseLitExist) *> (sepby1 parseNamedTerm (option parseLitA)))

newtype ConstStatement = ConstStatement [Text]
  deriving (Show, Eq)

parseConstStatement = option(parseLit "the") *> option(parseLit "thesis") *>
  ( option(parseLit "the") *> (rp $ parseLit "contrary" ) <||>
    parseLitA *> (rp $ parseLit "contradiction"))

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
  | TVarAnnotatedVars AnnotatedVars
  deriving (Show, Eq)

data TVarAssignment = TVarAssignment {tvar :: TVar, mct :: Maybe ColonType, term :: Term}
-- to be parsed with an ASSIGN literal between maybeColonType and term
  deriving (Show, Eq)

-- parseTVarAssignment :: Parser TVarAssignment
-- parseTVarAssignment = do
--   tv <- parseTVar
--   mct <- option (parseColonType)
--   (parseLit ":=")
--   t <- parseTerm
--   return $ TVarAssignment tv mct t

newtype WhereSuffix  = WhereSuffix [TVarAssignment] -- to be parsed using a brace-enclosed, semicolon-separated list
  deriving (Show, Eq)

-- parseWhereSuffix :: Parser WhereSuffix
-- parseWhereSuffix = between parseLBrace parseRBracec $ do
--   tvas <- (sepby1 parseTVarAssignment (parseLit ";"))
--   return $ WhereSuffix tvas

data ColonType = ColonType GeneralType
  deriving (Show, Eq)

parseColonType :: Parser ColonType
parseColonType = ColonType <$> ((parseColon) *> parseGeneralType)

data GeneralType = GeneralType OpenTailType
  deriving (Show, Eq)

parseGeneralType :: Parser GeneralType
parseGeneralType = empty

data OpenTailType =
  OpenTailTypeBinOpType BinOpType | OpenTailTypeQuotientType QuotientType | OpenTailTypeCoercionType CoercionType | OpenTailTypePrimStructure PrimStructure
  deriving (Show, Eq)

-- parseOpenTailType :: Parser OpenTailType
-- parseOpenTailType =
--   (parseBinOpType >>= return . OpenTailTypeBinOpType) <||>
--   (parseQuotientType >>= return . OpenTailTypeQuotientType) <||>
--   (parseCoercionType >>= return . OpenTailTypeCoercionType) <||>
--   (parsePrimStructure >>= return . OpenTailTypePrimStructure)

data CoercionType =
    CoercionTypeTerm Term 
  | ImplicitCoercion Term -- note, this is not implemented yet but will be supported in the future.
  deriving (Show, Eq)

data PrimStructure = PrimStructure  -- this is a stub for Cabarete-style structure, and is not supported yet
  deriving (Show, Eq)

data Term =
    TermDefiniteTerm DefiniteTerm
  | TermAnyName AnyName
  deriving (Show, Eq)

-- parseTerm = TermDefiniteTerm <$> parseDefiniteTerm <||> TermAnyName <$> parseAnyName

-- parseTerms :: Parser [Term]
-- parseTerms = sep_list parseTerm

data QuotientType = QuotientType {domain :: GeneralType, eqv :: Term}
  deriving (Show, Eq)

data DefiniteTerm =
    DefiniteTermSymbolicTerm SymbolicTerm
  | DefiniteTermNoun PrimDefiniteNoun
  | DefiniteTermParenNoun PrimDefiniteNoun
  deriving (Show, Eq)

data SymbolicTerm = SymbolicTerm OpenTailTerm (Maybe WhereSuffix)
  deriving (Show, Eq)

data OpenTailTerm =
    OpenTailTermLambdaTerm LambdaTerm
  | OpenTailTermLambdaFun  LambdaFun
  | OpenTaiLTermLetTerm LetTerm
  | OpenTaiLTermIfThenElseTerm IfThenElseTerm
  | OpenTailTermTdopTerm TdopTerm
  deriving (Show, Eq)

data IfThenElseTerm = IfThenElseTerm Prop Term OpenTailTerm
  deriving (Show, Eq)

-- parseIfThenElseTerm = do --TODO(jesse): make sure this actually works
--   (IfThenElseTerm)
--     <$> (parseLit "if" *> parseProp) <*>
--         (parseLit "then" *> parseTerm) <*>
--         (parseLit "else" *> parseOpenTailTerm)


data LambdaFun = LambdaFun Identifier GeneralizedArgs (Maybe ColonType) OpenTailTerm
  deriving (Show, Eq)

-- parseLambdaFun =
--   (LambdaFun) <$>
--     ((parseLit "fun") *> parseIdentifier) <*>
--     (parseGeneralizedArgs) <*>
--     (option parseColonType) <*>
--     (parseAssign *> parseOpenTailTerm)


data LetTerm = LitTerm Term Term OpenTailTerm
  deriving (Show, Eq)

-- parseLetTerm = (LitTerm) <$>
--   (parseLit "let" *> parseTerm) <*>
--   (parseAssign *> parseTerm) <*>
--   (parseLit "in" *> parseOpenTailTerm)


data TdopTerm = DummyConstructor2 -- TODO(jesse): fix me
  deriving (Show, Eq)

data LambdaTerm =
    PrimLambdaBinder PrimLambdaBinder GeneralizedArgs OpenTailTerm -- parse binder comma to separate args from tail
  | LambdaTermGeneralizedArg GeneralizedArg OpenTailTerm -- parse mapsto to separate args from tail
  deriving (Show, Eq)

data BinOpType = BinOpType { head :: TypeOperand, tail :: [(TypeOp,TypeOperand)]}
  deriving (Show, Eq)

data TypeOp =
    TypeOpPrimTypeOp Token -- TODO(jesse) fix me
  | TypeOpDummyConstructor -- TODO(jesse) fix me
  deriving (Show, Eq)

parseTypeOp :: Parser TypeOp
parseTypeOp = empty

data TypeOperand =
    TypeOperandBinderType BinderType
  | TypeOperandDependentVars DependentVars
  deriving (Show, Eq)

data BinderType =
    BinderTypeAppType
  | BinderTypePrimPiBinder PrimPiBinder GeneralizedArgs BinderType -- parsed with a binder_comma between generalizedargs and bindertype
  deriving (Show, Eq)

data GeneralizedArgs = GeneralizedArgs {optArgs :: OptArgs, generalizedArg :: [GeneralizedArg] }
  deriving (Show, Eq)

data VarOrAtomic =
    VarOrAtomicVar Var
  | VarOrAtomicAtomic AtomicId
  deriving (Show, Eq)

parseVarOrAtomic :: Parser VarOrAtomic
parseVarOrAtomic = (VarOrAtomicVar <$> parseVar) <||> (VarOrAtomicAtomic <$> parseAtomicId)
  
data GeneralizedArg =
    GeneralizedArg TightestTerm
  | GeneralizedArgVarOrAtomic {varOrAtomic1 :: VarOrAtomic, varOrAtomic2 :: VarOrAtomic, optColonType :: (Maybe ColonType)}
  deriving (Show, Eq)

data DependentVars = DependentVars {maybeOptArgs :: (Maybe OptArgs), annotatedVars :: [AnnotatedVars]}
  deriving (Show, Eq)

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
  
data TypedNameWithoutAttribute = -- note: I removed the extra constructor paren(typed_name_without_attribute) and am instead encoding that into the parser (it will attempt to parse a pair of enclosing parentheses first).
    TypedNamePrimTypedName PrimTypedName
  | TypedNameTVar TVar
  | TypedNamePrimClassifier PrimClassifier TVar
  | TypedNameVar Var GeneralType -- parsed as var "with type" general_type
  deriving (Show, Eq)

newtype FreePredicate = FreePredicate (Attribute FreePredicateWithoutAttribute)
  deriving (Show, Eq)

data FreePredicateWithoutAttribute =
    FreePredicateProp Prop HoldingVar -- note, this 
  | FreePredicateVars2 Vars2 BinaryRelationOp TdopTerm
  deriving (Show, Eq)

-- TODO(jesse): see if this can be factored out of this mutually recursive mess into a separate file
data Prop =
    PropBinderProp BinderProp
  | PropTdopProp TdopProp
  deriving (Show, Eq)

data TdopProp =
    TdopPropHead (Maybe BinderProp) [PropOp]
  | TdopPropTail [(BinderProp,[PropOp])] (Maybe BinderProp)
  deriving (Show, Eq)

data PropOp =
    PropOpPrim PrimPropositionalOp
  | PropOpCS (CSBrace PrimPropositionalOpControlSeq)
  deriving (Show, Eq)

data BinderProp =
    BinderPropAppProp AppProp
  | BinderPropTdopRelProp TdopRelProp
  | BinderPropPrimBinderProp PrimBinderProp Args BinderProp -- parsed with binder comma between Args and BinderProp
  deriving (Show, Eq)

data TdopRelProp = TdopRelProp [TdopTerm] [(BinaryRelationOp, TdopTerm)] -- last list must be empty, tdop_terms is nonempty comma-separated
  deriving (Show, Eq)

data TightestProp =
    ParenStatement Statement -- parsed with mandatory parentheses
  | TightestPropIdentifierProp IdentifierProp
  | TightestPropVar Var
  | TightestPropAnnotatedProp AnnotatedProp
  deriving (Show, Eq)

data Identifier =
    IdentifierAtomicId AtomicId
  | IdentifierHierId HierId
  deriving (Show, Eq)

parseIdentifier :: Parser Identifier
parseIdentifier = (IdentifierHierId <$> parseHierId) <||> (IdentifierAtomicId <$> parseAtomicId)

newtype IdentifierProp = IdentifierProp Identifier
  deriving (Show, Eq)

parseIdentifierProp = parseIdentifier >>= return . IdentifierProp

newtype AnnotatedProp = AnnotatedProp Prop
  deriving (Show, Eq)

-- parseAnnotatedProp :: Parser AnnotatedProp
-- parseAnnotatedProp = between parseLParen parseRParen parseProp

data AppProp = AppProp TightestProp AppArgs
  deriving (Show, Eq)

data AppArgs = AppArgs (Maybe RecordAssignTerm) [TightestExpr]
  deriving (Show, Eq)

data TightestExpr =
    TightestExprTerm TightestTerm
  | TightestExprProp TightestProp
  | TightestExprType TightestType
  | TightestExprProof ProofExpr
  deriving (Show, Eq)

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
  deriving (Show, Eq)

newtype ParenType = ParenType GeneralType
  deriving (Show, Eq)

parseParenType = between parseLParen parseRParen parseGeneralType >>= return . ParenType


newtype AnnotatedType = AnnotatedType GeneralType
  deriving (Show, Eq)

parseAnnotatedType = between parseLParen parseRParen $ parseGeneralType <* parseColon <* parseLit "type"


newtype ControlSeqType = ControlSeqType (CSBrace PrimTypeControlSeq)
  deriving (Show, Eq)

newtype ConstType = ConstType TypeIdentifier
  deriving (Show, Eq)
newtype TypeIdentifier = TypeIdentifier Identifier
  deriving (Show, Eq)
parseTypeIdentifier = parseIdentifier >>= return . TypeIdentifier
parseConstType = parseTypeIdentifier >>= return . ConstType


data VarType =
    VarTypeVar Var
  | VarTypeAnnotated Var
  deriving (Show, Eq)
  
-- note: possibly need an extra sc in the first branch
parseVarType = VarTypeAnnotated <$> (between parseLParen parseRParen $ parseVar <* parseLit "Type") <||> VarTypeVar <$> parseVar

data Subtype = Subtype Term HoldingVar Statement
  deriving (Show, Eq)

-- parseSubtype = between parseLBrace parseRBrace $ do
--   t <- parseTerm
--   hvar <- parseHoldingVar
--   parseLit "//" -- TODO(jesse): confirm this interpretation of LIT_SUBTYPEMID
--   s <- parseStatement
--   return $ Subtype t hvar s

data InductiveType = InductiveType Identifier Args (Maybe ColonSort) [Maybe AltConstructor]
  deriving (Show, Eq)

newtype ColonSort = ColonSort SortExpr
  deriving (Show, Eq)

-- parseColonSort = parseColon *> parseSortExpr >>= return . ColonSort


data AltConstructor = AltConstructor Identifier Args ColonType
  deriving (Show, Eq)

-- parseAltConstructor = parseAlt *> do
--   id <- parseIdentifier
--   args <- parseArgs
--   ct <- parseColonType
--   return $ AltConstructor id args ct

data MutualInductiveType = MutualInductiveType [Identifier] Args [(AtomicId, Args, ColonType, [AltConstructor])]   -- comma-separated nonempty list of identifiers, second list parsed with mandatory prefix LIT_WITH
-- MutualInductiveType must be parsed with a LIT_END at the end.
  deriving (Show, Eq)


{-
structure : option(LIT_NOTATIONAL) LIT_STRUCTURE 
     option(lit_param) args
     option(LIT_WITH) option(brace_semi(field))
     option(LIT_SATISFYING satisfying_preds {}) {}
-}
data Structure = Structure Args (Maybe [Field]) (Maybe SatisfyingPreds) -- [Field] is parsed by brace_semi
  deriving (Show, Eq)

-- parseStructure = do
--   args <- option(parseLit "notational") *> parseLit "structure" *> option (parseLitParam) parseArgs
--   mfs <- option(parseLit "with") *> option(brace_semi parseField)
--   msps <- option (option (parseLit "satisfying") *> parseSatisfyingPreds)
--   return $ Structure args mfs msps

data Field = Field FieldPrefix FieldIdentifier (Maybe FieldSuffix)
  deriving (Show, Eq)

newtype SatisfyingPreds =SatisfyingPreds [SatisfyingPred]
  deriving (Show, Eq)

newtype FieldPrefix = FieldPrefix [[Text]]
  deriving (Show, Eq)
parseFieldPrefix = parseAlt *> (many1' parseLitFieldKey) >>= return . FieldPrefix


data FieldIdentifier = FieldIdentifier VarOrAtomic (Maybe ColonType)
  deriving (Show, Eq)

parseFieldIndentifier = do
  va <- parseVarOrAtomic
  mct <- option parseColonType
  return $ FieldIdentifier va mct

data FieldSuffix =
    WithoutNotation
  | FieldSuffixFieldAssign FieldAssign
  deriving (Show, Eq)

-- parseFieldSuffix =
--   (do parseLit "without"
--       parseLit "notation"
--       return WithoutNotation) <||>
--   parseFieldAssign >>= return . FieldSuffixFieldAssign

newtype FieldAssign = FieldAssign Expr
  deriving (Show, Eq)

-- parseFieldAssign = parseAssign *> parseExpr >>= return . FieldAssign


-- parseSatisfyingPreds = brace_semi (parseSatisfyingPred) >>= return . SatisfyingPreds

data SatisfyingPred = SatisfyingPred (Maybe AtomicId) Prop
  deriving (Show, Eq)

-- parseSatisfyingPred = do
--   mid <- parseAlt *> option(parseAtomicId <* parseColon)
--   p <- parseProp
--   return $ SatisfyingPred mid p

data TightestTerm =
    TightestTerm TightestPrefix
  | TightestTermFieldAcc TightestTerm FieldAcc
  | TightestTermApplySub [TightestTerm] -- should be parsed by a paren-enclosed nonempty list of tightest terms, preceded by an ApplySub literal
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data AltTerm =
    AltTermCaseTerm Term -- parsed as CASE term OF
  | AltTermMatchTerm MatchSeq -- parsed as MATCH match_seq WITH
  | AltTermLambdaFunction LambdaFunction
  deriving (Show, Eq)

-- parseAltTerm :: Parser AltTerm
-- parseAltTerm =
--   parseLit "case" *> parseTerm <* parseLit "of" >>= return . AltTermCaseTerm <||>
--   parseMatchSeq >>= return . AltTermMatchTerm <||>
--   parseLambdaFunction >>= return . AltTermLambdaFunction

newtype MatchSeq = MatchSeq [Term]  -- parsed as a comma-separated nonempty list
  deriving (Show, Eq)

-- parseMatchSeq = sepby1 parseTerm parseComma >>= return . MatchSeq

data LambdaFunction = LambdaFunction Identifier Args
  deriving (Show, Eq)

-- parseLambdaFunction = parseLit "function" *> do
--   id <- parseIdentifier
--   args <- parseArgs
--   return $ LambdaFunction id args

data ControlSeqTerm = ControlSeqTerm (CSBrace PrimTermControlSeq)
  deriving (Show, Eq)

data DelimitedTerm =
    DelimitedTermParen
  | DelimitedTermAnnotated
  | DelimitedTermMake
  | DelimitedTermList
  | DelimitedTermTuple
  | DelimitedTermSetEnum
  | DelimitedTermSetComprehension
  deriving (Show, Eq)

newtype RecordAssignTerm = RecordAssignTerm [(VarOrAtomic, Maybe ColonType, Expr)] -- parsed as brace-enclosed semicolon-separated list of items, with LIT_ASSIGN between ColonType and Expr
  deriving (Show, Eq)

data Expr =
    ExprGeneralType GeneralType
  | ExprTerm Term
  | ExprProp Prop
  | ExprProofExpr ProofExpr
  | ExprSortExpr SortExpr
  deriving (Show, Eq)

newtype ProofExpr = ProofExpr Text -- note: removed paren(proof_expr) and moved into parser. this parses a SYMBOL_QED for the Text.
  deriving (Show, Eq)

data SortExpr = SortExpr (Maybe Args) LitSort -- Args should be nonempty
  deriving (Show, Eq)

data LitSort = LitType | LitProp -- this information should not be discarded, so we need a separate type
  deriving (Show, Eq)

parseLitSort' :: Parser LitSort
parseLitSort' = (parseLit "type" *> return LitType) <||> (parseLit "prop" *> return LitProp)
  
data Args = Args (Maybe OptArgs) [RequiredArg] -- i think requiredargs are whitespace-separated?
  deriving (Show, Eq)

data RequiredArg =
    RequiredArgAnnotated VarOrAtomic (Maybe ColonType) -- note, this must be parsed with enclosing parentheses
  | RequiredArgVarOrAtomic VarOrAtomic
  deriving (Show, Eq)

data CSBrace a = CSBrace a [Expr]
  deriving (Show, Eq)

data BinaryRelationOp =
    BinaryRelationOpPrimBinaryRelationOp PrimBinaryRelationOp
  | BinaryRelationOpControlSeq (CSBrace PrimBinaryRelationControlSeq)
  deriving (Show, Eq)

data Vars2 = Vars2 TVar [TVar] -- parse a TVar, then parse a comma-separated list of TVars
  deriving (Show, Eq)

newtype HoldingVar = HoldingVar (Maybe [Var]) -- for the parse to succeed, must be preceded by HOLDING and vars must be in a nonempty comma-separated list
  deriving (Show, Eq)

parseHoldingVar :: Parser HoldingVar
parseHoldingVar = HoldingVar <$> (option $ parseLit "holding" *> (sepby1 parseVar (parseComma)))

data Attribute a = Attribute [LeftAttribute] a [RightAttribute]
  deriving (Show, Eq)

data LeftAttribute =
    LeftAttributeSingleSubject PrimSimpleAdjective
  | LeftAttributeMultiSubject PrimSimpleAdjectiveMultiSubject
  deriving (Show, Eq)

data RightAttribute =
    RightAttributeIsPred IsPred -- TODO(jesse) implement this inside of Primitive.hs
  | RightAttributeDoesPred DoesPred -- TODO(jesse) implement this inside of Primitive.hs
  | RightAttributeStatement Statement
  deriving (Show, Eq)

data AnyArg =
    AnyArgVar Var
  | AnyArgAnnotatedVars AnnotatedVars
  deriving (Show, Eq)

newtype LetAnnotation = LetAnnotation [AnnotatedVars] -- LIT_LET comma_nonempty_list(annotated_vars)
  deriving (Show, Eq)

-- note: we don't need annotatedvar because we can always just parse a singleton list
data AnnotatedVars = AnnotatedVars {varModifier :: VarModifier, vars :: [Var], maybeColonType :: (Maybe ColonType)
                                   }
                     deriving (Show, Eq)
                     
newtype VarModifier = VarModifier (Maybe [Text]) -- this is parsed as an optional LitVarMod
  deriving (Show, Eq)

parseVarModifier :: Parser VarModifier
parseVarModifier = VarModifier <$> option parseLitVarMod

parseAnnotatedVars :: Parser AnnotatedVars
parseAnnotatedVars = between parseLParen parseRParen $
  do varmod <- option parseLitVarMod
     vs     <- (many1' parseVar)
     mct    <- option parseColonType
     return $ AnnotatedVars (VarModifier varmod) vs mct

parseLetAnnotation :: Parser [AnnotatedVars]
parseLetAnnotation = parseLit "let" *> (sepby1 parseAnnotatedVars parseComma)

