{-
Author(s): Jesse Michael Han (2019)

Parsing definitions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Definition where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, option, Label, Tokens)
import Control.Monad (guard, liftM)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.List (intersperse)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Monad.Trans.State.Lazy (modify, gets)

import Control.Lens

import CNLean.Basic.Basic
import CNLean.Type
import CNLean.Assumption
import CNLean.Pattern

data Definition = Definition DefinitionPreamble [Assumption] DefinitionAffirm
  deriving (Show, Eq)

--TODO(jesse): find fix for token parser in definition statement consuming copula literals
parseDefinition :: Parser Definition
parseDefinition = Definition <$> parseDefinitionPreamble <*> (many' parseAssumption) <*> parseDefinitionAffirm

newtype DefinitionPreamble = DefinitionPreamble (Maybe Label) -- parse (Lit AXIOM), maybe a label, optional period.
  deriving (Show, Eq)

parseDefinitionPreamble :: Parser DefinitionPreamble
parseDefinitionPreamble = DefinitionPreamble <$> do
  parseLitDef
  ml <- option parseLabel
  parsePeriod
  return ml

-- test parseDefinitionPreamble "DEFINITION foo."

-- test parseDefinitionPreamble "DEF."

data DefinitionAffirm = DefinitionAffirm DefinitionStatement (Maybe ThisExists)
  deriving (Show, Eq)

parseDefinitionAffirm :: Parser DefinitionAffirm
parseDefinitionAffirm = DefinitionAffirm <$> parseDefinitionStatement <* parsePeriod <*> option (parseThisExists)
  
newtype ThisExists = ThisExists [ThisDirectivePred]
  deriving (Show, Eq)

parseThisExists :: Parser ThisExists
parseThisExists = parseLit "this" *> (ThisExists <$> sep_list(parseThisDirectivePred))

-- test parseThisExists "this exists and is well defined and is canonical"

data ThisDirectivePred =
    ThisDirectivePredAdjective [[Text]]
  | ThisDirectivePredVerb ThisDirectiveVerb
  deriving (Show, Eq)

parseThisDirectivePred :: Parser ThisDirectivePred
parseThisDirectivePred =
  ThisDirectivePredAdjective <$> (parseLit "is" *> (sep_list1 parseThisDirectiveAdjective)) <||>
  ThisDirectivePredVerb <$> parseThisDirectiveVerb

parseThisDirectiveAdjective :: Parser [Text]
parseThisDirectiveAdjective =
  parse_any_of $ map (\x -> parse_list x parseLit) thisDirAdjList
  where
    thisDirAdjList :: [[Text]]
    thisDirAdjList =
      [
        ["unique"],
        ["canonical"],
        ["welldefined"],
        ["well-defined"],
        ["well", "defined"],
        ["total"],
        ["well", "propped"],
        ["exhaustive"]
      ]

newtype ThisDirectiveVerb = ThisDirectiveVerbExists (Maybe ThisDirectiveRightAttr)
  deriving (Show, Eq)

parseThisDirectiveVerb = parseLit "exists" *> (ThisDirectiveVerbExists <$> option parseThisDirectiveRightAttr)

newtype ThisDirectiveRightAttr = ThisDirectiveRightAttr [Text]
  deriving (Show, Eq)

parseThisDirectiveRightAttr :: Parser ThisDirectiveRightAttr
parseThisDirectiveRightAttr = ThisDirectiveRightAttr <$> (parse_list ["by", "recursion"] parseLit)

data DefinitionStatement =
    DefinitionStatementClassifier ClassifierDef
  | DefinitionStatementTypeDef TypeDef
  | DefinitionStatementFunctionDef FunctionDef
  | DefinitionStatementPredicateDef PredicateDef
  -- | DefinitionStatementStructureDef  StructureDef
  -- | DefinitionStatementInductiveDef InductiveDef
  -- | DefinitionStatementMutualInductiveDef MutualInductiveDef
  deriving (Show, Eq)

parseDefinitionStatement :: Parser DefinitionStatement
parseDefinitionStatement =
  DefinitionStatementClassifier <$> parseClassifierDef <||>
  DefinitionStatementTypeDef <$> parseTypeDef <||>
  DefinitionStatementFunctionDef <$> parseFunctionDef <||>
  DefinitionStatementPredicateDef <$> parsePredicateDef--  <||>
  -- DefinitionStatementStructureDef <$> parseStructureDef <||>
  -- DefinitionStatementInductiveDef <$> parseInductiveDef <||>
  -- DefinitionStatementMutualInductiveDef <$> parseMutualInductiveDef

data PredicateDef = PredicateDef PredicateHead IffJunction Statement
  deriving (Show, Eq)

parsePredicateDef :: Parser PredicateDef
parsePredicateDef = PredicateDef <$> (parseOptSay *> parsePredicateHead) <*> parseIffJunction <*> parseStatement

patternOfPredicateDef :: PredicateDef -> Parser [Patt]
patternOfPredicateDef fd = case fd of
  PredicateDef predicatehead iffjunction statement -> case predicatehead of
    PredicateHeadPredicateTokenPattern (predtkpatt) -> patternOfPredicateTokenPattern predtkpatt
    PredicateHeadIdentifierPattern idpatt -> patternOfIdentifierPattern idpatt
    PredicateHeadSymbolPattern sympatt mpl -> patternOfSymbolPattern sympatt

--TODO(jesse): add side effects for predicate definitions
-- identifier pattern becomes primRelation
-- Symbolpattern becomes... binary?

data IffJunction = IffJunction
  deriving (Show, Eq)

parseIffJunction = parseLitIff *> return IffJunction

data PredicateHead =
    PredicateHeadPredicateTokenPattern PredicateTokenPattern
  | PredicateHeadSymbolPattern SymbolPattern (Maybe ParenPrecedenceLevel)
  | PredicateHeadIdentifierPattern IdentifierPattern
  -- | PredicateHeadControlSeqPattern ControlSeqPattern
  -- | PredicateHeadBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parsePredicateHead :: Parser PredicateHead
parsePredicateHead =
  PredicateHeadPredicateTokenPattern <$> parsePredicateTokenPattern <||>
  PredicateHeadSymbolPattern <$> parseSymbolPattern <*> (option parseParenPrecedenceLevel) <||>
  PredicateHeadIdentifierPattern <$> parseIdentifierPattern--  <||>
  -- PredicateHeadControlSeqPattern <$> parseControlSeqPattern <||>
  -- PredicateHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data PredicateTokenPattern =
    PredicateTokenPatternAdjectivePattern AdjectivePattern
  | PredicateTokenPatternAdjectiveMultiSubjectPattern AdjectiveMultiSubjectPattern
  | PredicateTokenPatternVerbPattern VerbPattern
  | PredicateTokenPatternVerbMultiSubjectPattern VerbMultiSubjectPattern
  deriving (Show, Eq)

parsePredicateTokenPattern :: Parser PredicateTokenPattern
parsePredicateTokenPattern =
  PredicateTokenPatternAdjectivePattern <$> parseAdjectivePattern <||>
  PredicateTokenPatternAdjectiveMultiSubjectPattern <$> parseAdjectiveMultiSubjectPattern <||>
  PredicateTokenPatternVerbPattern <$> parseVerbPattern <||>
  PredicateTokenPatternVerbMultiSubjectPattern <$> parseVerbMultiSubjectPattern

patternOfPredicateTokenPattern :: PredicateTokenPattern -> Parser [Patt]
patternOfPredicateTokenPattern ptkpatt = case ptkpatt of
  (PredicateTokenPatternAdjectivePattern adjpatt) -> patternOfAdjectivePattern adjpatt
  (PredicateTokenPatternAdjectiveMultiSubjectPattern adjmspatt) -> patternOfAdjectiveMultiSubjectPattern adjmspatt
  (PredicateTokenPatternVerbPattern vpatt) -> patternOfVerbPattern vpatt
  (PredicateTokenPatternVerbMultiSubjectPattern vmspatt) -> patternOfVerbMultiSubjectPattern vmspatt

data AdjectivePattern = AdjectivePattern TVar TokenPattern
  deriving (Show, Eq)

parseAdjectivePattern :: Parser AdjectivePattern
parseAdjectivePattern =
  AdjectivePattern <$> parseTVar <*> (parseLit "is" *> (option $ parseLit "called") *> parseTokenPattern)

patternOfAdjectivePattern :: AdjectivePattern -> Parser [Patt]
patternOfAdjectivePattern (AdjectivePattern tv tkpatt) =
  (<>) <$> patternOfTVar tv <*> patternOfTokenPattern tkpatt

data AdjectiveMultiSubjectPattern = AdjectiveMultiSubjectPattern VarMultiSubject TokenPattern
  deriving (Show, Eq)

patternOfAdjectiveMultiSubjectPattern :: AdjectiveMultiSubjectPattern -> Parser [Patt]
patternOfAdjectiveMultiSubjectPattern (AdjectiveMultiSubjectPattern varms tkpatt) =
  (<>) <$> patternOfVarMultiSubject varms <*> patternOfTokenPattern tkpatt

parseAdjectiveMultiSubjectPattern :: Parser AdjectiveMultiSubjectPattern
parseAdjectiveMultiSubjectPattern = AdjectiveMultiSubjectPattern <$> parseVarMultiSubject <*> parseTokenPattern

data VerbPattern = VerbPattern TVar TokenPattern
  deriving (Show, Eq)

patternOfVerbPattern :: VerbPattern -> Parser [Patt]
patternOfVerbPattern (VerbPattern tv tkpatt) =
  (<>) <$> patternOfTVar tv <*> patternOfTokenPattern tkpatt

parseVerbPattern :: Parser VerbPattern
parseVerbPattern = VerbPattern <$> parseTVar <*> parseTokenPattern

data VerbMultiSubjectPattern = VerbMultiSubjectPattern VarMultiSubject TokenPattern
  deriving (Show, Eq)

patternOfVerbMultiSubjectPattern :: VerbMultiSubjectPattern -> Parser [Patt]
patternOfVerbMultiSubjectPattern (VerbMultiSubjectPattern vms tkpatt) =
  (<>) <$> patternOfVarMultiSubject vms <*> patternOfTokenPattern tkpatt

parseVerbMultiSubjectPattern :: Parser VerbMultiSubjectPattern
parseVerbMultiSubjectPattern = VerbMultiSubjectPattern <$> parseVarMultiSubject <*> parseTokenPattern

data VarMultiSubject =
    VarMultiSubjectTVar TVar TVar
  | VarMultiSubjectParen Var Var ColonType
  deriving (Show, Eq)

patternOfVarMultiSubject :: VarMultiSubject -> Parser [Patt]
patternOfVarMultiSubject x = case x of
  (VarMultiSubjectTVar tv1 tv2) -> (<>) <$> patternOfTVar tv1 <*> patternOfTVar tv2
  (VarMultiSubjectParen v1 v2 ct) -> (<>) <$> patternOfVar v1 <*> patternOfVar v2

parseVarMultiSubject :: Parser VarMultiSubject
parseVarMultiSubject =
  VarMultiSubjectTVar <$> parseTVar <* parseComma <*> parseTVar <||>
  (paren $ (VarMultiSubjectParen <$> parseVar <* parseComma <*> parseVar <*> parseColonType))

data StructureDef = StructureDef IdentifierPattern Structure
  deriving (Show, Eq)

parseStructureDef :: Parser StructureDef
parseStructureDef = StructureDef <$>
  (option parseLitA *> parseIdentifierPattern) <* parseLit "is" <* parseLitA <*>
  parseStructure


data InductiveDef = InductiveDef InductiveType
  deriving (Show, Eq)

parseInductiveDef :: Parser InductiveDef
parseInductiveDef = InductiveDef <$> (parseOptDefine *> parseInductiveType)

data MutualInductiveDef = MutualInductiveDef MutualInductiveType
  deriving (Show, Eq)

parseMutualInductiveDef :: Parser MutualInductiveDef
parseMutualInductiveDef = MutualInductiveDef <$> (parseOptDefine *> parseMutualInductiveType)

data FunctionDef = FunctionDef FunctionHead Copula PlainTerm
  deriving (Show, Eq)

patternOfFunctionDef :: FunctionDef -> Parser [Patt]
patternOfFunctionDef fd = case fd of
  FunctionDef functionhead copula plainterm -> case functionhead of
    FunctionHeadFunctionTokenPattern (FunctionTokenPattern tkpatt) -> patternOfTokenPattern tkpatt
    FunctionHeadIdentifierPattern idpatt -> patternOfIdentifierPattern idpatt
    FunctionHeadSymbolPattern sympatt mpl -> patternOfSymbolPattern sympatt

precOfFunctionDef :: FunctionDef -> Parser (Int, AssociativeParity)
precOfFunctionDef fd = case fd of
  FunctionDef functionhead copula plainterm -> case functionhead of
    FunctionHeadFunctionTokenPattern (FunctionTokenPattern tkpatt) -> empty
    FunctionHeadIdentifierPattern idpatt -> empty
    FunctionHeadSymbolPattern sympatt mpl -> case mpl of
      Nothing -> return defaultPrec
      Just (ParenPrecedenceLevelPrecedenceLevel (PrecedenceLevel ni mp)) -> precHandler ni mp
      Just (ParenPrecedenceLevelParen (PrecedenceLevel ni mp)) -> precHandler ni mp
      where
        precHandler ni mp = case mp of
          Nothing -> precHandler ni (Just defaultAssociativeParity)
          (Just ap) -> (,) <$> (readNumInt ni) <*> return ap

parseFunctionDef :: Parser FunctionDef
parseFunctionDef =
  with_result (with_result parse_function_def_main m) m'
  where
    parse_function_def_main = FunctionDef <$> (parseOptDefine *> parseFunctionHead) <*>
                                parseCopula <* option (parseLitEqual) <* option (parseLit "the")
                                <*> parsePlainTerm
    m = \x -> patternOfFunctionDef x >>= updatePrimDefiniteNoun
    m' = \x -> do patt <- patternOfFunctionDef x
                  precOfFunctionDef x >>= (uncurry $ updatePrimPrecTable patt)
-- TODO(jesse): differentiate updates based on the parsed pattern---maybe move side effects to simpler parsers

data FunctionHead =
    FunctionHeadFunctionTokenPattern FunctionTokenPattern
  | FunctionHeadSymbolPattern SymbolPattern (Maybe ParenPrecedenceLevel)
  | FunctionHeadIdentifierPattern IdentifierPattern
  -- | FunctionHeadControlSeqPattern ControlSeqPattern
  -- | FunctionHeadBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parseFunctionHead :: Parser FunctionHead
parseFunctionHead =
  FunctionHeadFunctionTokenPattern <$> parseFunctionTokenPattern <||>
  FunctionHeadSymbolPattern <$> parseSymbolPattern <*> (option parseParenPrecedenceLevel)<||>
  FunctionHeadIdentifierPattern <$> parseIdentifierPattern--  <||>
  -- FunctionHeadControlSeqPattern <$> parseControlSeqPattern <||>
  -- FunctionHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data FunctionTokenPattern = FunctionTokenPattern TokenPattern
  deriving (Show, Eq)

parseFunctionTokenPattern :: Parser FunctionTokenPattern
parseFunctionTokenPattern = FunctionTokenPattern <$> (parseLit "the" *> parseTokenPattern)

data SymbolLowercase = -- corresponds to literal "symbol", not "SYMBOL" in the grammar specification
    SymbolLowercaseSymbol Symbol
  | SymbolLowercaseCSBrace (CSBrace ControlSequence)
  deriving (Show, Eq)

patternOfSymbolLowercase :: SymbolLowercase -> Parser [Patt]
patternOfSymbolLowercase x = case x of
  SymbolLowercaseSymbol symb -> patternOfSymbol symb
  SymbolLowercaseCSBrace (CSBrace cs tvars) -> patternOfControlSequence cs <+> (patternOfList patternOfTVar tvars)

parseSymbolLowercase :: Parser SymbolLowercase
parseSymbolLowercase =
  SymbolLowercaseSymbol <$> parseSymbol <||>
  SymbolLowercaseCSBrace <$> (parseCSBrace parseControlSequence)

data SymbolPattern = SymbolPattern (Maybe TVar) SymbolLowercase [(TVar, SymbolLowercase)] (Maybe TVar)
  deriving (Show, Eq)

patternOfSymbolPattern :: SymbolPattern -> Parser [Patt]
patternOfSymbolPattern (SymbolPattern mtvar symbs tvarsymbs mtvar') =
  (patternOfOption patternOfTVar mtvar) <+>
  (patternOfSymbolLowercase symbs) <+>
  (patternOfList (\(a,b) -> patternOfTVar a <+> patternOfSymbolLowercase b) tvarsymbs)
  <+> (patternOfOption patternOfTVar mtvar')

parseSymbolPattern :: Parser SymbolPattern
parseSymbolPattern = SymbolPattern <$> (option parseTVar) <*> parseSymbolLowercase <*>
                                       (many' $ (,) <$> parseTVar <*> parseSymbolLowercase) <*>
                                       (option parseTVar)   

data TypeDef = TypeDef TypeHead Copula GeneralType
  deriving (Show, Eq)

parseTypeDef :: Parser TypeDef
parseTypeDef = TypeDef <$> parseTypeHead <*> parseCopula <* parseLitA <*> parseGeneralType

data TypeHead =
    TypeHeadTypeTokenPattern TypeTokenPattern
  | TypeHeadIdentifierPattern IdentifierPattern
  | TypeHeadControlSeqPattern ControlSeqPattern
  | TypeHeadBinaryControlSeqPattern BinaryControlSeqPattern
  deriving (Show, Eq)

parseTypeHead :: Parser TypeHead
parseTypeHead =
  TypeHeadTypeTokenPattern <$> parseTypeTokenPattern <||>
  TypeHeadIdentifierPattern <$> parseIdentifierPattern <||>
  TypeHeadControlSeqPattern <$> parseControlSeqPattern <||>
  TypeHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern

data TypeTokenPattern = TypeTokenPattern TokenPattern
  deriving (Show, Eq)

parseTypeTokenPattern :: Parser TypeTokenPattern
parseTypeTokenPattern = TypeTokenPattern <$> (parseLitA *> parseTokenPattern)

 -- (* restriction: tokens in pattern cannot be a variant of
 --    "to be", "called", "iff" "a" "stand" "denote"
 --    cannot start with "the"  *)

parsePatternToken :: Parser Token
parsePatternToken = fail_iff_succeeds (lookAhead' parseCopula) *> -- note(jesse): added to ensure copula literals are not consumed by token pattern parsing
  (guard_result "forbidden token parsed, failing" parseToken $
                      \x -> not $ elem (tokenToText x) ["the", "to be", "called", "iff", "a", "stand", "denote"])

newtype Tokens = Tokens [Token]
  deriving (Show, Eq)

parseTokens :: Parser Tokens
parseTokens = Tokens <$> many1' parsePatternToken

tokensToTokens :: Tokens -> [Token]
tokensToTokens tks0@(Tokens tks) = tks

data TokenPattern = TokenPattern Tokens [(TVar, Tokens)] (Maybe TVar)
  deriving (Show, Eq)

parseTokenPattern :: Parser TokenPattern
parseTokenPattern = TokenPattern <$> parseTokens <*>
                                     (many' $ (,) <$> parseTVar <*> parseTokens) <*>
                                     (option parseTVar)

patternOfTokenPattern :: TokenPattern -> Parser [Patt]
patternOfTokenPattern tkPatt@(TokenPattern (Tokens tks) tvstkss mtvar) =
  (<>) <$> ( do strsyms <- concat <$> use (allStates strSyms)
                return $ (map (Wd . tokenToText'_aux strsyms) tks) <>
                     concat (map (\(tv,tks) -> Vr : map (Wd . pure . tokenToText) (tokensToTokens tks)) tvstkss) )
            <*> ((unoption $ return mtvar) *> return [Vr] <||> return [])

data Copula =
    CopulaIsDefinedAs
  | CopulaAssign
  | CopulaDenote
  deriving (Show, Eq)

parseCopula :: Parser Copula
parseCopula =
  parseLitIs *> (option parseLitDefinedAs) *> return CopulaIsDefinedAs <||>
  parseAssign *> return CopulaAssign <||>
  parseLitDenote *> return CopulaDenote

data IdentifierPattern =
    IdentifierPattern Identifier Args (Maybe ColonType)
  | IdentifierPatternBlank Args (Maybe ColonType)
  deriving (Show, Eq)

parseIdentifierPattern :: Parser IdentifierPattern
parseIdentifierPattern =
  IdentifierPattern <$> parseIdentifier <*> parseArgs <*> (option parseColonType) <||>
  IdentifierPatternBlank <$> parseArgs <*> (option parseColonType)

patternOfIdentifierPattern :: IdentifierPattern -> Parser [Patt]
patternOfIdentifierPattern idpatt =
  case idpatt of
    IdentifierPattern ident args mct -> (patternOfIdent ident) <+> (patternOfArgs args)
    IdentifierPatternBlank args mct -> (patternOfArgs args)
  -- (<>) <$> (return $ (map (Wd . pure . tokenToText) tks) <>
  --                    concat (map (\(tv,tks) -> Vr : map (Wd . pure . tokenToText) (tokensToTokens tks)) tvstkss) )
  --           <*> ((unoption $ return mtvar) *> return [Vr] <||> return [])
   
data ControlSeqPattern = ControlSeqPattern ControlSequence [TVar]
  deriving (Show, Eq)

parseControlSeqPattern :: Parser ControlSeqPattern
parseControlSeqPattern = ControlSeqPattern <$> parseControlSequence <*> (many' $ brace $ parseTVar)

data BinaryControlSeqPattern = BinaryControlSeqPattern TVar ControlSeqPattern TVar
  deriving (Show, Eq)

parseBinaryControlSeqPattern :: Parser BinaryControlSeqPattern
parseBinaryControlSeqPattern = BinaryControlSeqPattern <$> parseTVar <*> parseControlSeqPattern <*> parseTVar

data ParenPrecedenceLevel =
    ParenPrecedenceLevelPrecedenceLevel PrecedenceLevel
  | ParenPrecedenceLevelParen PrecedenceLevel 
  deriving (Show, Eq)

parseParenPrecedenceLevel :: Parser ParenPrecedenceLevel
parseParenPrecedenceLevel =
  ParenPrecedenceLevelPrecedenceLevel <$> parsePrecedenceLevel <||>
  ParenPrecedenceLevelParen <$> (paren $ parsePrecedenceLevel)

parseAssociativeParity :: Parser AssociativeParity
parseAssociativeParity =
  parseLit "left" *> return AssociatesLeft <||>
  parseLit "right" *> return AssociatesRight <||>
  parseLit "no" *> return AssociatesNone

data PrecedenceLevel = PrecedenceLevel NumInt (Maybe AssociativeParity)
  deriving (Show, Eq)

parsePrecedenceLevel :: Parser PrecedenceLevel
parsePrecedenceLevel = PrecedenceLevel <$> (parseLit "with" *> parseLit "precedence" *> parseNumInt) <*> (option $ parseLit "and" *> parseAssociativeParity <* parseLit "associativity")

newtype ClassifierDef = ClassifierDef ClassTokens
  deriving (Show, Eq)

newtype ClassTokens = ClassTokens [[Token]]
  deriving (Show, Eq)

parseClassTokens = ClassTokens <$> sepby1 (many1' ((notFollowedBy (parseLitIs <* option parseLitA <* parseLitClassifier)) *> parseToken)) parseComma

parseClassifierDef =
  ClassifierDef <$>
  (do ctkss@(ClassTokens tkss) <- (parseLit "let" *>  parseClassTokens <* parseLitIs <* option parseLitA <* parseLitClassifier)
      updateClsList2 (map (liftM tokenToText) tkss)
      return $ ctkss)

-- test parseClassifierDef "let scheme, schemes, stacks be classifiers"
-- test (parseClassifierDef *> gets clsList) "let scheme, schemes, stacks, derived stacks be classifiers"
-- test parseClassifierDef "let lattice be a classifier"
