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
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Monad.Trans.State.Lazy (modify, gets)

import CNLean.Basic.Basic
import CNLean.Type
import CNLean.Assumption

data Definition = Definition DefinitionPreamble [Assumption] DefinitionAffirm
  deriving (Show, Eq)

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
  | DefinitionStatementPredicateDef PredicateDef -- TODO(jesse): remove the 3 fields StructureDef, InductiveDef, MutualInductiveDef
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

data IffJunction = IffJunction
  deriving (Show, Eq)

parseIffJunction = parseLitIff *> return IffJunction

data PredicateHead = -- TODO(jesse): remove controlseq and binarycontrolseq cases
    PredicateHeadPredicateTokenPattern PredicateTokenPattern
  | PredicateHeadSymbolPattern SymbolPattern (Maybe ParenPrecedenceLevel)
  | PredicateHeadIdentifierPattern IdentifierPattern
  | PredicateHeadControlSeqPattern ControlSeqPattern
  | PredicateHeadBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parsePredicateHead :: Parser PredicateHead
parsePredicateHead =
  PredicateHeadPredicateTokenPattern <$> parsePredicateTokenPattern <||>
  PredicateHeadSymbolPattern <$> parseSymbolPattern <*> (option parseParenPrecedenceLevel) <||>
  PredicateHeadIdentifierPattern <$> parseIdentifierPattern <||>
  PredicateHeadControlSeqPattern <$> parseControlSeqPattern <||>
  PredicateHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data PredicateTokenPattern =
    PredicateTokenPatternAdjectivePattern AdjectivePattern
  | PredicateTokenPatternAdjectiveMultiSubjectPattern AdjectiveMultiSubjectPattern
  | PredicateTokenPatternVerbPattern VerbPattern
  | PredicateTokenPatternVerbMultiSubjectPattern VerbMultiSubjectPattern
  deriving (Show, Eq)

data AdjectivePattern = AdjectivePattern TVar TokenPattern
  deriving (Show, Eq)

parseAdjectivePattern :: Parser AdjectivePattern
parseAdjectivePattern =
  AdjectivePattern <$> parseTVar <*> (parseLit "is" *> (option $ parseLit "called") *> parseTokenPattern)

data AdjectiveMultiSubjectPattern = AdjectiveMultiSubjectPattern VarMultiSubject TokenPattern
  deriving (Show, Eq)

parseAdjectiveMultiSubjectPattern :: Parser AdjectiveMultiSubjectPattern
parseAdjectiveMultiSubjectPattern = AdjectiveMultiSubjectPattern <$> parseVarMultiSubject <*> parseTokenPattern

data VerbPattern = VerbPattern TVar TokenPattern
  deriving (Show, Eq)

parseVerbPattern :: Parser VerbPattern
parseVerbPattern = VerbPattern <$> parseTVar <*> parseTokenPattern

data VerbMultiSubjectPattern = VerbMultiSubjectPattern VarMultiSubject TokenPattern
  deriving (Show, Eq)

parseVerbMultiSubjectPattern :: Parser VerbMultiSubjectPattern
parseVerbMultiSubjectPattern = VerbMultiSubjectPattern <$> parseVarMultiSubject <*> parseTokenPattern

data VarMultiSubject =
    VarMultiSubjectTVar TVar TVar
  | VarMultiSubjectParen Var Var ColonType
  deriving (Show, Eq)

parseVarMultiSubject :: Parser VarMultiSubject
parseVarMultiSubject =
  VarMultiSubjectTVar <$> parseTVar <* parseComma <*> parseTVar <||>
  (paren $ (VarMultiSubjectParen <$> parseVar <* parseComma <*> parseVar <*> parseColonType))

parsePredicateTokenPattern :: Parser PredicateTokenPattern
parsePredicateTokenPattern =
  PredicateTokenPatternAdjectivePattern <$> parseAdjectivePattern <||>
  PredicateTokenPatternAdjectiveMultiSubjectPattern <$> parseAdjectiveMultiSubjectPattern <||>
  PredicateTokenPatternVerbPattern <$> parseVerbPattern <||>
  PredicateTokenPatternVerbMultiSubjectPattern <$> parseVerbMultiSubjectPattern  

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

-- TODO(jesse): implement function def (and functionhead) correctly

data FunctionDef =
    FunctionDefFunctionTokenPattern FunctionTokenPattern
  | FunctionDefSymbolPattern SymbolPattern (Maybe ParenPrecedenceLevel)
  | FunctionDefIdentifierPattern IdentifierPattern
  | FunctionDefControlSeqPattern ControlSeqPattern
  | FunctionDefBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parseFunctionDef :: Parser FunctionDef
parseFunctionDef =
  FunctionDefFunctionTokenPattern <$> parseFunctionTokenPattern <||>
  FunctionDefSymbolPattern <$> parseSymbolPattern <*> (option parseParenPrecedenceLevel)<||>
  FunctionDefIdentifierPattern <$> parseIdentifierPattern <||>
  FunctionDefControlSeqPattern <$> parseControlSeqPattern <||>
  FunctionDefBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data FunctionTokenPattern = FunctionTokenPattern TokenPattern
  deriving (Show, Eq)

parseFunctionTokenPattern :: Parser FunctionTokenPattern
parseFunctionTokenPattern = FunctionTokenPattern <$> (parseLit "the" *> parseTokenPattern)

data SymbolLowercase =
    SymbolLowercaseSymbol Symbol
  | SymbolLowercaseCSBrace (CSBrace ControlSequence)
  deriving (Show, Eq)

parseSymbolLowercase :: Parser SymbolLowercase
parseSymbolLowercase =
  SymbolLowercaseSymbol <$> parseSymbol <||>
  SymbolLowercaseCSBrace <$> (parseCSBrace parseControlSequence)

data SymbolPattern = SymbolPattern (Maybe TVar) SymbolLowercase [(TVar, SymbolLowercase)] (Maybe TVar)
  deriving (Show, Eq)

parseSymbolPattern :: Parser SymbolPattern
parseSymbolPattern = SymbolPattern <$> (option parseTVar) <*> parseSymbolLowercase <*>
                                       (many' $ (,) <$> parseTVar <*> parseSymbolLowercase) <*>
                                       (option parseTVar)   

data TypeDef = TypeDef TypeHead Copula GeneralType
  deriving (Show, Eq)

parseTypeDef :: Parser TypeDef
parseTypeDef = TypeDef <$> parseTypeHead <*> parseCopula <* parseLitA <*> parseGeneralType

data TypeHead = -- TODO(jesse): remove the maybe parenprecedencelevel
    TypeHeadTypeTokenPattern TypeTokenPattern
  | TypeHeadIdentifierPattern IdentifierPattern
  | TypeHeadControlSeqPattern ControlSeqPattern
  | TypeHeadBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parseTypeHead :: Parser TypeHead
parseTypeHead =
  TypeHeadTypeTokenPattern <$> parseTypeTokenPattern <||>
  TypeHeadIdentifierPattern <$> parseIdentifierPattern <||>
  TypeHeadControlSeqPattern <$> parseControlSeqPattern <||>
  TypeHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data TypeTokenPattern = TypeTokenPattern TokenPattern
  deriving (Show, Eq)

parseTypeTokenPattern :: Parser TypeTokenPattern
parseTypeTokenPattern = TypeTokenPattern <$> (parseLitA *> parseTokenPattern)

 -- (* restriction: tokens in pattern cannot be a variant of
 --    "to be", "called", "iff" "a" "stand" "denote"
 --    cannot start with "the"  *)

parsePatternToken :: Parser Token
parsePatternToken = guard_result "forbidden token parsed, failing" parseToken $
                      \x -> not $ elem (tokenToText x) ["the", "to be", "called", "iff", "a", "stand", "denote"]

newtype Tokens = Tokens [Token]
  deriving (Show, Eq)

parseTokens :: Parser Tokens
parseTokens = Tokens <$> many1' parsePatternToken

data TokenPattern = TokenPattern Tokens [(TVar, Tokens)] (Maybe TVar)
  deriving (Show, Eq)

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

data ControlSeqPattern = ControlSeqPattern ControlSequence [TVar]
  deriving (Show, Eq)

parseControlSeqPattern :: Parser ControlSeqPattern
parseControlSeqPattern = ControlSeqPattern <$> parseControlSequence <*> (many' $ brace $ parseTVar)

data BinaryControlSeqPattern = BinaryControlSeqPattern TVar ControlSeqPattern TVar
  deriving (Show, Eq)

parseBinaryControlSeqPattern :: Parser BinaryControlSeqPattern
parseBinaryControlSeqPattern = BinaryControlSeqPattern <$> parseTVar <*> parseControlSeqPattern <*> parseTVar

data ParenPrecedenceLevel =
    ParenPrecendenceLevelPrecedenceLevel PrecedenceLevel
  | ParenPrecedenceLevelParen PrecedenceLevel 
  deriving (Show, Eq)

parseParenPrecedenceLevel :: Parser ParenPrecedenceLevel
parseParenPrecedenceLevel =
  ParenPrecendenceLevelPrecedenceLevel <$> parsePrecedenceLevel <||>
  ParenPrecedenceLevelParen <$> (paren $ parsePrecedenceLevel)

data AssociativeParity =
    AssociatesLeft
  | AssociatesRight
  | AssociatesNone
  deriving (Show, Eq)

parseAssociativeParity :: Parser AssociativeParity
parseAssociativeParity =
  parseLit "left" *> return AssociatesLeft <||>
  parseLit "right" *> return AssociatesRight <||>
  parseLit "no" *> return AssociatesNone

data PrecedenceLevel = PrecendenceLevel NumInt (Maybe AssociativeParity)
  deriving (Show, Eq)

parsePrecedenceLevel :: Parser PrecedenceLevel
parsePrecedenceLevel = PrecendenceLevel <$> (parseLit "with" *> parseLit "precedence" *> parseNumInt) <*> (option $ parseLit "and" *> parseAssociativeParity <* parseLit "associativity")


parseTokenPattern :: Parser TokenPattern
parseTokenPattern = TokenPattern <$> parseTokens <*>
                                     (many' $ (,) <$> parseTVar <*> parseTokens) <*>
                                     (option parseTVar)



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
