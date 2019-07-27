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

data Definition = Definition DefinitionPreamble [Assumption] DefinitionAffirm
  deriving (Show, Eq)

data DefinitionAffirm = DefinitionAffirm DefinitionStatement (Maybe ThisExists)
  deriving (Show, Eq)

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
  -- | FunctionDef
  -- | PredicateDef
  -- | StructureDef 
  -- | InductiveDef
  -- | MutualInductiveDef
  deriving (Show, Eq)

data TypeDef = TypeDef TypeHead Copula GeneralType
  deriving (Show, Eq)

parseTypeDef :: Parser TypeDef
parseTypeDef = TypeDef <$> parseTypeHead <*> parseCopula <* parseLitA <*> parseGeneralType

data TypeHead =
    TypeHeadTypePattern TypePattern
  | TypeHeadIdentifierPattern IdentifierPattern
  | TypeHeadControlSeqPattern ControlSeqPattern
  | TypeHeadBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parseTypeHead :: Parser TypeHead
parseTypeHead =
  TypeHeadTypePattern <$> parseTypePattern <||>
  TypeHeadIdentifierPattern <$> parseIdentifierPattern <||>
  TypeHeadControlSeqPattern <$> parseControlSeqPattern <||>
  TypeHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data TypePattern = TypePattern TokenPattern
  deriving (Show, Eq)

parseTypePattern :: Parser TypePattern
parseTypePattern = TypePattern <$> (parseLitA *> parseTokenPattern)

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

data IdentifierPattern = IdentifierPattern Identifier Args (Maybe ColonType)
  deriving (Show, Eq)

parseIdentifierPattern :: Parser IdentifierPattern
parseIdentifierPattern = IdentifierPattern <$> parseIdentifier <*> parseArgs <*> (option parseColonType)

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

data PrecedenceLevel = PrecendenceLevel Numeric (Maybe AssociativeParity)
  deriving (Show, Eq)

parsePrecedenceLevel :: Parser PrecedenceLevel
parsePrecedenceLevel = PrecendenceLevel <$> (parseLit "with" *> parseLit "precedence" *> parseNumeric) <*> (option $ parseLit "and" *> parseAssociativeParity <* parseLit "associativity")


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
