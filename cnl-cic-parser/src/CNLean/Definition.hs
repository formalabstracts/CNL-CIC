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
import Text.Megaparsec hiding (Token, option, Label)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

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
  -- | TypeDef  -- TODO(jesse): implement the rest of these
  -- | FunctionDef
  -- | PredicateDef
  -- | StructureDef 
  -- | InductiveDef
  -- | MutualInductiveDef
  deriving (Show, Eq)

newtype ClassifierDef = ClassifierDef ClassTokens
  deriving (Show, Eq)

newtype ClassTokens = ClassTokens [Token]
  deriving (Show, Eq)

parseClassTokens = ClassTokens <$> sepby1 parseToken parseComma

parseClassifierDef = ClassifierDef <$> (parseLit "let" *>  parseClassTokens <* parseLitIs <* option parseLitA <* parseLitClassifier)

-- test parseClassifierDef "let scheme, schemes, stacks be classifiers"
-- test parseClassifierDef "let lattice be a classifier"
