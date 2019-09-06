{-
Author(s): Jesse Michael Han (2019)

Parsing assumptions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Assumption where

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

import Colada.Basic.Basic
import Colada.Type

data Assumption = -- note: parsing a list of assumptions means using (many1 parseAssumption)
    Assumption AssumptionPrefix Statement -- parsed with period at end
  | AssumptionLetAnnotation LetAnnotation -- parsed with period at end
  deriving (Show, Eq)

parseAssumption :: Parser Assumption
parseAssumption =
  (AssumptionLetAnnotation <$> (parseLetAnnotation <* parsePeriod)) <||>
  (Assumption <$> parseAssumptionPrefix <*> parseStatement <* parsePeriod)
  
newtype AssumptionPrefix = AssumptionPrefix [Text]
  deriving (Show, Eq)

parseAssumptionPrefix =
  AssumptionPrefix
    <$> concat
      <$> delete_nothings
        <$>
          (pure . pure <$> parseLitLets) <+>
          (pure . pure <$> parseLitAssume) <+>
          (pure <$> option (pure <$> parseLit "that"))

data ThenPrefix = ThenPrefix (Maybe [Text])
  deriving (Show, Eq)

parseThenPrefix = ThenPrefix <$> (option $ parseLitThen)
