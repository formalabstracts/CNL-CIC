{-
Author(s): Jesse Michael Han (2019)

Parsing assumptions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Assumption where

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
import CNLean.Type

data Assumption = -- note: parsing a list of assumptions means using (many1 parseAssumption)
    Assumption AssumptionPrefix Statement -- parsed with period at end
  | AssumptionLetAnnotation LetAnnotation -- parsed with period at end
  deriving (Show, Eq)
  
data AssumptionPrefix = AssumptionPrefix (Maybe [Text]) -- make consumption inspectable for debugging
  deriving (Show, Eq)

parseAssumptionPrefix = AssumptionPrefix <$> (do
  lt1 <- parseLitLets
  lt2 <- parseLitAssume
  mlt3 <- option (rp $ parseLit "that")
  case mlt3 of
    Nothing -> return $ Just $ lt1 <> lt2
    Just lt3 -> return $ Just $ lt1 <> lt2 <> lt3) <||>
  (AssumptionPrefix . Just) <$> (rp $ parseLit "let")

data ThenPrefix = ThenPrefix (Maybe Text)
  deriving (Show, Eq)

parseThenPrefix = ThenPrefix <$> (option $ parseLit "then")
