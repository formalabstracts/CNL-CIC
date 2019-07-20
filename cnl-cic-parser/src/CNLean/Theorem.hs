{-
Author(s): Jesse Michael Han (2019)

Parsing theorems.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Theorem where

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

import CNLean.Basic
import CNLean.Token

data TheoremPreamble = TheoremPreamble Token (Maybe Label) -- parse (Lit AXIOM), maybe a label, optional period.
  deriving (Show, Eq)

parseTheoremPreamble :: Parser TheoremPreamble
parseTheoremPreamble = do
  tk <- ((parseLit_aux THEOREM) <||> (parseLit_aux PROPOSITION) <||> (parseLit_aux LEMMA) <||> (parseLit_aux COROLLARY) >>= return . Lit)
  maybeLabel <- option (parseLabel)
  try (parsePeriod)
  return $ TheoremPreamble tk maybeLabel
