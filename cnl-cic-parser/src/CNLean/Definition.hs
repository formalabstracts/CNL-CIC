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

import CNLean.Basic
import CNLean.Token

data DefinitionPreamble = DefinitionPreamble Token (Maybe Label) -- parse (Lit AXIOM), maybe a label, optional period.
  deriving (Show, Eq)

parseDefinitionPreamble :: Parser DefinitionPreamble
parseDefinitionPreamble = do
  tk <- ((parseLit_aux DEFINITION) <||> (parseLit_aux DEF) <||> (parseLit_aux LEMMA) <||> (parseLit_aux COROLLARY) >>= return . Lit)
  maybeLabel <- option (parseLabel)
  try (parsePeriod)
  return $ DefinitionPreamble tk maybeLabel
