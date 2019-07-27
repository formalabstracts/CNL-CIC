{-
Author(s): Jesse Michael Han (2019)

Parsing axioms.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Axiom where

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
import CNLean.Assumption
import CNLean.Type

data Axiom = Axiom { preamble :: AxiomPreamble, assumptions :: [Assumption], thenPrefix :: ThenPrefix, statement :: Statement }
  deriving (Show, Eq)

parseAxiom :: Parser Axiom
parseAxiom = Axiom <$> parseAxiomPreamble <*> (many' parseAssumption) <*> parseThenPrefix <*> parseStatement

data AxiomPreamble = AxiomPreamble (Maybe Label)
  deriving (Show, Eq)

parseAxiomPreamble :: Parser AxiomPreamble
parseAxiomPreamble = AxiomPreamble <$> (parseLitAxiom *> (option parseLabel) <* parsePeriod)

-- test parseAxiomPreamble "Axiom riemann_hypothesis."
-- test parseThenPrefix "therefore"

-- test parseAxiom "Axiom The_Riemann_Hypothesis. One is positive."
-- test parseAxiom "Axiom The_Riemann_Hypothesis. The zero is not positive."

