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

data AxiomPreamble = AxiomPreamble Token (Maybe Label) -- parse (Lit AXIOM), maybe a label, optional period.
  deriving (Show, Eq)

-- parseAxiomPreamble :: Parser AxiomPreamble
-- parseAxiomPreamble = do
--   tk <- ((parseLit_aux AXIOM) <||> (parseLit_aux CONJECTURE) <||> (parseLit_aux HYPOTHESIS) >>= return . Lit)
--   maybeLabel <- option (parseLabel)
--   try (parsePeriod)
--   return $ AxiomPreamble tk maybeLabel

  
