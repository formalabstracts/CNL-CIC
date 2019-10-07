{-
Author(s): Jesse Michael Han (2019)

Parsing declarations.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Declaration where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import Colada.Basic.Basic
import Colada.Axiom
import Colada.Definition
import Colada.Theorem

data Declaration =
    DeclarationAxiom Axiom
  | DeclarationDefinition Definition
  | DeclarationTheorem Theorem
  deriving (Show, Eq)

parseDeclaration :: Parser Declaration
parseDeclaration =
  DeclarationAxiom <$> parseAxiom <||>
  DeclarationDefinition <$> parseDefinition <||>
  DeclarationTheorem <$> parseTheorem
