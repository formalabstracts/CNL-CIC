{-
Author(s): Jesse Michael Han (2019)

Parsing declarations.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Declaration where

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

import CNLean.Basic
import CNLean.Token
import CNLean.Axiom
import CNLean.Definition
import CNLean.Theorem

data Declaration =
  DeclarationAxiom Axiom | DeclarationDefinition Definition | DeclarationTheorem Theorem

parseDeclaration :: Parser Declaration
parseDeclaration = (parseAxiom >>= return . DeclarationAxiom) <||>
                   (parseDefinition >>= return . DeclarationDefinition) <||>
                   (parseTheorem >>= return . DeclarationTheorem)
  

-- data Definition =

-- data Theorem = 

-- parseDeclaration :: Parser Declaration
-- parseDeclaration = do xs <- (many1 item)
--                       return DummyConstructor
