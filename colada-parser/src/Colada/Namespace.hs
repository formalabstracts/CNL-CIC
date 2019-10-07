{-
Author(s): Jesse Michael Han (2019)

Macros.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Namespace where

import Prelude hiding (Word) -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Word)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import Colada.Basic.Basic

data Namespace = NamespaceDummyConstructor
  deriving (Show, Eq)

parseNamespace :: Parser Namespace
parseNamespace = empty

-- parseNamespace :: Parser Namespace
-- parseNamespace = do xs <- (many1' item)
--                     return DummyConstructor
