{-
Author(s): Jesse Michael Han (2019)

Parsing types and type ascriptions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Term where

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
import CNLean.Basic.Token

data Term =
    TermDefiniteTerm DefiniteTerm
  | TermAnyName AnyName
