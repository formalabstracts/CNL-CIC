{-
Author(s): Jesse Michael Han (2019)

Parsing statements
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Statement where

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

data Statement =
    HeadStatement HeadStatement
  | ChainStatement ChainStatement

data HeadStatement =
    HeadStatementForAny [AnyName] Statement -- list of anyname parsed by a comma-or-LIT_AND separated list
  | HeadStatementIfThen Statement Statement
  | HeadStatementItsWrong Statement

data ChainStatement =
    AndOrChain AndOrChain
  | AndOrChainIff AndOrChain Statement
