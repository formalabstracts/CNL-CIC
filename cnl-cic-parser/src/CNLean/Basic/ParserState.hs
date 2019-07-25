{-
Author(s): Jesse Michael Han (2019)

Managing the parser state.
-}

{-# LANGUAGE OverloadedStrings #-}
module CNLean.Basic.ParserState where

import Prelude
import Control.Monad.Trans.State
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack, toLower)
import Data.Void
import Control.Monad.Trans.State.Lazy (modify, gets)
import qualified Data.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import CNLean.Basic.Core
import CNLean.Basic.State

updateSerialCounter fs x = fs {serialCounter = x}
updateHiddenCount fs x = fs {hiddenCount = x}
updateIdCount fs x = fs {idCount = x}
updateVarDecl fs x = fs {varDecl = x}
updateStrSyms fs x = fs {strSyms = x}

setClsList x fs = fs {clsList = x}
pushClsList z fs = setClsList (z:(clsList fs)) fs
prependClsList zs fs = setClsList (zs <> (clsList fs)) fs

updateClsList :: [Text] -> Parser ()
updateClsList = modify . pushClsList

updateClsList2 :: [[Text]] -> Parser ()
updateClsList2 = modify . prependClsList

-- TODO(jesse): define analogous functions for the rest of the state
