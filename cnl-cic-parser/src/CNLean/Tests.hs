{-
Author(s): Jesse Michael Han (2019)

Tests, for preliminary debugging. This will eventually be merged into the test suite and the absolute paths will be removed.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Tests where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.Foldable
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Core
import CNLean.Basic
import CNLean.Token
import CNLean.Namespace
import CNLean.SectionPreamble
import CNLean.Declaration
import CNLean.Macro
import CNLean.Instr

testSectionPreamble :: IO ()
testSectionPreamble = do
  txt <- TIO.readFile "/home/pv/org/projects/jmh-CNL-CIC/cnl-cic-parser/test/section.txt"
  parseTest (many1 $ sc *> parseSectionPreamble <* sc) txt
