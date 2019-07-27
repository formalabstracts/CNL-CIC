{-
Author(s): Jesse Michael Han (2019)

High-level parsing.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Core where

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

import CNLean.Basic.Basic
import CNLean.Namespace
import CNLean.SectionPreamble
import CNLean.Declaration
import CNLean.Macro
import CNLean.Instr

data TextItem =
    Namespace Namespace
  | SectionPreamble SectionPreamble
  | Declaration Declaration
  | Macro Macro
  | Instr Instr
  deriving (Show, Eq)

parseTextItem :: Parser TextItem
parseTextItem =
  (Namespace <$> parseNamespace) <||>
  (SectionPreamble <$> parseSectionPreamble) <||>
  (Declaration <$> parseDeclaration)  <||>
  (Macro <$> parseMacro) <||>
  (Instr <$> parseInstr)

newtype ProgramText = TextItems [TextItem]
  deriving (Show, Eq)

parseProgramText :: Parser ProgramText
parseProgramText = (many1' parseTextItem) >>= return . TextItems

parseProgram :: Parser ProgramText
parseProgram = parseProgramText <* eof
