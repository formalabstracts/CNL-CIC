{-
Author(s): Jesse Michael Han (2019)

High-level parsing.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Core where

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
import Colada.Namespace
import Colada.SectionPreamble
import Colada.Declaration
import Colada.Macro
import Colada.Instr

data TextItem =
    TextItemNamespace Namespace
  | TextItemSectionPreamble SectionPreamble
  | TextItemSectionPostamble SectionPostamble
  | TextItemDeclaration Declaration
  | TextItemMacro Macro
  | TextItemInstr Instr
  deriving (Show, Eq)

parseTextItem :: Parser TextItem
parseTextItem =
  (TextItemNamespace <$> parseNamespace) <||>
  (TextItemSectionPreamble <$> parseSectionPreamble) <||>
  (TextItemSectionPostamble <$> parseSectionPostamble) <||>
  (TextItemDeclaration <$> parseDeclaration)  <||>
  (TextItemMacro <$> parseMacro) <||>
  (TextItemInstr <$> parseInstr)

newtype ProgramText = TextItems [TextItem]
  deriving (Show, Eq)

parseProgramText :: Parser ProgramText
parseProgramText = (many1' parseTextItem) >>= return . TextItems

parseProgram :: Parser ProgramText
parseProgram = parseProgramText <* eof
