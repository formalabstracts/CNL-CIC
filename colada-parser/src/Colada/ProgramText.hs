{-
Author(s): Jesse Michael Han (2019)

High-level parsing.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.ProgramText where

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

-- type synonym possibly allowing errors for textitems
-- so that parsing can recover from errors
-- this assumes that every textitem is preceded by a newline
type RawResults s e t = [RawResult s e t]
type RawResult s e t = Either (ParseError s e) t

parseTreeOfRawResult :: RawResult s e t -> Maybe t
parseTreeOfRawResult rslt = case rslt of
  (Left err) -> Nothing
  (Right tr) -> Just tr

errorOfRawResult :: RawResult s e t -> Maybe (ParseError s e)
errorOfRawResult rslt = case rslt of
  (Left err) -> Just err
  (Right tr) -> Nothing

parseTextItem_main :: Parser TextItem
parseTextItem_main =
  (TextItemNamespace <$> parseNamespace) <||>
  (TextItemSectionPreamble <$> parseSectionPreamble) <||>
  (TextItemSectionPostamble <$> parseSectionPostamble) <||>
  (TextItemDeclaration <$> parseDeclaration)  <||>
  (TextItemMacro <$> parseMacro) <||>
  (TextItemInstr <$> parseInstr)

parseRawTextItem :: Parser (RawResult Text SimpleError TextItem)
parseRawTextItem = (withRecovery recover (Right <$> parseTextItem_main)) <* sc
  where recover err = Left err <$ manyTill item parsePeriod

parseTextItem :: Parser TextItem
parseTextItem = unoption $ parseTreeOfRawResult <$> parseRawTextItem

newtype ProgramText = ProgramText [TextItem]
  deriving (Show, Eq)

-- a CNL-compliant document must contain at least one text_item
parseRawTextItems :: Parser (RawResults Text SimpleError TextItem)
parseRawTextItems = many1' (parseRawTextItem <* sc)

parseTextItems :: Parser [TextItem]
parseTextItems = delete_nothings <$> (map parseTreeOfRawResult <$> parseRawTextItems)

parseProgramText :: Parser ProgramText
parseProgramText = ProgramText <$> parseTextItems

parseProgram = parseProgramText <* eof
