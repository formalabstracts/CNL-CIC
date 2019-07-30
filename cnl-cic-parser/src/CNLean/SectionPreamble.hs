{-
Author(s): Jesse Michael Han (2019)

Section preambles
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.SectionPreamble where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic.Basic
import CNLean.Assumption
import CNLean.Type
import CNLean.PhraseList

data SectionPreamble = SectionPreamble SectionTag (Maybe Label)
  deriving (Show, Eq)

parseSectionPreamble :: Parser SectionPreamble
parseSectionPreamble = SectionPreamble <$> parseSectionTag <*> option parseLabel <* parsePeriod

newtype SectionTag = SectionTag [Text]
  deriving (Show, Eq)

parseSectionTag :: Parser SectionTag
parseSectionTag = SectionTag <$> parseLitDocument


