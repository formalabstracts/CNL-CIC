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

import Control.Lens

data SectionPreamble = SectionPreamble SectionTag (Maybe Label)
  deriving (Show, Eq)

parseSectionPreamble_aux :: Parser (SectionPreamble, Int)
parseSectionPreamble_aux = do
  (sec_tag, level) <- parseSectionTag_aux
  (,) <$> (SectionPreamble sec_tag <$> (option parseLabel <* parsePeriod)) <*> return level
  
parseSectionPreamble :: Parser SectionPreamble
parseSectionPreamble =
  fst <$> (with_result parseSectionPreamble_aux (updateStack . snd))
  where
    updateStack k = modify $ sectionHandler initialFState emptyFState k

newtype SectionTag = SectionTag [Text]
  deriving (Show, Eq)

parseSectionTag_aux :: Parser (SectionTag, Int)
parseSectionTag_aux =
  (_1 %~ SectionTag) <$> parseLitDocument_aux
