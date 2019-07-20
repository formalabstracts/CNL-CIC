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
import Text.Megaparsec hiding (Token)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic
import CNLean.Token 

data SectionPreamble = SectionPreamble { sectionTag :: SectionTag
                                       , maybeLabel :: Label } deriving (Show, Eq)

newtype Label =
  MaybeToken (Maybe Token)
  deriving (Show, Eq)

parseLabel :: Parser Label
parseLabel = (parseAtomicId >>= return . MaybeToken . Just) <||> (sc *> (return $ MaybeToken Nothing))

newtype SectionTag =
  LitDocument LitDocument
  deriving (Show, Eq)

data LitDocument =
    Document
  | Article
  | Section
  | Subsection
  | Subsubsection
  deriving (Show, Eq)

parseLitDocument :: Parser LitDocument
parseLitDocument = (parseLit_aux DOCUMENT >> return Document)
              <||> (parseLit_aux ARTICLE  >> return Article)
              <||> (parseLit_aux SECTION  >> return Section)
              <||> (parseLit_aux SUBSECTION >> return Subsection)
              <||> (parseLit_aux SUBSUBSECTION >> return Subsubsection)

parseSectionTag :: Parser SectionTag
parseSectionTag = parseLitDocument >>= return . LitDocument

parseSectionPreamble :: Parser SectionPreamble
parseSectionPreamble = do
  secTag <- parseSectionTag
  label  <- parseLabel
  parsePeriod
  return $ SectionPreamble secTag label

