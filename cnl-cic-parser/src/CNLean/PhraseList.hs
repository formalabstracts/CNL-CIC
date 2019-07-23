{-# LANGUAGE ExistentialQuantification #-}
{-
Author(s): Jesse Michael Han (2019)

Parsing statements
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.PhraseList where

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

-- TODO(jesse) write a parser which parses configuration files like phrase_lists.txt and outputs a parser
-- for now, we hard-code the phrase lists as specific parsers. It will be easy to refactor this to a more general setup.

data Maybe' a =
    J a
  | Q a
  deriving (Show, Eq)

phraseListFiller :: [[Maybe' Text]]
phraseListFiller = [
                     [J "we", J "have", Q "that"],
                     [J "we", J "know", Q "that"],
                     [Q "we", J "put"],
                     [J "we", J "write"],
                     [Q "we", J "write"]
                   ]

-- note: if (aux x) fails, its place in the list is filled by `Nothing`.
-- The output of a phrase list parser generated in this way will have to be sanitized
-- but doing that should be easy.
maybe'ToParser :: (a -> Parser b) -> Maybe' a -> Parser (Maybe b)
maybe'ToParser aux m = case m of
  J x -> aux x >>= return . Just
  Q x -> option (aux x)

maybe'Lit :: Maybe' Text -> Parser (Maybe [Text])
maybe'Lit = maybe'ToParser (rp . parseLit)

parsePhraseList_aux0 :: [Maybe' Text] -> Parser (Maybe [Text])
parsePhraseList_aux0 ph = case ph of
  [] -> return Nothing
  x:xs -> (maybe'Lit x) <+> (parsePhraseList_aux0 xs)

parsePhraseList_aux :: [[Maybe' Text]] -> Parser (Maybe [Text])
parsePhraseList_aux phs = case phs of
  [] -> return Nothing
  x:xs -> (parsePhraseList_aux0 x) <||> (parsePhraseList_aux xs)
  
parsePhraseListFiller = parsePhraseList_aux phraseListFiller

-- test parsePhraseListFiller "we have" -> Just ["we", "have"]
-- test parsePhraseListFiller "put" -> Just ["put"]
-- test parsePhraseListFiller "ramalamadingdong" -> Nothing... :^)
