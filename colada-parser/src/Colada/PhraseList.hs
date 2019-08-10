{-# LANGUAGE ExistentialQuantification #-}
{-
Author(s): Jesse Michael Han (2019)

Parsing phrase lists
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.PhraseList where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Control.Monad.Trans.State.Lazy (modify, gets)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Lens

import Colada.Basic.Basic

parseOfParserMarkUp x =
  case x of
    J txt -> (Just <$> parseLit txt)
    Q txt -> (option $ parseLit txt)
    A ps -> parse_any_of (map parseOfParserMarkUp ps)

parsePhraseList :: [[ParserMarkUp Text]] -> Parser ([Maybe Text])
parsePhraseList mtxts = parse_any_of (map parseOfParserMarkUps mtxts)
  where parseOfParserMarkUps :: [ParserMarkUp Text] -> Parser ([Maybe Text])
        parseOfParserMarkUps z = case z of
          [] -> return []
          x:xs -> (pure <$> parseOfParserMarkUp x) <+> parseOfParserMarkUps xs
  
parsePhraseListFiller_aux :: Parser [Text] -- note: maybe make this [Maybe Text] for easier debugging?
parsePhraseListFiller_aux =
  delete_nothings <$> ((use $ allStates primPhraseListFiller) >>= parsePhraseList . concat)

  -- (use (top . primPhraseListFiller) >>= parsePhraseList) -- TODO fix this, use top . isn't right

parsePhraseListTransition_aux :: Parser [Text]
parsePhraseListTransition_aux =
  delete_nothings <$> ((use $ allStates primPhraseListTransition) >>= parsePhraseList . concat)

parsePhraseListProofStatement_aux :: Parser [Text]
parsePhraseListProofStatement_aux =
  delete_nothings <$> ((use $ allStates primPhraseListProofStatement) >>= parsePhraseList . concat)

newtype PhraseListFiller = PhraseListFiller [Text]
  deriving (Show, Eq)

parsePhraseListFiller = PhraseListFiller <$> parsePhraseListFiller_aux

newtype Filler = Filler (Maybe PhraseListFiller)
  deriving (Show, Eq)

parseFiller :: Parser Filler
parseFiller = Filler <$> option parsePhraseListFiller

newtype PhraseListTransition = PhraseListTransition [Text]
  deriving (Show, Eq)

parsePhraseListTransition :: Parser PhraseListTransition
parsePhraseListTransition = PhraseListTransition <$> parsePhraseListTransition_aux

newtype PhraseListProofStatement = PhraseListProofStatement [Text]
  deriving (Show, Eq)

parsePhraseListProofStatement :: Parser PhraseListProofStatement
parsePhraseListProofStatement = PhraseListProofStatement <$> parsePhraseListProofStatement_aux

-- test parsePhraseListFiller_aux "we have" -> Just ["we", "have"]
-- test parsePhraseListFiller_aux "put" -> Just ["put"]
-- test parsePhraseListFiller_aux "ramalamadingdong" -> fails
-- test parsePhraseListTransition_aux "without loss of generality"
-- test parsePhraseListProofStatement_aux "the theorem now follows."
-- test parsePhraseListProofStatement_aux "the theorem follows."

ocamlsc :: Parser ()
ocamlsc = L.space space1 empty (L.skipBlockComment "(*" "*)")

preprocessPhraseList :: Text -> Parser [[ParserMarkUp Text]]
                         -- TODO : add support for parsing alternation lists
                         -- TODO : integrate this into the state for on-the-fly extension of the phrase lists
preprocessPhraseList txt =
  case (runParser (toParsec parsePhraseListFile) "" txt) of
           Left _ -> fail "adios"
           Right s -> return s
  where
  parsePhraseListFile :: Parser [[ParserMarkUp Text]]
  parsePhraseListFile = many1' $ many1' (foo <* ocamlsc)
    where
      foo :: Parser (ParserMarkUp Text)
      foo = do chnk <- (many1 not_whitespace_aux)
               let b = (last chnk)
               if (b == "?") then return (Q . join . init $ chnk)
                             else return (J . join $ chnk)
                        
-- processPhraseList :: Text -> Parser (Maybe [Text])
-- processPhraseList txt = preprocessPhraseList txt >>= parsePhraseList_aux
