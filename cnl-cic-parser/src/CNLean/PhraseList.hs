{-# LANGUAGE ExistentialQuantification #-}
{-
Author(s): Jesse Michael Han (2019)

Parsing phrase lists
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.PhraseList where

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

import CNLean.Basic.Basic

-- TODO(jesse) write a parser which parses configuration files like phrase_lists.txt and outputs a parser
-- for now, we hard-code the phrase lists as specific parsers. It will be easy to refactor this to a more general setup.

-- phraseListFiller :: [[ParserMarkUp Text]]
-- phraseListFiller = [
--                      [J "we", J "have", Q "that"],
--                      [J "we", J "know", Q "that"],
--                      [Q "we", J "put"],
--                      [J "we", J "write"],
--                      [Q "we", J "write"]
--                    ]

-- note: if (aux x) fails, its place in the list is filled by `Nothing`.
-- The output of a phrase list parser generated in this way will have to be sanitized
-- but doing that should be easy.

-- maybe'ToParser :: (a -> Parser b) -> ParserMarkUp a -> Parser (Maybe b)
-- maybe'ToParser aux m = case m of
--   J x -> aux x >>= return . Just
--   Q x -> option (aux x)

-- maybe'Lit :: ParserMarkUp Text -> Parser (Maybe [Text])
-- maybe'Lit = maybe'ToParser (rp . parseLit)

-- parsePhraseList_aux0 :: [ParserMarkUp Text] -> Parser (Maybe [Text])
-- parsePhraseList_aux0 ph = case ph of
--   [] -> return Nothing
--   x:xs -> (maybe'Lit x) <+> (parsePhraseList_aux0 xs)

-- parsePhraseList_aux :: [[ParserMarkUp Text]] -> Parser (Maybe [Text])
-- parsePhraseList_aux phs = case phs of
--   [] -> empty
--   x:xs -> (parsePhraseList_aux0 x) <||> (parsePhraseList_aux xs)

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
  
parsePhraseListFiller_aux :: Parser [Text] -- note(jesse): maybe make this [Maybe Text] for easier debugging?
parsePhraseListFiller_aux =
  delete_nothings <$> ((use $ allStates primPhraseListFiller) >>= parsePhraseList . concat)

  -- (use (top . primPhraseListFiller) >>= parsePhraseList) -- TODO(jesse) fix this, use top . isn't right

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
                         -- TODO (jesse): add support for parsing alternation lists
                         -- TODO (jesse): integrate this into the state for on-the-fly extension of the phrase lists
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
