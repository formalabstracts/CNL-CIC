{-
Author(s): Jesse Michael Han (2019)

Patterns.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Basic.Pattern where

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

import CNLean.Basic.Core
import CNLean.Basic.State
import CNLean.Basic.Token

data ParsedPatt =
    ParsedWd Token -- note, each 
  | ParsedSymbol Symbol
  | ParsedVar Var
  | ParsedName [Var]
  deriving (Show, Eq)

parsePatt :: Patt -> Parser ParsedPatt
parsePatt ptt = case ptt of
  Nm -> ParsedName <$> (sepby1 parseVar parseComma)
  Wd ts -> ParsedWd <$> parse_any_of (map parseTokenOfLit ts)
  Sm t -> do s <- parseSymbol
             guard (s == Symbol t)
             return $ ParsedSymbol s             
  Vr   -> ParsedVar <$> parseVar

parsePatts :: [Patt] -> Parser [ParsedPatt]
parsePatts ptts = parse_list ptts parsePatt

parse_any_Patts :: [[Patt]] -> Parser [ParsedPatt]
parse_any_Patts = parse_any $ rp . parsePatt
-- note: this is equivalent to parse_any_of . map parsePatts

examplePatts :: [Patt]
examplePatts = [Wd ["foo"], Wd ["bar"], Vr]

examplePatts2 :: [Patt]
examplePatts2 = [Wd["subsets", "subset"], Nm, Wd["of"], Vr]
-- note, prefixes must appear later in the list because they will succeed first

-- test (parsePatts examplePatts) "foo bar a1"
-- test (parsePatts examplePatts2) "subset a of x0"
-- test (parsePatts examplePatts2) "subsets a,b of x0"
