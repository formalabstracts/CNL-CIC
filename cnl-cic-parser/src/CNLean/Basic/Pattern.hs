{-
Author(s): Jesse Michael Han (2019)

Patterns
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

import CNLean.Basic.Basic
import CNLean.Basic.State
import CNLean.Basic.Token

data ParsedPatt =
    ParsedWd [Token]
  | ParsedSymbol Symbol
  | ParsedVar Var
  | ParsedName [Var]
  deriving (Show, Eq)

parsePatt :: Patt -> Parser ParsedPatt
parsePatt ptt = case ptt of
  Wd ts -> parse_list ts parseTokenOfLit >>= return . ParsedWd
  Sm t -> do s <- parseSymbol
             guard (s == Symbol t)
             return $ ParsedSymbol s             
  Vr   -> parseVar >>= return . ParsedVar

parsePatts :: [Patt] -> Parser [ParsedPatt]
parsePatts ptts = parse_list ptts parsePatt

--tests

examplePatts :: [Patt]
examplePatts = [Wd ["foo"], Wd ["bar"], Vr]

-- test (parsePatts examplePatts) "foo bar a1"

-- result:
-- [ParsedWd [Token "foo"],ParsedWd [Token "bar"],ParsedVar (Var "a1")]
