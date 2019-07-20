{-
Author(s): Jesse Michael Han (2019)

Parsing assumptions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Assumption where

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

import CNLean.Basic
import CNLean.Token
import CNLean.Annotation

data Assumption = -- note: parsing a list of assumptions means using (many1 parseAssumption)
    AssumptionAssumptionPrefix AssumptionPrefix Statement -- parsed with period at end
  | AssumptionLetAnnotation LetAnnotation -- parsed with period at end
  deriving (Show, Eq)
  
data AssumptionPrefix =
    LitLet Token -- LIT_LET
  | LitLets [Token] Token (Maybe Token) -- lit_lets lit_assume option(LIT_THAT) {}
  deriving (Show, Eq)

parseAssumptionPrefix :: Parser AssumptionPrefix
parseAssumptionPrefix = (do tks <- parseLitLets
                            tk  <- parseLitAssume
                            o   <- option $ parseLit_aux THAT >>= return . Lit
                            return $ LitLets tks tk o)
                      <||> (parseLit_aux LET >>= return . LitLet . Lit)

newtype ThenPrefix = ThenPrefix (Maybe Token) --   then_prefix : option(lit_then) {}

parseThenPrefix :: Parser ThenPrefix
parseThenPrefix = option $ parseLit_aux THEN >>= return . Lit



