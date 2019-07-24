{-
Author(s): Jesse Michael Han (2019)

Parsing annotations.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Annotation where

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

newtype LetAnnotation = LetAnnotation [AnnotatedVars] -- LIT_LET comma_nonempty_list(annotated_vars)
  deriving (Show, Eq)

data AnnotatedVars = AnnotatedVars {varModifier :: VarModifier, vars :: [Var]-- , maybeColonType :: (Maybe ColonType)
                                   } deriving (Show, Eq)

newtype VarModifier = VarModifier (Maybe [Text])
  deriving (Show, Eq)

-- parseVarModifier :: Parser VarModifier
-- parseVarModifier = option (parseLit_aux FIXED <||> parseLit_aux IMPLICIT <||> parseLit_aux RESOLVED <||> parseLit_aux REMOVE >>= return . Lit) >>= return . VarModifier

parseAnnotatedVars :: Parser AnnotatedVars
parseAnnotatedVars = between parseLParen parseRParen $
  do varmod <- option parseLitVarMod
     vs     <- (many1' parseVar)
     return $ AnnotatedVars (VarModifier varmod) vs

parseLetAnnotation :: Parser [AnnotatedVars]
parseLetAnnotation = parseLit "let" *> (sepby1 parseAnnotatedVars parseComma)


-- note, we still haven't defined colontypes yet.
