{-
Author(s): Jesse Michael Han (2019)

Parsing annotations.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Axiom where

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

newtype LetAnnotation = LetAnnotation [AnnotatedVars] -- LIT_LET comma_nonempty_list(annotated_vars)

data AnnotatedVars = AnnotatedVars {varModifier :: VarModifier, vars :: [Token], maybeColonType :: (Maybe ColonType)}

newtype VarModifier = VarModifier (Maybe Token)

parseVarModifier :: Parser VarModifier
parseVarModifier = option (parseLit_aux FIXED <||> parseLit_aux IMPLICIT <||> parseLit_aux RESOLVED <||> parseLit_aux REMOVE >>= return . Lit) >>= return . VarModifier

