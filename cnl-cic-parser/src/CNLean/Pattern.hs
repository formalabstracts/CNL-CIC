{-
Author(s): Jesse Michael Han (2019)

Utilities for parsing patterns
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Pattern where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, option, Label, Tokens)
import Control.Monad (guard, liftM)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.List (intersperse)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Monad.Trans.State.Lazy (modify, gets)

import CNLean.Basic.Basic

patternOfOption :: (a -> Parser [Patt]) -> (Maybe a -> Parser [Patt])
patternOfOption p mx =
  case mx of
    Nothing -> return []
    Just x -> p x

patternOfList :: (a -> Parser [Patt]) -> ([a] -> Parser [Patt])
patternOfList m xs = case xs of
  [] -> return []
  y:ys -> m y <+> patternOfList m ys

patternOfAtomicId :: AtomicId -> Parser [Patt]
patternOfAtomicId atomicid@(AtomicId txt) = return . pure . Wd . pure $ txt

patternOfHierId :: HierId -> Parser [Patt]
patternOfHierId hierid@(HierId aids mvn) =
  return . pure . Wd . pure $ join $ intersperse "." (map (\(AtomicId txt) -> txt) aids)

patternOfVar :: Var -> Parser [Patt]
patternOfVar v = return [Vr]

patternOfSymbol :: Symbol -> Parser [Patt]
patternOfSymbol (Symbol txt) = return . pure . Sm $ txt

patternOfControlSequence :: ControlSequence -> ([Patt] -> Parser [Patt])
patternOfControlSequence (ControlSequence txt) args = return . pure $ CSeq txt args

