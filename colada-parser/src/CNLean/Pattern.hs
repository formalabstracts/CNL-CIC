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

import Control.Lens

import CNLean.Basic.Basic

patternOfOption :: (a -> Parser Pattern) -> (Maybe a -> Parser Pattern)
patternOfOption p mx =
  case mx of
    Nothing -> return . Patts $ []
    Just x -> p x

patternOfList :: (a -> Parser Pattern) -> ([a] -> Parser Pattern)
patternOfList m xs = case xs of
  [] -> return . Patts $ []
  y:ys -> (<>) <$> m y <*> patternOfList m ys

patternOfAtomicId :: AtomicId -> Parser Pattern
patternOfAtomicId atomicid@(AtomicId txt) = return . Patts . pure . Wd . pure $ txt

patternOfHierId :: HierId -> Parser Pattern
patternOfHierId hierid@(HierId aids mvn) =
  return . Patts . pure . Wd . pure $ join $ intersperse "." (map (\(AtomicId txt) -> txt) aids)

patternOfVar :: Var -> Parser Pattern
patternOfVar v = return . Patts $ [Vr]

patternOfSymbol :: Symbol -> Parser Pattern
patternOfSymbol (Symbol txt) = return . Patts . pure . Sm $ txt

patternOfControlSequence :: ControlSequence -> (Pattern -> Parser Pattern)
patternOfControlSequence (ControlSequence txt) args = return . Patts . pure $ CSeq txt (args^.patts :: [Patt])
