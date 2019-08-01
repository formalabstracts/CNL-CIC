{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-
Author(s): Jesse Michael Han (2019)

Managing the parser state.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CNLean.Basic.ParserState where

import Prelude
import Control.Monad.Trans.State
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack, toLower)
import Data.Void
import Control.Monad.Trans.State.Lazy (modify, gets)
import qualified Data.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Lens hiding (element)
import Control.Lens.TH

import Language.Haskell.TH

import CNLean.Basic.Core
import CNLean.Basic.State

$(makeLenses ''FState)

$(makeLenses ''Stack)

updateGlobal :: (FState -> FState) -> Parser ()
updateGlobal f =
  (rest %= \fs -> (fs & _last %~ f)) <||> -- if the first branch fails, then rest is empty, so modify top instead
  (top %= f)

updateClsList :: [Text] -> Parser ()
updateClsList txts = top . clsList %= (:) txts

updateClsList2 :: [[Text]] -> Parser ()
updateClsList2 txtss = top . clsList %= (<>) txtss

updateGlobalClsList :: [Text] -> Parser ()
updateGlobalClsList txts = updateGlobal $ clsList %~ (:) txts

updateStrSyms :: [Text] -> Parser ()
updateStrSyms txts = top . strSyms %= (:) txts

updateStrSyms2 :: [[Text]] -> Parser ()
updateStrSyms2 txtss = top . strSyms %= (<>) txtss

updateGlobalStrSyms :: [Text] -> Parser ()
updateGlobalStrSyms txts = updateGlobal $ strSyms %~ (:) txts

updatePrimDefiniteNoun :: [Patt] -> Parser ()
updatePrimDefiniteNoun txts = top . primDefiniteNoun %= (:) txts

updatePrimDefiniteNoun2 :: [[Patt]] -> Parser ()
updatePrimDefiniteNoun2 txtss = top . primDefiniteNoun %= (<>) txtss

updateGlobalPrimDefiniteNoun :: [Patt] -> Parser ()
updateGlobalPrimDefiniteNoun txts = updateGlobal $ primDefiniteNoun %~ (:) txts

allStates :: Getter FState a -> Getter (Stack FState) [a]
allStates g = to $ \stk -> stk^..(top . g) <> (stk^..(rest . traverse . g))
