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
import Text.Megaparsec hiding (Token, Label, option, Tokens)
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack, toLower)
import Data.Void
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy (modify, gets)
import qualified Data.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Lens hiding (element)
import Control.Lens.TH

import Language.Haskell.TH

import CNLean.Basic.Core
import CNLean.Basic.State
import CNLean.Basic.Token

$(makeLenses ''FState)

$(makeLenses ''Stack)

updateGlobal :: (FState -> FState) -> Parser ()
updateGlobal f =
  (rest %= \fs -> (fs & _last %~ f)) <||> -- if the first branch fails, then rest is empty, so modify top instead
  (top %= f)

--TODO(jesse): possibly make updates global only, except when parsing macros

updatePrimPrecTable :: [Patt] -> Int -> AssociativeParity -> Parser ()
updatePrimPrecTable ptts level parity = top . primPrecTable %= M.insert ptts (level, parity)

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

updateIdCount :: Int -> Parser ()
updateIdCount k = top . idCount %= (const k)

allStates :: Getter FState a -> Getter (Stack FState) [a]
allStates g = to $ \stk -> stk^..(top . g) <> (stk^..(rest . traverse . g))

---- returns the current document level of the parser
parserDocumentLevel :: Parser Int
parserDocumentLevel = depthStack <$> get

tokenToText'_aux :: [[Text]] -> Token -> [Text]
tokenToText'_aux strsyms tk = case tk of
  Token txt -> case (filter (elem txt) strsyms) of
    [] -> [txt]
    (x:xs) -> ((<>) x $ concat xs)

-- tokenToText' extracts the underlying Text of a token and adds all available synonyms from the state.
tokenToText' :: Token -> Parser [Text]
tokenToText' tk = tokenToText'_aux <$> (concat <$> (use (allStates strSyms))) <*> return tk

