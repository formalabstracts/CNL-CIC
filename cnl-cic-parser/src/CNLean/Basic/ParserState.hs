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

updatePrimIdentifierTerm :: [Patt] -> Parser ()
updatePrimIdentifierTerm txts = top . primIdentifierTerm %= (:) txts

updatePrimIdentifierTerm2 :: [[Patt]] -> Parser ()
updatePrimIdentifierTerm2 txtss = top . primIdentifierTerm %= (<>) txtss

updateGlobalPrimIdentifierTerm :: [Patt] -> Parser ()
updateGlobalPrimIdentifierTerm txts = updateGlobal $ primIdentifierTerm %~ (:) txts

updatePrimTermControlSeq :: [Patt] -> Parser ()
updatePrimTermControlSeq txts = top . primTermControlSeq %= (:) txts

updatePrimTermControlSeq2 :: [[Patt]] -> Parser ()
updatePrimTermControlSeq2 txtss = top . primTermControlSeq %= (<>) txtss

updateGlobalPrimTermControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTermControlSeq txts = updateGlobal $ primTermControlSeq %~ (:) txts

updatePrimTermOpControlSeq :: [Patt] -> Parser ()
updatePrimTermOpControlSeq txts = top . primTermOpControlSeq %= (:) txts

updatePrimTermOpControlSeq2 :: [[Patt]] -> Parser ()
updatePrimTermOpControlSeq2 txtss = top . primTermOpControlSeq %= (<>) txtss

updateGlobalPrimTermOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTermOpControlSeq txts = updateGlobal $ primTermOpControlSeq %~ (:) txts

-- updatePrimPrefixFunction :: [Patt] -> Parser ()
-- updatePrimPrefixFunction txts = top . primPrefixFunction %= (:) txts

-- updatePrimPrefixFunction2 :: [[Patt]] -> Parser ()
-- updatePrimPrefixFunction2 txtss = top . primPrefixFunction %= (<>) txtss

-- updateGlobalPrimPrefixFunction :: [Patt] -> Parser ()
-- updateGlobalPrimPrefixFunction txts = updateGlobal $ primPrefixFunction %~ (:) txts

updatePrimAdjective :: [Patt] -> Parser ()
updatePrimAdjective txts = top . primAdjective %= (:) txts

updatePrimAdjective2 :: [[Patt]] -> Parser ()
updatePrimAdjective2 txtss = top . primAdjective %= (<>) txtss

updateGlobalPrimAdjective :: [Patt] -> Parser ()
updateGlobalPrimAdjective txts = updateGlobal $ primAdjective %~ (:) txts

updatePrimAdjectiveMultiSubject :: [Patt] -> Parser ()
updatePrimAdjectiveMultiSubject txts = top . primAdjectiveMultiSubject %= (:) txts

updatePrimAdjectiveMultiSubject2 :: [[Patt]] -> Parser ()
updatePrimAdjectiveMultiSubject2 txtss = top . primAdjectiveMultiSubject %= (<>) txtss

updateGlobalPrimAdjectiveMultiSubject :: [Patt] -> Parser ()
updateGlobalPrimAdjectiveMultiSubject txts = updateGlobal $ primAdjectiveMultiSubject %~ (:) txts

updatePrimVerb :: [Patt] -> Parser ()
updatePrimVerb txts = top . primVerb %= (:) txts

updatePrimVerb2 :: [[Patt]] -> Parser ()
updatePrimVerb2 txtss = top . primVerb %= (<>) txtss

updateGlobalPrimVerb :: [Patt] -> Parser ()
updateGlobalPrimVerb txts = updateGlobal $ primVerb %~ (:) txts

updatePrimVerbMultiSubject :: [Patt] -> Parser ()
updatePrimVerbMultiSubject txts = top . primVerbMultiSubject %= (:) txts

updatePrimVerbMultiSubject2 :: [[Patt]] -> Parser ()
updatePrimVerbMultiSubject2 txtss = top . primVerbMultiSubject %= (<>) txtss

updateGlobalPrimVerbMultiSubject :: [Patt] -> Parser ()
updateGlobalPrimVerbMultiSubject txts = updateGlobal $ primVerbMultiSubject %~ (:) txts

updatePrimRelation :: [Patt] -> Parser ()
updatePrimRelation txts = top . primRelation %= (:) txts

updatePrimRelation2 :: [[Patt]] -> Parser ()
updatePrimRelation2 txtss = top . primRelation %= (<>) txtss

updateGlobalPrimRelation :: [Patt] -> Parser ()
updateGlobalPrimRelation txts = updateGlobal $ primRelation %~ (:) txts

updatePrimPropositionalOp :: [Patt] -> Parser ()
updatePrimPropositionalOp txts = top . primPropositionalOp %= (:) txts

updatePrimPropositionalOp2 :: [[Patt]] -> Parser ()
updatePrimPropositionalOp2 txtss = top . primPropositionalOp %= (<>) txtss

updateGlobalPrimPropositionalOp :: [Patt] -> Parser ()
updateGlobalPrimPropositionalOp txts = updateGlobal $ primPropositionalOp %~ (:) txts

updatePrimBinaryRelationOp :: [Patt] -> Parser ()
updatePrimBinaryRelationOp txts = top . primBinaryRelationOp %= (:) txts

updatePrimBinaryRelationOp2 :: [[Patt]] -> Parser ()
updatePrimBinaryRelationOp2 txtss = top . primBinaryRelationOp %= (<>) txtss

updateGlobalPrimBinaryRelationOp :: [Patt] -> Parser ()
updateGlobalPrimBinaryRelationOp txts = updateGlobal $ primBinaryRelationOp %~ (:) txts

updatePrimBinaryRelationControlSeq :: [Patt] -> Parser ()
updatePrimBinaryRelationControlSeq txts = top . primBinaryRelationControlSeq %= (:) txts

updatePrimBinaryRelationControlSeq2 :: [[Patt]] -> Parser ()
updatePrimBinaryRelationControlSeq2 txtss = top . primBinaryRelationControlSeq %= (<>) txtss

updateGlobalPrimBinaryRelationControlSeq :: [Patt] -> Parser ()
updateGlobalPrimBinaryRelationControlSeq txts = updateGlobal $ primBinaryRelationControlSeq %~ (:) txts

updatePrimPropositionalOpControlSeq :: [Patt] -> Parser ()
updatePrimPropositionalOpControlSeq txts = top . primPropositionalOpControlSeq %= (:) txts

updatePrimPropositionalOpControlSeq2 :: [[Patt]] -> Parser ()
updatePrimPropositionalOpControlSeq2 txtss = top . primPropositionalOpControlSeq %= (<>) txtss

updateGlobalPrimPropositionalOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimPropositionalOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimIdentifierType :: [Patt] -> Parser ()
updatePrimIdentifierType txts = top . primPropositionalOpControlSeq %= (:) txts

updatePrimIdentifierType2 :: [[Patt]] -> Parser ()
updatePrimIdentifierType2 txtss = top . primPropositionalOpControlSeq %= (<>) txtss

updateGlobalPrimIdentifierType :: [Patt] -> Parser ()
updateGlobalPrimIdentifierType txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimTypeOp :: [Patt] -> Parser ()
updatePrimTypeOp txts = top . primPropositionalOpControlSeq %= (:) txts

updatePrimTypeOp2 :: [[Patt]] -> Parser ()
updatePrimTypeOp2 txtss = top . primPropositionalOpControlSeq %= (<>) txtss

updateGlobalPrimTypeOp :: [Patt] -> Parser ()
updateGlobalPrimTypeOp txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimTypeOpControlSeq :: [Patt] -> Parser ()
updatePrimTypeOpControlSeq txts = top . primPropositionalOpControlSeq %= (:) txts

updatePrimTypeOpControlSeq2 :: [[Patt]] -> Parser ()
updatePrimTypeOpControlSeq2 txtss = top . primPropositionalOpControlSeq %= (<>) txtss

updateGlobalPrimTypeOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTypeOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimTypeControlSeq :: [Patt] -> Parser ()
updatePrimTypeControlSeq txts = top . primPropositionalOpControlSeq %= (:) txts

updatePrimTypeControlSeq2 :: [[Patt]] -> Parser ()
updatePrimTypeControlSeq2 txtss = top . primPropositionalOpControlSeq %= (<>) txtss

updateGlobalPrimTypeControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTypeControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

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

