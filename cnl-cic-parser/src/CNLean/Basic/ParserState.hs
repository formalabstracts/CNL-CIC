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

import Control.Lens
import Control.Lens.TH

import Language.Haskell.TH

import CNLean.Basic.Core
import CNLean.Basic.State
import CNLean.Basic.Token

$(makeLenses ''FState)

$(makeLenses ''Stack)

data LocalGlobalFlag =
    Globally
  | Locally
  | AtLevel Int
  deriving (Show, Eq)

updateGlobal :: (FState -> FState) -> Parser ()
updateGlobal f =
  (rest %= \fs -> (fs & _last %~ f)) <||> -- if the first branch fails, then rest is empty, so modify top instead
  (top %= f)

updateLocal :: (FState -> FState) -> Parser ()
updateLocal f =
  (top %= f)

updateAtLevel :: Int -> (FState -> FState) -> Parser ()
updateAtLevel k f = do
  d <- depthStack <$> get
  if k > d + 1
    then fail "stack index out of bounds"
    else if k == d + 1
            then updateLocal f
            else (rest . element k %= f)    

updateGlobalPrimPrecTable :: [Patt] -> Int -> AssociativeParity -> Parser ()
updateGlobalPrimPrecTable ptts level parity = updateGlobal $ primPrecTable %~ M.insert ptts (level, parity)

updateGlobalClsList :: [Text] -> Parser ()
updateGlobalClsList txts = updateGlobal $ clsList %~ (:) txts

updateGlobalClsList2 :: [[Text]] -> Parser ()
updateGlobalClsList2 txtss = updateGlobal $ clsList %~ (<>) txtss

updateLocalClsList :: [Text] -> Parser ()
updateLocalClsList txts = updateLocal $ clsList %~ (:) txts

updateAtLevelClsList :: Int -> [Text] -> Parser ()
updateAtLevelClsList k txts = updateAtLevel k $ clsList %~ (:) txts

updateClsList :: LocalGlobalFlag -> [Text] -> Parser ()
updateClsList b args =
  case b of
    Globally -> updateGlobalClsList args
    Locally -> updateLocalClsList args
    AtLevel k -> updateAtLevelClsList k args

updateGlobalStrSyms :: [Text] -> Parser ()
updateGlobalStrSyms txts = updateGlobal $ strSyms %~ (:) txts

updateGlobalStrSyms2 :: [[Text]] -> Parser ()
updateGlobalStrSyms2 txtss = updateGlobal $ strSyms %~ (<>) txtss

updateLocalStrSyms :: [Text] -> Parser ()
updateLocalStrSyms txts = updateLocal $ strSyms %~ (:) txts

updateAtLevelStrSyms :: Int -> [Text] -> Parser ()
updateAtLevelStrSyms k txts = updateAtLevel k $ strSyms %~ (:) txts

updateStrSyms :: LocalGlobalFlag -> [Text] -> Parser ()
updateStrSyms b args =
  case b of
    Globally -> updateGlobalStrSyms args
    Locally -> updateLocalStrSyms args
    AtLevel k -> updateAtLevelStrSyms k args

updateGlobalPrimDefiniteNoun :: [Patt] -> Parser ()
updateGlobalPrimDefiniteNoun txts = updateGlobal $ primDefiniteNoun %~ (:) txts

updateGlobalPrimDefiniteNoun2 :: [[Patt]] -> Parser ()
updateGlobalPrimDefiniteNoun2 txtss = updateGlobal $ primDefiniteNoun %~ (<>) txtss

updateLocalPrimDefiniteNoun :: [Patt] -> Parser ()
updateLocalPrimDefiniteNoun txts = updateLocal $ primDefiniteNoun %~ (:) txts

updateAtLevelPrimDefiniteNoun :: Int -> [Patt] -> Parser ()
updateAtLevelPrimDefiniteNoun k txts = updateAtLevel k $ primDefiniteNoun %~ (:) txts

updatePrimDefiniteNoun :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimDefiniteNoun b args =
  case b of
    Globally -> updateGlobalPrimDefiniteNoun args
    Locally -> updateLocalPrimDefiniteNoun args
    AtLevel k -> updateAtLevelPrimDefiniteNoun k args

updateGlobalPrimIdentifierTerm :: [Patt] -> Parser ()
updateGlobalPrimIdentifierTerm txts = updateGlobal $ primIdentifierTerm %~ (:) txts

updateGlobalPrimIdentifierTerm2 :: [[Patt]] -> Parser ()
updateGlobalPrimIdentifierTerm2 txtss = updateGlobal $ primIdentifierTerm %~ (<>) txtss

updateLocalPrimIdentifierTerm :: [Patt] -> Parser ()
updateLocalPrimIdentifierTerm txts = updateLocal $ primIdentifierTerm %~ (:) txts

updateAtLevelPrimIdentifierTerm :: Int -> [Patt] -> Parser ()
updateAtLevelPrimIdentifierTerm k txts = updateAtLevel k $ primIdentifierTerm %~ (:) txts

updatePrimIdentifierTerm :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimIdentifierTerm b args =
  case b of
    Globally -> updateGlobalPrimIdentifierTerm args
    Locally -> updateLocalPrimIdentifierTerm args
    AtLevel k -> updateAtLevelPrimIdentifierTerm k args

updateGlobalPrimTermControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTermControlSeq txts = updateGlobal $ primTermControlSeq %~ (:) txts

updateGlobalPrimTermControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTermControlSeq2 txtss = updateGlobal $ primTermControlSeq %~ (<>) txtss

updateLocalPrimTermControlSeq :: [Patt] -> Parser ()
updateLocalPrimTermControlSeq txts = updateLocal $ primTermControlSeq %~ (:) txts

updateAtLevelPrimTermControlSeq :: Int -> [Patt] -> Parser ()
updateAtLevelPrimTermControlSeq k txts = updateAtLevel k $ primTermControlSeq %~ (:) txts

updatePrimTermControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTermControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTermControlSeq args
    Locally -> updateLocalPrimTermControlSeq args
    AtLevel k -> updateAtLevelPrimTermControlSeq k args

updateGlobalPrimTermOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTermOpControlSeq txts = updateGlobal $ primTermOpControlSeq %~ (:) txts

updateGlobalPrimTermOpControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTermOpControlSeq2 txtss = updateGlobal $ primTermOpControlSeq %~ (<>) txtss

updateLocalPrimTermOpControlSeq :: [Patt] -> Parser ()
updateLocalPrimTermOpControlSeq txts = updateLocal $ primTermOpControlSeq %~ (:) txts

updateAtLevelPrimTermOpControlSeq :: Int -> [Patt] -> Parser ()
updateAtLevelPrimTermOpControlSeq k txts = updateAtLevel k $ primTermOpControlSeq %~ (:) txts

updatePrimTermOpControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTermOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTermOpControlSeq args
    Locally -> updateLocalPrimTermOpControlSeq args
    AtLevel k -> updateAtLevelPrimTermOpControlSeq k args

-- updateGlobalPrimPrefixFunction :: [Patt] -> Parser ()
-- updGlobalatePrimPrefixFunction txts = updateLocal $ primPrefixFunction %= (:) txts
-- updateGlobalPrimPrefixFunction2 :: [[Patt]] -> Parser ()
-- updGlobalatePrimPrefixFunction2 txtss = updateGlobal $ primPrefixFunction %= (<>) txtss

-- updateLocalPrimPrefixFunction :: [Patt] -> Parser ()
-- updateLocalPrimPrefixFunction txts = updateLocal $ primPrefixFunction %~ (:) txts

updateGlobalPrimAdjective :: [Patt] -> Parser ()
updateGlobalPrimAdjective txts = updateGlobal $ primAdjective %~ (:) txts

updateGlobalPrimAdjective2 :: [[Patt]] -> Parser ()
updateGlobalPrimAdjective2 txtss = updateGlobal $ primAdjective %~ (<>) txtss

updateLocalPrimAdjective :: [Patt] -> Parser ()
updateLocalPrimAdjective txts = updateLocal $ primAdjective %~ (:) txts

updateAtLevelPrimAdjective :: Int -> [Patt] -> Parser ()
updateAtLevelPrimAdjective k txts = updateAtLevel k $ primAdjective %~ (:) txts

updatePrimAdjective :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimAdjective b args =
  case b of
    Globally -> updateGlobalPrimAdjective args
    Locally -> updateLocalPrimAdjective args
    AtLevel k -> updateAtLevelPrimAdjective k args

updateGlobalPrimAdjectiveMultiSubject :: [Patt] -> Parser ()
updateGlobalPrimAdjectiveMultiSubject txts = updateGlobal $ primAdjectiveMultiSubject %~ (:) txts

updateGlobalPrimAdjectiveMultiSubject2 :: [[Patt]] -> Parser ()
updateGlobalPrimAdjectiveMultiSubject2 txtss = updateGlobal $ primAdjectiveMultiSubject %~ (<>) txtss

updateLocalPrimAdjectiveMultiSubject :: [Patt] -> Parser ()
updateLocalPrimAdjectiveMultiSubject txts = updateLocal $ primAdjectiveMultiSubject %~ (:) txts

updateAtLevelPrimAdjectiveMultiSubject :: Int -> [Patt] -> Parser ()
updateAtLevelPrimAdjectiveMultiSubject k txts = updateAtLevel k $ primAdjectiveMultiSubject %~ (:) txts

updatePrimAdjectiveMultiSubject :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimAdjectiveMultiSubject b args =
  case b of
    Globally -> updateGlobalPrimAdjectiveMultiSubject args
    Locally -> updateLocalPrimAdjectiveMultiSubject args
    AtLevel k -> updateAtLevelPrimAdjectiveMultiSubject k args

updateGlobalPrimVerb :: [Patt] -> Parser ()
updateGlobalPrimVerb txts = updateGlobal $ primVerb %~ (:) txts

updateGlobalPrimVerb2 :: [[Patt]] -> Parser ()
updateGlobalPrimVerb2 txtss = updateGlobal $ primVerb %~ (<>) txtss

updateLocalPrimVerb :: [Patt] -> Parser ()
updateLocalPrimVerb txts = updateLocal $ primVerb %~ (:) txts

updateAtLevelPrimVerb :: Int -> [Patt] -> Parser ()
updateAtLevelPrimVerb k txts = updateAtLevel k $ primVerb %~ (:) txts

updatePrimVerb :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimVerb b args =
  case b of
    Globally -> updateGlobalPrimVerb args
    Locally -> updateLocalPrimVerb args
    AtLevel k -> updateAtLevelPrimVerb k args

updateGlobalPrimVerbMultiSubject :: [Patt] -> Parser ()
updateGlobalPrimVerbMultiSubject txts = updateGlobal $ primVerbMultiSubject %~ (:) txts

updateGlobalPrimVerbMultiSubject2 :: [[Patt]] -> Parser ()
updateGlobalPrimVerbMultiSubject2 txtss = updateGlobal $ primVerbMultiSubject %~ (<>) txtss

updateLocalPrimVerbMultiSubject :: [Patt] -> Parser ()
updateLocalPrimVerbMultiSubject txts = updateLocal $ primVerbMultiSubject %~ (:) txts

updateAtLevelPrimVerbMultiSubject :: Int -> [Patt] -> Parser ()
updateAtLevelPrimVerbMultiSubject k txts = updateAtLevel k $ primVerbMultiSubject %~ (:) txts

updatePrimVerbMultiSubject :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimVerbMultiSubject b args =
  case b of
    Globally -> updateGlobalPrimVerbMultiSubject args
    Locally -> updateLocalPrimVerbMultiSubject args
    AtLevel k -> updateAtLevelPrimVerbMultiSubject k args

updateGlobalPrimRelation :: [Patt] -> Parser ()
updateGlobalPrimRelation txts = updateGlobal $ primRelation %~ (:) txts

updateGlobalPrimRelation2 :: [[Patt]] -> Parser ()
updateGlobalPrimRelation2 txtss = updateGlobal $ primRelation %~ (<>) txtss

updateLocalPrimRelation :: [Patt] -> Parser ()
updateLocalPrimRelation txts = updateLocal $ primRelation %~ (:) txts

updateAtLevelPrimRelation :: Int -> [Patt] -> Parser ()
updateAtLevelPrimRelation k txts = updateAtLevel k $ primRelation %~ (:) txts

updatePrimRelation :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimRelation b args =
  case b of
    Globally -> updateGlobalPrimRelation args
    Locally -> updateLocalPrimRelation args
    AtLevel k -> updateAtLevelPrimRelation k args

updateGlobalPrimPropositionalOp :: [Patt] -> Parser ()
updateGlobalPrimPropositionalOp txts = updateGlobal $ primPropositionalOp %~ (:) txts

updateGlobalPrimPropositionalOp2 :: [[Patt]] -> Parser ()
updateGlobalPrimPropositionalOp2 txtss = updateGlobal $ primPropositionalOp %~ (<>) txtss

updateLocalPrimPropositionalOp :: [Patt] -> Parser ()
updateLocalPrimPropositionalOp txts = updateLocal $ primPropositionalOp %~ (:) txts

updateAtLevelPrimPropositionalOp :: Int -> [Patt] -> Parser ()
updateAtLevelPrimPropositionalOp k txts = updateAtLevel k $ primPropositionalOp %~ (:) txts

updatePrimPropositionalOp :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimPropositionalOp b args =
  case b of
    Globally -> updateGlobalPrimPropositionalOp args
    Locally -> updateLocalPrimPropositionalOp args
    AtLevel k -> updateAtLevelPrimPropositionalOp k args

updateGlobalPrimBinaryRelationOp :: [Patt] -> Parser ()
updateGlobalPrimBinaryRelationOp txts = updateGlobal $ primBinaryRelationOp %~ (:) txts

updateGlobalPrimBinaryRelationOp2 :: [[Patt]] -> Parser ()
updateGlobalPrimBinaryRelationOp2 txtss = updateGlobal $ primBinaryRelationOp %~ (<>) txtss

updateLocalPrimBinaryRelationOp :: [Patt] -> Parser ()
updateLocalPrimBinaryRelationOp txts = updateLocal $ primBinaryRelationOp %~ (:) txts

updateAtLevelPrimBinaryRelationOp :: Int -> [Patt] -> Parser ()
updateAtLevelPrimBinaryRelationOp k txts = updateAtLevel k $ primBinaryRelationOp %~ (:) txts

updatePrimBinaryRelationOp :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimBinaryRelationOp b args =
  case b of
    Globally -> updateGlobalPrimBinaryRelationOp args
    Locally -> updateLocalPrimBinaryRelationOp args
    AtLevel k -> updateAtLevelPrimBinaryRelationOp k args

updateGlobalPrimBinaryRelationControlSeq :: [Patt] -> Parser ()
updateGlobalPrimBinaryRelationControlSeq txts = updateGlobal $ primBinaryRelationControlSeq %~ (:) txts

updateGlobalPrimBinaryRelationControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimBinaryRelationControlSeq2 txtss = updateGlobal $ primBinaryRelationControlSeq %~ (<>) txtss

updateLocalPrimBinaryRelationControlSeq :: [Patt] -> Parser ()
updateLocalPrimBinaryRelationControlSeq txts = updateLocal $ primBinaryRelationControlSeq %~ (:) txts

updateAtLevelPrimBinaryRelationControlSeq :: Int -> [Patt] -> Parser ()
updateAtLevelPrimBinaryRelationControlSeq k txts = updateAtLevel k $ primBinaryRelationControlSeq %~ (:) txts

updatePrimBinaryRelationControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimBinaryRelationControlSeq b args =
  case b of
    Globally -> updateGlobalPrimBinaryRelationControlSeq args
    Locally -> updateLocalPrimBinaryRelationControlSeq args
    AtLevel k -> updateAtLevelPrimBinaryRelationControlSeq k args

updateGlobalPrimPropositionalOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimPropositionalOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimPropositionalOpControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimPropositionalOpControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimPropositionalOpControlSeq :: [Patt] -> Parser ()
updateLocalPrimPropositionalOpControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimPropositionalOpControlSeq :: Int -> [Patt] -> Parser ()
updateAtLevelPrimPropositionalOpControlSeq k txts = updateAtLevel k $ primPropositionalOpControlSeq %~ (:) txts

updatePrimPropositionalOpControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimPropositionalOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimPropositionalOpControlSeq args
    Locally -> updateLocalPrimPropositionalOpControlSeq args
    AtLevel k -> updateAtLevelPrimPropositionalOpControlSeq k args

updateGlobalPrimIdentifierType :: [Patt] -> Parser ()
updateGlobalPrimIdentifierType txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimIdentifierType2 :: [[Patt]] -> Parser ()
updateGlobalPrimIdentifierType2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimIdentifierType :: [Patt] -> Parser ()
updateLocalPrimIdentifierType txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimIdentifierType :: Int -> [Patt] -> Parser ()
updateAtLevelPrimIdentifierType k txts = updateAtLevel k $ primIdentifierType %~ (:) txts

updatePrimIdentifierType :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimIdentifierType b args =
  case b of
    Globally -> updateGlobalPrimIdentifierType args
    Locally -> updateLocalPrimIdentifierType args
    AtLevel k -> updateAtLevelPrimIdentifierType k args

updateGlobalPrimTypeOp :: [Patt] -> Parser ()
updateGlobalPrimTypeOp txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeOp2 :: [[Patt]] -> Parser ()
updateGlobalPrimTypeOp2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeOp :: [Patt] -> Parser ()
updateLocalPrimTypeOp txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimTypeOp :: Int -> [Patt] -> Parser ()
updateAtLevelPrimTypeOp k txts = updateAtLevel k $ primTypeOp %~ (:) txts

updatePrimTypeOp :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTypeOp b args =
  case b of
    Globally -> updateGlobalPrimTypeOp args
    Locally -> updateLocalPrimTypeOp args
    AtLevel k -> updateAtLevelPrimTypeOp k args

updateGlobalPrimTypeOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTypeOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeOpControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTypeOpControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeOpControlSeq :: [Patt] -> Parser ()
updateLocalPrimTypeOpControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimTypeOpControlSeq :: Int -> [Patt] -> Parser ()
updateAtLevelPrimTypeOpControlSeq k txts = updateAtLevel k $ primTypeOpControlSeq %~ (:) txts

updatePrimTypeOpControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTypeOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTypeOpControlSeq args
    Locally -> updateLocalPrimTypeOpControlSeq args
    AtLevel k -> updateAtLevelPrimTypeOpControlSeq k args

updateGlobalPrimTypeControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTypeControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTypeControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeControlSeq :: [Patt] -> Parser ()
updateLocalPrimTypeControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimTypeControlSeq :: Int -> [Patt] -> Parser ()
updateAtLevelPrimTypeControlSeq k txts = updateAtLevel k $ primTypeControlSeq %~ (:) txts

updatePrimTypeControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTypeControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTypeControlSeq args
    Locally -> updateLocalPrimTypeControlSeq args
    AtLevel k -> updateAtLevelPrimTypeControlSeq k args

updateGlobalIdCount :: Int -> Parser ()
updateGlobalIdCount k = updateGlobal $ idCount %~ (const k)

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

