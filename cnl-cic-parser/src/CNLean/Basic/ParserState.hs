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

updateGlobalPrimPrecTable :: Pattern -> Int -> AssociativeParity -> Parser ()
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

updateGlobalPrimDefiniteNoun :: Pattern -> Parser ()
updateGlobalPrimDefiniteNoun txts = updateGlobal $ primDefiniteNoun %~ (:) txts

updateGlobalPrimDefiniteNoun2 :: [Pattern] -> Parser ()
updateGlobalPrimDefiniteNoun2 txtss = updateGlobal $ primDefiniteNoun %~ (<>) txtss

updateLocalPrimDefiniteNoun :: Pattern -> Parser ()
updateLocalPrimDefiniteNoun txts = updateLocal $ primDefiniteNoun %~ (:) txts

updateAtLevelPrimDefiniteNoun :: Int -> Pattern -> Parser ()
updateAtLevelPrimDefiniteNoun k txts = updateAtLevel k $ primDefiniteNoun %~ (:) txts

updatePrimDefiniteNoun :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimDefiniteNoun b args =
  case b of
    Globally -> updateGlobalPrimDefiniteNoun args
    Locally -> updateLocalPrimDefiniteNoun args
    AtLevel k -> updateAtLevelPrimDefiniteNoun k args

updateGlobalPrimIdentifierTerm :: Pattern -> Parser ()
updateGlobalPrimIdentifierTerm txts = updateGlobal $ primIdentifierTerm %~ (:) txts

updateGlobalPrimIdentifierTerm2 :: [Pattern] -> Parser ()
updateGlobalPrimIdentifierTerm2 txtss = updateGlobal $ primIdentifierTerm %~ (<>) txtss

updateLocalPrimIdentifierTerm :: Pattern -> Parser ()
updateLocalPrimIdentifierTerm txts = updateLocal $ primIdentifierTerm %~ (:) txts

updateAtLevelPrimIdentifierTerm :: Int -> Pattern -> Parser ()
updateAtLevelPrimIdentifierTerm k txts = updateAtLevel k $ primIdentifierTerm %~ (:) txts

updatePrimIdentifierTerm :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimIdentifierTerm b args =
  case b of
    Globally -> updateGlobalPrimIdentifierTerm args
    Locally -> updateLocalPrimIdentifierTerm args
    AtLevel k -> updateAtLevelPrimIdentifierTerm k args

updateGlobalPrimTermControlSeq :: Pattern -> Parser ()
updateGlobalPrimTermControlSeq txts = updateGlobal $ primTermControlSeq %~ (:) txts

updateGlobalPrimTermControlSeq2 :: [Pattern] -> Parser ()
updateGlobalPrimTermControlSeq2 txtss = updateGlobal $ primTermControlSeq %~ (<>) txtss

updateLocalPrimTermControlSeq :: Pattern -> Parser ()
updateLocalPrimTermControlSeq txts = updateLocal $ primTermControlSeq %~ (:) txts

updateAtLevelPrimTermControlSeq :: Int -> Pattern -> Parser ()
updateAtLevelPrimTermControlSeq k txts = updateAtLevel k $ primTermControlSeq %~ (:) txts

updatePrimTermControlSeq :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimTermControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTermControlSeq args
    Locally -> updateLocalPrimTermControlSeq args
    AtLevel k -> updateAtLevelPrimTermControlSeq k args

updateGlobalPrimTermOpControlSeq :: Pattern -> Parser ()
updateGlobalPrimTermOpControlSeq txts = updateGlobal $ primTermOpControlSeq %~ (:) txts

updateGlobalPrimTermOpControlSeq2 :: [Pattern] -> Parser ()
updateGlobalPrimTermOpControlSeq2 txtss = updateGlobal $ primTermOpControlSeq %~ (<>) txtss

updateLocalPrimTermOpControlSeq :: Pattern -> Parser ()
updateLocalPrimTermOpControlSeq txts = updateLocal $ primTermOpControlSeq %~ (:) txts

updateAtLevelPrimTermOpControlSeq :: Int -> Pattern -> Parser ()
updateAtLevelPrimTermOpControlSeq k txts = updateAtLevel k $ primTermOpControlSeq %~ (:) txts

updatePrimTermOpControlSeq :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimTermOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTermOpControlSeq args
    Locally -> updateLocalPrimTermOpControlSeq args
    AtLevel k -> updateAtLevelPrimTermOpControlSeq k args

-- updateGlobalPrimPrefixFunction :: Pattern -> Parser ()
-- updGlobalatePrimPrefixFunction txts = updateLocal $ primPrefixFunction %= (:) txts
-- updateGlobalPrimPrefixFunction2 :: [Pattern] -> Parser ()
-- updGlobalatePrimPrefixFunction2 txtss = updateGlobal $ primPrefixFunction %= (<>) txtss

-- updateLocalPrimPrefixFunction :: Pattern -> Parser ()
-- updateLocalPrimPrefixFunction txts = updateLocal $ primPrefixFunction %~ (:) txts

updateGlobalPrimAdjective :: Pattern -> Parser ()
updateGlobalPrimAdjective txts = updateGlobal $ primAdjective %~ (:) txts

updateGlobalPrimAdjective2 :: [Pattern] -> Parser ()
updateGlobalPrimAdjective2 txtss = updateGlobal $ primAdjective %~ (<>) txtss

updateLocalPrimAdjective :: Pattern -> Parser ()
updateLocalPrimAdjective txts = updateLocal $ primAdjective %~ (:) txts

updateAtLevelPrimAdjective :: Int -> Pattern -> Parser ()
updateAtLevelPrimAdjective k txts = updateAtLevel k $ primAdjective %~ (:) txts

updatePrimAdjective :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimAdjective b args =
  case b of
    Globally -> updateGlobalPrimAdjective args
    Locally -> updateLocalPrimAdjective args
    AtLevel k -> updateAtLevelPrimAdjective k args

updateGlobalPrimAdjectiveMultiSubject :: Pattern -> Parser ()
updateGlobalPrimAdjectiveMultiSubject txts = updateGlobal $ primAdjectiveMultiSubject %~ (:) txts

updateGlobalPrimAdjectiveMultiSubject2 :: [Pattern] -> Parser ()
updateGlobalPrimAdjectiveMultiSubject2 txtss = updateGlobal $ primAdjectiveMultiSubject %~ (<>) txtss

updateLocalPrimAdjectiveMultiSubject :: Pattern -> Parser ()
updateLocalPrimAdjectiveMultiSubject txts = updateLocal $ primAdjectiveMultiSubject %~ (:) txts

updateAtLevelPrimAdjectiveMultiSubject :: Int -> Pattern -> Parser ()
updateAtLevelPrimAdjectiveMultiSubject k txts = updateAtLevel k $ primAdjectiveMultiSubject %~ (:) txts

updatePrimAdjectiveMultiSubject :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimAdjectiveMultiSubject b args =
  case b of
    Globally -> updateGlobalPrimAdjectiveMultiSubject args
    Locally -> updateLocalPrimAdjectiveMultiSubject args
    AtLevel k -> updateAtLevelPrimAdjectiveMultiSubject k args

updateGlobalPrimVerb :: Pattern -> Parser ()
updateGlobalPrimVerb txts = updateGlobal $ primVerb %~ (:) txts

updateGlobalPrimVerb2 :: [Pattern] -> Parser ()
updateGlobalPrimVerb2 txtss = updateGlobal $ primVerb %~ (<>) txtss

updateLocalPrimVerb :: Pattern -> Parser ()
updateLocalPrimVerb txts = updateLocal $ primVerb %~ (:) txts

updateAtLevelPrimVerb :: Int -> Pattern -> Parser ()
updateAtLevelPrimVerb k txts = updateAtLevel k $ primVerb %~ (:) txts

updatePrimVerb :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimVerb b args =
  case b of
    Globally -> updateGlobalPrimVerb args
    Locally -> updateLocalPrimVerb args
    AtLevel k -> updateAtLevelPrimVerb k args

updateGlobalPrimVerbMultiSubject :: Pattern -> Parser ()
updateGlobalPrimVerbMultiSubject txts = updateGlobal $ primVerbMultiSubject %~ (:) txts

updateGlobalPrimVerbMultiSubject2 :: [Pattern] -> Parser ()
updateGlobalPrimVerbMultiSubject2 txtss = updateGlobal $ primVerbMultiSubject %~ (<>) txtss

updateLocalPrimVerbMultiSubject :: Pattern -> Parser ()
updateLocalPrimVerbMultiSubject txts = updateLocal $ primVerbMultiSubject %~ (:) txts

updateAtLevelPrimVerbMultiSubject :: Int -> Pattern -> Parser ()
updateAtLevelPrimVerbMultiSubject k txts = updateAtLevel k $ primVerbMultiSubject %~ (:) txts

updatePrimVerbMultiSubject :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimVerbMultiSubject b args =
  case b of
    Globally -> updateGlobalPrimVerbMultiSubject args
    Locally -> updateLocalPrimVerbMultiSubject args
    AtLevel k -> updateAtLevelPrimVerbMultiSubject k args

updateGlobalPrimRelation :: Pattern -> Parser ()
updateGlobalPrimRelation txts = updateGlobal $ primRelation %~ (:) txts

updateGlobalPrimRelation2 :: [Pattern] -> Parser ()
updateGlobalPrimRelation2 txtss = updateGlobal $ primRelation %~ (<>) txtss

updateLocalPrimRelation :: Pattern -> Parser ()
updateLocalPrimRelation txts = updateLocal $ primRelation %~ (:) txts

updateAtLevelPrimRelation :: Int -> Pattern -> Parser ()
updateAtLevelPrimRelation k txts = updateAtLevel k $ primRelation %~ (:) txts

updatePrimRelation :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimRelation b args =
  case b of
    Globally -> updateGlobalPrimRelation args
    Locally -> updateLocalPrimRelation args
    AtLevel k -> updateAtLevelPrimRelation k args

updateGlobalPrimPropositionalOp :: Pattern -> Parser ()
updateGlobalPrimPropositionalOp txts = updateGlobal $ primPropositionalOp %~ (:) txts

updateGlobalPrimPropositionalOp2 :: [Pattern] -> Parser ()
updateGlobalPrimPropositionalOp2 txtss = updateGlobal $ primPropositionalOp %~ (<>) txtss

updateLocalPrimPropositionalOp :: Pattern -> Parser ()
updateLocalPrimPropositionalOp txts = updateLocal $ primPropositionalOp %~ (:) txts

updateAtLevelPrimPropositionalOp :: Int -> Pattern -> Parser ()
updateAtLevelPrimPropositionalOp k txts = updateAtLevel k $ primPropositionalOp %~ (:) txts

updatePrimPropositionalOp :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimPropositionalOp b args =
  case b of
    Globally -> updateGlobalPrimPropositionalOp args
    Locally -> updateLocalPrimPropositionalOp args
    AtLevel k -> updateAtLevelPrimPropositionalOp k args

updateGlobalPrimBinaryRelationOp :: Pattern -> Parser ()
updateGlobalPrimBinaryRelationOp txts = updateGlobal $ primBinaryRelationOp %~ (:) txts

updateGlobalPrimBinaryRelationOp2 :: [Pattern] -> Parser ()
updateGlobalPrimBinaryRelationOp2 txtss = updateGlobal $ primBinaryRelationOp %~ (<>) txtss

updateLocalPrimBinaryRelationOp :: Pattern -> Parser ()
updateLocalPrimBinaryRelationOp txts = updateLocal $ primBinaryRelationOp %~ (:) txts

updateAtLevelPrimBinaryRelationOp :: Int -> Pattern -> Parser ()
updateAtLevelPrimBinaryRelationOp k txts = updateAtLevel k $ primBinaryRelationOp %~ (:) txts

updatePrimBinaryRelationOp :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimBinaryRelationOp b args =
  case b of
    Globally -> updateGlobalPrimBinaryRelationOp args
    Locally -> updateLocalPrimBinaryRelationOp args
    AtLevel k -> updateAtLevelPrimBinaryRelationOp k args

updateGlobalPrimBinaryRelationControlSeq :: Pattern -> Parser ()
updateGlobalPrimBinaryRelationControlSeq txts = updateGlobal $ primBinaryRelationControlSeq %~ (:) txts

updateGlobalPrimBinaryRelationControlSeq2 :: [Pattern] -> Parser ()
updateGlobalPrimBinaryRelationControlSeq2 txtss = updateGlobal $ primBinaryRelationControlSeq %~ (<>) txtss

updateLocalPrimBinaryRelationControlSeq :: Pattern -> Parser ()
updateLocalPrimBinaryRelationControlSeq txts = updateLocal $ primBinaryRelationControlSeq %~ (:) txts

updateAtLevelPrimBinaryRelationControlSeq :: Int -> Pattern -> Parser ()
updateAtLevelPrimBinaryRelationControlSeq k txts = updateAtLevel k $ primBinaryRelationControlSeq %~ (:) txts

updatePrimBinaryRelationControlSeq :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimBinaryRelationControlSeq b args =
  case b of
    Globally -> updateGlobalPrimBinaryRelationControlSeq args
    Locally -> updateLocalPrimBinaryRelationControlSeq args
    AtLevel k -> updateAtLevelPrimBinaryRelationControlSeq k args

updateGlobalPrimPropositionalOpControlSeq :: Pattern -> Parser ()
updateGlobalPrimPropositionalOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimPropositionalOpControlSeq2 :: [Pattern] -> Parser ()
updateGlobalPrimPropositionalOpControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimPropositionalOpControlSeq :: Pattern -> Parser ()
updateLocalPrimPropositionalOpControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimPropositionalOpControlSeq :: Int -> Pattern -> Parser ()
updateAtLevelPrimPropositionalOpControlSeq k txts = updateAtLevel k $ primPropositionalOpControlSeq %~ (:) txts

updatePrimPropositionalOpControlSeq :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimPropositionalOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimPropositionalOpControlSeq args
    Locally -> updateLocalPrimPropositionalOpControlSeq args
    AtLevel k -> updateAtLevelPrimPropositionalOpControlSeq k args

updateGlobalPrimIdentifierType :: Pattern -> Parser ()
updateGlobalPrimIdentifierType txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimIdentifierType2 :: [Pattern] -> Parser ()
updateGlobalPrimIdentifierType2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimIdentifierType :: Pattern -> Parser ()
updateLocalPrimIdentifierType txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimIdentifierType :: Int -> Pattern -> Parser ()
updateAtLevelPrimIdentifierType k txts = updateAtLevel k $ primIdentifierType %~ (:) txts

updatePrimIdentifierType :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimIdentifierType b args =
  case b of
    Globally -> updateGlobalPrimIdentifierType args
    Locally -> updateLocalPrimIdentifierType args
    AtLevel k -> updateAtLevelPrimIdentifierType k args

updateGlobalPrimTypeOp :: Pattern -> Parser ()
updateGlobalPrimTypeOp txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeOp2 :: [Pattern] -> Parser ()
updateGlobalPrimTypeOp2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeOp :: Pattern -> Parser ()
updateLocalPrimTypeOp txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimTypeOp :: Int -> Pattern -> Parser ()
updateAtLevelPrimTypeOp k txts = updateAtLevel k $ primTypeOp %~ (:) txts

updatePrimTypeOp :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimTypeOp b args =
  case b of
    Globally -> updateGlobalPrimTypeOp args
    Locally -> updateLocalPrimTypeOp args
    AtLevel k -> updateAtLevelPrimTypeOp k args

updateGlobalPrimTypeOpControlSeq :: Pattern -> Parser ()
updateGlobalPrimTypeOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeOpControlSeq2 :: [Pattern] -> Parser ()
updateGlobalPrimTypeOpControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeOpControlSeq :: Pattern -> Parser ()
updateLocalPrimTypeOpControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimTypeOpControlSeq :: Int -> Pattern -> Parser ()
updateAtLevelPrimTypeOpControlSeq k txts = updateAtLevel k $ primTypeOpControlSeq %~ (:) txts

updatePrimTypeOpControlSeq :: LocalGlobalFlag -> Pattern -> Parser ()
updatePrimTypeOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTypeOpControlSeq args
    Locally -> updateLocalPrimTypeOpControlSeq args
    AtLevel k -> updateAtLevelPrimTypeOpControlSeq k args

updateGlobalPrimTypeControlSeq :: Pattern -> Parser ()
updateGlobalPrimTypeControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeControlSeq2 :: [Pattern] -> Parser ()
updateGlobalPrimTypeControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeControlSeq :: Pattern -> Parser ()
updateLocalPrimTypeControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updateAtLevelPrimTypeControlSeq :: Int -> Pattern -> Parser ()
updateAtLevelPrimTypeControlSeq k txts = updateAtLevel k $ primTypeControlSeq %~ (:) txts

updatePrimTypeControlSeq :: LocalGlobalFlag -> Pattern -> Parser ()
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

