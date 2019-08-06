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

data LocalGlobalFlag =
    Globally
  | Locally
  | AtLevel Int -- this constructor is not implemented yet, but it is easy to do so if we want more flexible state modifications
  deriving (Show, Eq)


updateGlobal :: (FState -> FState) -> Parser ()
updateGlobal f =
  (rest %= \fs -> (fs & _last %~ f)) <||> -- if the first branch fails, then rest is empty, so modify top instead
  (top %= f)

updateLocal :: (FState -> FState) -> Parser ()
updateLocal f =
  (top %= f)

updateGlobalPrimPrecTable :: [Patt] -> Int -> AssociativeParity -> Parser ()
updateGlobalPrimPrecTable ptts level parity = updateGlobal $ primPrecTable %~ M.insert ptts (level, parity)

updateGlobalClsList :: [Text] -> Parser ()
updateGlobalClsList txts = updateGlobal $ clsList %~ (:) txts

updateGlobalClsList2 :: [[Text]] -> Parser ()
updateGlobalClsList2 txtss = updateGlobal $ clsList %~ (<>) txtss

updateLocalClsList :: [Text] -> Parser ()
updateLocalClsList txts = updateLocal $ clsList %~ (:) txts

updateClsList :: LocalGlobalFlag -> [Text] -> Parser ()
updateClsList b args =
  case b of
    Globally -> updateGlobalClsList args
    Locally -> updateLocalClsList args

updateGlobalStrSyms :: [Text] -> Parser ()
updateGlobalStrSyms txts = updateGlobal $ strSyms %~ (:) txts

updateGlobalStrSyms2 :: [[Text]] -> Parser ()
updateGlobalStrSyms2 txtss = updateGlobal $ strSyms %~ (<>) txtss

updateLocalStrSyms :: [Text] -> Parser ()
updateLocalStrSyms txts = updateLocal $ strSyms %~ (:) txts

updateStrSyms :: LocalGlobalFlag -> [Text] -> Parser ()
updateStrSyms b args =
  case b of
    Globally -> updateGlobalStrSyms args
    Locally -> updateLocalStrSyms args

updateGlobalPrimDefiniteNoun :: [Patt] -> Parser ()
updateGlobalPrimDefiniteNoun txts = updateGlobal $ primDefiniteNoun %~ (:) txts

updateGlobalPrimDefiniteNoun2 :: [[Patt]] -> Parser ()
updateGlobalPrimDefiniteNoun2 txtss = updateGlobal $ primDefiniteNoun %~ (<>) txtss

updateLocalPrimDefiniteNoun :: [Patt] -> Parser ()
updateLocalPrimDefiniteNoun txts = updateLocal $ primDefiniteNoun %~ (:) txts

updatePrimDefiniteNoun :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimDefiniteNoun b args =
  case b of
    Globally -> updateGlobalPrimDefiniteNoun args
    Locally -> updateLocalPrimDefiniteNoun args

updateGlobalPrimIdentifierTerm :: [Patt] -> Parser ()
updateGlobalPrimIdentifierTerm txts = updateGlobal $ primIdentifierTerm %~ (:) txts

updateGlobalPrimIdentifierTerm2 :: [[Patt]] -> Parser ()
updateGlobalPrimIdentifierTerm2 txtss = updateGlobal $ primIdentifierTerm %~ (<>) txtss

updateLocalPrimIdentifierTerm :: [Patt] -> Parser ()
updateLocalPrimIdentifierTerm txts = updateLocal $ primIdentifierTerm %~ (:) txts

updatePrimIdentifierTerm :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimIdentifierTerm b args =
  case b of
    Globally -> updateGlobalPrimIdentifierTerm args
    Locally -> updateLocalPrimIdentifierTerm args

updateGlobalPrimTermControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTermControlSeq txts = updateGlobal $ primTermControlSeq %~ (:) txts

updateGlobalPrimTermControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTermControlSeq2 txtss = updateGlobal $ primTermControlSeq %~ (<>) txtss

updateLocalPrimTermControlSeq :: [Patt] -> Parser ()
updateLocalPrimTermControlSeq txts = updateLocal $ primTermControlSeq %~ (:) txts

updatePrimTermControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTermControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTermControlSeq args
    Locally -> updateLocalPrimTermControlSeq args

updateGlobalPrimTermOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTermOpControlSeq txts = updateGlobal $ primTermOpControlSeq %~ (:) txts

updateGlobalPrimTermOpControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTermOpControlSeq2 txtss = updateGlobal $ primTermOpControlSeq %~ (<>) txtss

updateLocalPrimTermOpControlSeq :: [Patt] -> Parser ()
updateLocalPrimTermOpControlSeq txts = updateLocal $ primTermOpControlSeq %~ (:) txts

updatePrimTermOpControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTermOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTermOpControlSeq args
    Locally -> updateLocalPrimTermOpControlSeq args

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

updatePrimAdjective :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimAdjective b args =
  case b of
    Globally -> updateGlobalPrimAdjective args
    Locally -> updateLocalPrimAdjective args

updateGlobalPrimAdjectiveMultiSubject :: [Patt] -> Parser ()
updateGlobalPrimAdjectiveMultiSubject txts = updateGlobal $ primAdjectiveMultiSubject %~ (:) txts

updateGlobalPrimAdjectiveMultiSubject2 :: [[Patt]] -> Parser ()
updateGlobalPrimAdjectiveMultiSubject2 txtss = updateGlobal $ primAdjectiveMultiSubject %~ (<>) txtss

updateLocalPrimAdjectiveMultiSubject :: [Patt] -> Parser ()
updateLocalPrimAdjectiveMultiSubject txts = updateLocal $ primAdjectiveMultiSubject %~ (:) txts

updatePrimAdjectiveMultiSubject :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimAdjectiveMultiSubject b args =
  case b of
    Globally -> updateGlobalPrimAdjectiveMultiSubject args
    Locally -> updateLocalPrimAdjectiveMultiSubject args

updateGlobalPrimVerb :: [Patt] -> Parser ()
updateGlobalPrimVerb txts = updateGlobal $ primVerb %~ (:) txts

updateGlobalPrimVerb2 :: [[Patt]] -> Parser ()
updateGlobalPrimVerb2 txtss = updateGlobal $ primVerb %~ (<>) txtss

updateLocalPrimVerb :: [Patt] -> Parser ()
updateLocalPrimVerb txts = updateLocal $ primVerb %~ (:) txts

updatePrimVerb :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimVerb b args =
  case b of
    Globally -> updateGlobalPrimVerb args
    Locally -> updateLocalPrimVerb args

updateGlobalPrimVerbMultiSubject :: [Patt] -> Parser ()
updateGlobalPrimVerbMultiSubject txts = updateGlobal $ primVerbMultiSubject %~ (:) txts

updateGlobalPrimVerbMultiSubject2 :: [[Patt]] -> Parser ()
updateGlobalPrimVerbMultiSubject2 txtss = updateGlobal $ primVerbMultiSubject %~ (<>) txtss

updateLocalPrimVerbMultiSubject :: [Patt] -> Parser ()
updateLocalPrimVerbMultiSubject txts = updateLocal $ primVerbMultiSubject %~ (:) txts

updatePrimVerbMultiSubject :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimVerbMultiSubject b args =
  case b of
    Globally -> updateGlobalPrimVerbMultiSubject args
    Locally -> updateLocalPrimVerbMultiSubject args

updateGlobalPrimRelation :: [Patt] -> Parser ()
updateGlobalPrimRelation txts = updateGlobal $ primRelation %~ (:) txts

updateGlobalPrimRelation2 :: [[Patt]] -> Parser ()
updateGlobalPrimRelation2 txtss = updateGlobal $ primRelation %~ (<>) txtss

updateLocalPrimRelation :: [Patt] -> Parser ()
updateLocalPrimRelation txts = updateLocal $ primRelation %~ (:) txts

updatePrimRelation :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimRelation b args =
  case b of
    Globally -> updateGlobalPrimRelation args
    Locally -> updateLocalPrimRelation args

updateGlobalPrimPropositionalOp :: [Patt] -> Parser ()
updateGlobalPrimPropositionalOp txts = updateGlobal $ primPropositionalOp %~ (:) txts

updateGlobalPrimPropositionalOp2 :: [[Patt]] -> Parser ()
updateGlobalPrimPropositionalOp2 txtss = updateGlobal $ primPropositionalOp %~ (<>) txtss

updateLocalPrimPropositionalOp :: [Patt] -> Parser ()
updateLocalPrimPropositionalOp txts = updateLocal $ primPropositionalOp %~ (:) txts

updatePrimPropositionalOp :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimPropositionalOp b args =
  case b of
    Globally -> updateGlobalPrimPropositionalOp args
    Locally -> updateLocalPrimPropositionalOp args

updateGlobalPrimBinaryRelationOp :: [Patt] -> Parser ()
updateGlobalPrimBinaryRelationOp txts = updateGlobal $ primBinaryRelationOp %~ (:) txts

updateGlobalPrimBinaryRelationOp2 :: [[Patt]] -> Parser ()
updateGlobalPrimBinaryRelationOp2 txtss = updateGlobal $ primBinaryRelationOp %~ (<>) txtss

updateLocalPrimBinaryRelationOp :: [Patt] -> Parser ()
updateLocalPrimBinaryRelationOp txts = updateLocal $ primBinaryRelationOp %~ (:) txts

updatePrimBinaryRelationOp :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimBinaryRelationOp b args =
  case b of
    Globally -> updateGlobalPrimBinaryRelationOp args
    Locally -> updateLocalPrimBinaryRelationOp args

updateGlobalPrimBinaryRelationControlSeq :: [Patt] -> Parser ()
updateGlobalPrimBinaryRelationControlSeq txts = updateGlobal $ primBinaryRelationControlSeq %~ (:) txts

updateGlobalPrimBinaryRelationControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimBinaryRelationControlSeq2 txtss = updateGlobal $ primBinaryRelationControlSeq %~ (<>) txtss

updateLocalPrimBinaryRelationControlSeq :: [Patt] -> Parser ()
updateLocalPrimBinaryRelationControlSeq txts = updateLocal $ primBinaryRelationControlSeq %~ (:) txts

updatePrimBinaryRelationControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimBinaryRelationControlSeq b args =
  case b of
    Globally -> updateGlobalPrimBinaryRelationControlSeq args
    Locally -> updateLocalPrimBinaryRelationControlSeq args

updateGlobalPrimPropositionalOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimPropositionalOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimPropositionalOpControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimPropositionalOpControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimPropositionalOpControlSeq :: [Patt] -> Parser ()
updateLocalPrimPropositionalOpControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimPropositionalOpControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimPropositionalOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimPropositionalOpControlSeq args
    Locally -> updateLocalPrimPropositionalOpControlSeq args

updateGlobalPrimIdentifierType :: [Patt] -> Parser ()
updateGlobalPrimIdentifierType txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimIdentifierType2 :: [[Patt]] -> Parser ()
updateGlobalPrimIdentifierType2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimIdentifierType :: [Patt] -> Parser ()
updateLocalPrimIdentifierType txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimIdentifierType :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimIdentifierType b args =
  case b of
    Globally -> updateGlobalPrimIdentifierType args
    Locally -> updateLocalPrimIdentifierType args

updateGlobalPrimTypeOp :: [Patt] -> Parser ()
updateGlobalPrimTypeOp txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeOp2 :: [[Patt]] -> Parser ()
updateGlobalPrimTypeOp2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeOp :: [Patt] -> Parser ()
updateLocalPrimTypeOp txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimTypeOp :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTypeOp b args =
  case b of
    Globally -> updateGlobalPrimTypeOp args
    Locally -> updateLocalPrimTypeOp args

updateGlobalPrimTypeOpControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTypeOpControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeOpControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTypeOpControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeOpControlSeq :: [Patt] -> Parser ()
updateLocalPrimTypeOpControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimTypeOpControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTypeOpControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTypeOpControlSeq args
    Locally -> updateLocalPrimTypeOpControlSeq args

updateGlobalPrimTypeControlSeq :: [Patt] -> Parser ()
updateGlobalPrimTypeControlSeq txts = updateGlobal $ primPropositionalOpControlSeq %~ (:) txts

updateGlobalPrimTypeControlSeq2 :: [[Patt]] -> Parser ()
updateGlobalPrimTypeControlSeq2 txtss = updateGlobal $ primPropositionalOpControlSeq %~ (<>) txtss

updateLocalPrimTypeControlSeq :: [Patt] -> Parser ()
updateLocalPrimTypeControlSeq txts = updateLocal $ primPropositionalOpControlSeq %~ (:) txts

updatePrimTypeControlSeq :: LocalGlobalFlag -> [Patt] -> Parser ()
updatePrimTypeControlSeq b args =
  case b of
    Globally -> updateGlobalPrimTypeControlSeq args
    Locally -> updateLocalPrimTypeControlSeq args

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

