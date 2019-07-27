{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-
Author(s): Jesse Michael Han (2019)

Parser state.
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module CNLean.Basic.State where

import Prelude
import Control.Monad.Trans.State
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack, toLower)
import Data.Void
import Control.Monad (guard)
import qualified Data.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

data Patt = Wd [Text] | Sm Text | Vr | Nm
            deriving (Eq, Show)

data Maybe' a =
    J a
  | Q a
  deriving (Show, Eq)

data FState = FState { 
  primAdjective,        primAdjectiveMultiSubject,   primSimpleAdjective, primSimpleAdjectiveMultiSubject :: [[Patt]],
  primDefiniteNoun,     primPossessedNoun :: [[Patt]],
  primVerb,             primVerbMultiSubject :: [[Patt]],
  primTermOp,           primTermOpControlSeq,        primTermControlSeq :: [[Patt]],
  primTypeOp,           primTypeOpControlSeq,        primTypeControlSeq :: [[Patt]],
  primLambdaBinder,     primPiBinder,                primBinderProp :: [[Patt]],
  primBinaryRelationOp, primBinaryRelationControlSeq :: [[Patt]],
  primPropositionalOp,  primPropositionalOpControlSeq :: [[Patt]],
  primRelation :: [[Patt]],
  primPrefixFunction :: [[Patt]],
  primIdentifierTerm :: [[Patt]],
  primTypedName :: [[Patt]],
  primFreePredicate :: [[Patt]],
  primPhraseListFiller :: [[Maybe' Text]],
  -- tvrExpr :: [TVar] -- TODO(jesse) integrate this later
  strSyms :: [[Text]], varDecl :: [Text], clsList :: [[Text]],
  idCount :: Int, hiddenCount :: Int, serialCounter :: Int}
  deriving (Show, Eq)

initialFState :: FState --TODO(jesse): move the rest of phrase_list.txt into the state and define corresponding parsers
initialFState = FState
  [] [] [] []
  primDefiniteNoun0 []
  [] []
  [] [] []
  [] [] []
  [] [] []
  [] []
  [] []
  []
  []
  []
  []
  []
  phraseListFiller
  [] [] clsL0
  0 0 0
  where
  primDefiniteNoun0 = [[Wd ["zero"]]]
  -- adjE0 []    ntnE0 sntE0
  -- cfnE0 rfnE0 []    []
  -- []    []    []    iprE0
  -- []    []    clsL0
  -- 0 0 0
  -- where
  -- adjE0 = [[Wd ["equal"], Wd ["to"], Vr],
  --          [Wd ["nonequal"], Wd ["to"], Vr]]
          
  -- ntnE0 = [([Wd ["function","functions"], Nm]),
  --          ([Wd ["set","sets"], Nm]),
  --          ([Wd ["element", "elements"], Nm, Wd ["of"], Vr]),
  --          ([Wd ["object", "objects"], Nm])]
          
  -- sntE0 = [([Sm "=", Vr])]
  
  -- cfnE0 = [([Sm "Dom", Sm "(",Vr,Sm ")"]),
  --          ([Sm "(", Vr, Sm ",", Vr, Sm ")"]) ]
          
  -- rfnE0 = [[Sm "[", Vr, Sm "]"]]
  
  -- iprE0 = [([Sm "="]),
  --          ([Sm "!", Sm "="]),
  --          ([Sm "-", Sm "<", Sm "-"]),
  --          ([Sm "-~-"]) ]
  clsL0 = [["function"],
           ["element"],
           ["object"],
           ["number"],
           ["quotient"],
           ["dependent", "function"],
           ["thing"],
           ["class"],
           ["map"],
           ["structure"],
           ["term"],
           ["binary", "relation"],
           ["relation"],
           ["operator"],
           ["binary", "operator"],
           ["pairs"],
           ["pair"],
           ["result"]
          ]
  phraseListFiller :: [[Maybe' Text]]
  phraseListFiller = [
                     [J "we", J "have", Q "that"],
                     [J "we", J "know", Q "that"],
                     [Q "we", J "put"],
                     [J "we", J "write"],
                     [Q "we", J "write"]
                   ]

