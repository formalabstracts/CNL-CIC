{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-
Author(s): Jesse Michael Han (2019)

Parser state.
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Colada.Basic.State where

import Prelude hiding (Word)
import Control.Monad.Trans.State
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack, toLower)
import Data.Void
import qualified Data.Map as M
import Control.Monad (guard)
import qualified Data.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Lens
import Control.Lens.TH

data Patt = Wd [Text] | Sm Text | Vr | Nm | CSeq Text [Patt]
            deriving (Eq, Show, Ord)

data Pattern =
    Patts {_patts :: [Patt]}
  | MacroPatts {_patts :: [Patt]}
  deriving (Show, Eq, Ord)

$(makeLenses ''Pattern)

toMacroPatts :: Pattern -> Pattern
toMacroPatts pttn = case pttn of
  (Patts ptts) -> MacroPatts ptts
  pttn         -> pttn

instance Semigroup Pattern where
  (<>) patt1 patt2 = Patts $ patt1^.patts <> patt2^.patts

-- wrapper datatype for parsing phrase lists (and extending the parser in general)
-- J stands for "Just", Q stands for question mark (as in postfix question mark)
-- A stands for alternation, i.e. parse any marked up word in the list.
data ParserMarkUp a =
    J a
  | Q a
  | A [ParserMarkUp a]
  deriving (Show, Eq)

data AssociativeParity =
    AssociatesLeft
  | AssociatesRight
  | AssociatesNone
  deriving (Show, Eq)

defaultPrec :: (Int, AssociativeParity)
defaultPrec =  (10, AssociatesLeft)

defaultAssociativeParity :: AssociativeParity
defaultAssociativeParity = AssociatesLeft

data FState = FState { 
  _primAdjective,        _primAdjectiveMultiSubject,   _primSimpleAdjective, _primSimpleAdjectiveMultiSubject :: [Pattern],
  _primDefiniteNoun,     _primPossessedNoun :: [Pattern],
  _primVerb,             _primVerbMultiSubject :: [Pattern],
  _primTermOp,           _primTermOpControlSeq,        _primTermControlSeq :: [Pattern],
  _primTypeOp,           _primTypeOpControlSeq,        _primTypeControlSeq :: [Pattern],
  _primLambdaBinder,     _primPiBinder,                _primBinderProp :: [Pattern],
  _primBinaryRelationOp, _primBinaryRelationControlSeq :: [Pattern],
  _primPropositionalOp,  _primPropositionalOpControlSeq :: [Pattern],
  _primRelation :: [Pattern],
  -- _primPrefixFunction :: [Pattern],
  _primIdentifierTerm :: [Pattern],
  _primIdentifierType :: [Pattern],
  _primTypedName :: [Pattern],
  _primFreePredicate :: [Pattern],
  _primPhraseListFiller :: [[ParserMarkUp Text]],
  _primPhraseListProofStatement :: [[ParserMarkUp Text]],
  _primPhraseListTransition :: [[ParserMarkUp Text]],
  _primPrecTable :: M.Map Pattern (Int, AssociativeParity),
  -- tvrExpr :: [TVar] -- TODO integrate this later
  _strSyms :: [[Text]], _varDecl :: [Text], _clsList :: [[Text]],
  _idCount :: Int, _hiddenCount :: Int, _serialCounter :: Int,
  _sectionId :: Maybe Text,
  _sectionTag :: Maybe Text
  }
  deriving (Show, Eq)

-- a StateVec is a nonempty list of states
data StateVec a = StateVec {_top :: a, _rest :: [a]}
  deriving (Show, Eq)

instance Functor StateVec where
  fmap g (StateVec t fs) = StateVec (g t) (map g fs)

states :: StateVec a -> [a]
states (StateVec a as) = a:as

depthStateVec :: StateVec a -> Int
depthStateVec (StateVec x xs) = (length xs)

mkStateVec :: [a] -> Maybe (StateVec a)
mkStateVec xs = case xs of
  [] -> Nothing
  x:xs -> Just $ StateVec x xs

consStateVec :: a -> StateVec a -> StateVec a
consStateVec x (StateVec t fs) = StateVec x (t:fs)

initialFStateVec :: StateVec FState
initialFStateVec = StateVec initialFState []

emptyFState :: FState
emptyFState = FState
  [] [] [] []
  [] []
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
  []
  []
  []
  M.empty
  [] [] []
  0 0 0
  Nothing
  Nothing

initialFState :: FState
initialFState = FState
  primAdjective0 [] [] []
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
  phraseListProofStatement
  phraseListTransition
  M.empty
  [] [] clsL0
  0 0 0
  Nothing
  (Just "document")
  where
  primAdjective0 = [Patts [Wd ["positive"]]]
  primDefiniteNoun0 = [Patts [Wd ["zero"]], Patts [Wd ["one"]]]
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
  clsL0 = [
           ["function"],
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
           ["result"],
           ["natural", "number"]
          ]
  phraseListFiller :: [[ParserMarkUp Text]]
  phraseListFiller = [
                     [J "we", J "have", Q "that"],
                     [J "we", J "know", Q "that"],
                     [Q "we", J "put"],
                     [J "we", J "write"],
                     [Q "we", J "write"]
                   ]
  phraseListProofStatement :: [[ParserMarkUp Text]]
  phraseListProofStatement = [
                             [J "We", J "proceed", J "as", J "follows", J "."],
                             [J "The", A [J "result" , J "lemma" , J "theorem" , J "proposition" , J "corollary"], Q "now", J "follows", J "."],
                             [J "The", J "other",  J "cases",  J "are",  J "similar",  J "."],
                             [ J "The",  J "proof",  J "is", A [ J "obvious",  J "trivial",  J"easy",  J "routine" ], J "."]
                             ]
  phraseListTransition :: [[ParserMarkUp Text]]
  phraseListTransition = [
                         [ J "a", J "basic", J "fact", J "is", J "that"],
                         [ J "accordingly" ],
                         [ J "additionally" ],
                         [ J "again" ],
                         [ J "also" ],
                         [ J "and", J "yet" ],
                         [ J "as", J "a", J "result"],
                         [ J "as", J "usual"],
                         [ J "as", J "we", J "have", J "seen"],
                         [ J "as", J "we", J "see"],
                         [ J "at", J "the", J "same", J "time"],
                         [ J "besides"],
                         [ J "but"],
                         [ J "by", J "definition"],
                         [ J "certainly"],
                         [ J "clearly"],
                         [ J "computations", J "show", J "that"],
                         [ J "consequently"],
                         [ J "conversely"],
                         [ J "equally", J "important"],
                         [ J "explicitly"],
                         [ J "finally"],
                         [ J "first"],
                         [ J "for", J "example"],
                         [ J "for", J "instance"],
                         [ J "for", J "simplicity"],
                         [ J "for", J "that", J "reason"],
                         [ J "for", J "this", J "purpose"],
                         [ J "further"],
                         [ J "furthermore"],
                         [ J "generally"],
                         [ J "hence"],
                         [ J "here"],
                         [ J "however"],
                         [ J "importantly"],
                         [ J "in", J "addition"],
                         [ J "in", J "any", J "event"],
                         [ J "in", J "brief"],
                         [ J "in", J "consequence"],
                         [ J "in", J "contrast"],
                         [ J "in", J "contrast", J "to", J "this"],
                         [ J "in", J "each", J "case"],
                         [ J "in", J "fact"],
                         [ J "in", J "general"],
                         [ J "in", J "other", J "words"],
                         [ J "in", J "particular"],
                         [ J "in", J "short"],
                         [ J "in", J "sum"],
                         [ J "in", J "summary"],
                         [ J "in", J "the", J "present", J "case"],
                         [ J "in", J "the", J "same", J "way"],
                         [ J "in", J "this", J "computation"],
                         [ J "in", J "this", J "sense"],
                         [ J "indeed"],
                         [ J "it", J "follows", Q "that"],
                         [ J "it", J "is", J "clear", J "that"],
                         [ J "it", J "is", J "enough", J "to", J "show", J "that"],
                         [ J "it", J "is", J "known", Q "that"],
                         [ J "it", J "is", J "routine", J "that"],
                         [ J "it", J "is", J "trivial", J "to", J "see", Q "that"],
                         [ J "it", J "is", J "understood", J "that"],
                         [ J "it", J "turns", J "out", J "that"],
                         [ J "last"],
                         [ J "likewise"],
                         [ J "more", J "precisely"],
                         [ J "moreover"],
                         [ J "most", J "importantly"],
                         [ J "neverthess"],
                         [ J "next"],
                         [ J "nonetheless"],
                         [ J "note", Q "that"],
                         [ J "notice", Q "that"],
                         [ J "now"],
                         [ J "observe", Q "that"],
                         [ J "obviously"],
                         [ J "of", J "course"],
                         [ J "on", J "the", J "contrary"],
                         [ J "on", J "the", J "other", J "hand"],
                         [ J "on", J "the", J "whole"],
                         [ J "otherwise"],
                         [ J "second"],
                         [ J "similarly"],
                         [ J "so"],
                         [ J "specifically"],
                         [ J "still"],
                         [ J "that", J "is"],
                         [ J "the", J "point", J "is", J "that"],
                         [ J "then"],
                         [ J "therefore"],
                         [ J "third"],
                         [ J "this", J "gives", Q "that"],
                         [ J "this", J "implies"],
                         [ J "this", J "means", Q "that"],
                         [ J "this", J "yields", Q "that"],
                         [ J "thus"],
                         [ J "thus", J "far"],
                         [ J "to", J "begin", J "with"],
                         [ J "to", J "this", J "end"],
                         [ J "trivially"],
                         [ J "we", J "claim", Q "that"],
                         [ J "we", J "emphasize", Q "that"],
                         [ J "we", J "first", J "show", Q "that"],
                         [ J "we", J "get"],
                         [ J "we", J "have", J "seen", J "that"],
                         [ J "we", J "have", Q "that"],
                         [ J "we", J "know", Q "that"],
                         [ J "we", Q "may", J "check", Q "that"],
                         [ J "we", J "obtain", Q "that"],
                         [ J "we", J "remark", Q "that"],
                         [ J "we", J "say", Q "that"],
                         [ J "we", J "see", J "that"],
                         [ J "we", J "show", Q "that"],
                         [ J "we", J "understand", Q "that"],
                         [ J "we", J "write", Q "that"],
                         [ Q  "we", J "recall", Q "that"],
                         [ J "without", J "loss", J "of", J "generality"],
                         [ J "yet"]
                         ]
