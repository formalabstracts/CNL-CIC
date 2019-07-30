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

-- wrapper datatype for parsing phrase lists (and extending the parser in general)
-- J stands for "Just", Q stands for question mark (as in postfix question mark)
-- A stands for alternation, i.e. for any of the [a]
data ParserMarkUp a =
    J a
  | Q a
  | A [ParserMarkUp a]
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
  primIdentifierType :: [[Patt]],
  primTypedName :: [[Patt]],
  primFreePredicate :: [[Patt]],
  primPhraseListFiller :: [[ParserMarkUp Text]],
  primPhraseListProofStatement :: [[ParserMarkUp Text]],
  primPhraseListTransition :: [[ParserMarkUp Text]],
  -- tvrExpr :: [TVar] -- TODO(jesse) integrate this later
  strSyms :: [[Text]], varDecl :: [Text], clsList :: [[Text]],
  idCount :: Int, hiddenCount :: Int, serialCounter :: Int}
  deriving (Show, Eq)

initialFState :: FState --TODO(jesse): move the rest of phrase_list.txt into the state and define corresponding parsers
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
  []
  phraseListFiller
  phraseListProofStatement
  phraseListTransition
  [] [] clsL0
  0 0 0
  where
  primAdjective0 = [[Wd ["positive"]]]
  primDefiniteNoun0 = [[Wd ["zero"]], [Wd ["one"]]]
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
