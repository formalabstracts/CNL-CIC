{-
Author(s): Jesse Michael Han (2019)

Parsing theorems.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Theorem where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import Colada.Basic.Basic
import Colada.Assumption
import Colada.Type
import Colada.PhraseList

data Theorem = Theorem TheoremPreamble [Assumption] AffirmProof
  deriving (Show, Eq)

parseTheorem :: Parser Theorem
parseTheorem = Theorem <$> parseTheoremPreamble <*> (many' parseAssumption) <*> parseAffirmProof

newtype TheoremPreamble = TheoremPreamble (Maybe Label)
  deriving (Show, Eq)

parseTheoremPreamble :: Parser TheoremPreamble
parseTheoremPreamble = TheoremPreamble <$> (parseLitTheorem *> option parseLabel <* parsePeriod)

data AffirmProof =
    AffirmProofStatementProof StatementProof
  | AffirmProofGoalProof GoalProof
  deriving (Show, Eq)


parseAffirmProof :: Parser AffirmProof
parseAffirmProof =
  AffirmProofStatementProof <$> parseStatementProof <||>
  AffirmProofGoalProof <$> parseGoalProof

data GoalProof = GoalProof GoalPrefix Statement ByRef ProofScript
  deriving (Show, Eq)

parseGoalProof :: Parser GoalProof
parseGoalProof = GoalProof <$> parseGoalPrefix <*> parseStatement <*> parseByRef <* parsePeriod <*> parseProofScript

newtype GoalPrefix = GoalPrefix ByMethod
  deriving (Show, Eq)

parseGoalPrefix :: Parser GoalPrefix
parseGoalPrefix = GoalPrefix <$> (option parseLitLets *> parseLitProve *> parseByMethod) <* option (parseLit "that")

newtype ByMethod = ByMethod (Maybe ProofMethod)
  deriving (Show, Eq)

parseByMethod :: Parser ByMethod
parseByMethod =  ByMethod <$> (option $ parseLit "by" *> parseProofMethod)

data StatementProof = StatementProof Statement ByRef (Maybe ProofScript)
  deriving (Show, Eq)

parseStatementProof :: Parser StatementProof
parseStatementProof =
  StatementProof <$> (parseThenPrefix *> parseStatement) <*> parseByRef <* parsePeriod <*> option parseProofScript

newtype ByRef = ByRef (Maybe RefItem)
  deriving (Show, Eq)

parseByRef :: Parser ByRef
parseByRef = ByRef <$> (option $ paren $ (parseLit "by" *> parseRefItem))

newtype RefItem = RefItem [(Maybe [Text], Label)]
  deriving (Show, Eq)

parseRefItem :: Parser RefItem
parseRefItem = RefItem <$> sep_list ((,) <$> option parseLitLocation <*> parseLabel)

----------------
-- PROOF SCRIPTS
----------------

data ProofScript = ProofScript ProofPreamble (Maybe ([(CannedPrefix, ProofBody)], CannedPrefix, ProofTail)) 
  deriving (Show, Eq)

parseProofScript :: Parser ProofScript
parseProofScript = ProofScript <$>
  parseProofPreamble <*>
  (option $ ((,,) <$> (many' $
                      ((,) <$> parseCannedPrefix <*> parseProofBody))
                               <*> parseCannedPrefix <*> parseProofTail) <*
  parseLitQED <* parsePeriod)

data ProofPreamble =
    ProofPreambleByMethod ByMethod
  | ProofPreambleIndeed
  deriving (Show, Eq)

parseProofPreamble :: Parser ProofPreamble
parseProofPreamble =
  ProofPreambleByMethod <$> (parseLit "proof" *> parseByMethod) <* parsePeriod <||>
  parseLit "indeed" *> (option parseComma) *> return ProofPreambleIndeed

data ProofBody =
    ProofBodyProofTail ProofTail
  | ProofBodyAssumption Assumption
  deriving (Show, Eq)

parseProofBody :: Parser ProofBody
parseProofBody =
  ProofBodyProofTail <$> parseProofTail <||>
  ProofBodyAssumption <$> parseAssumption

data ProofTail =
    ProofTailAffirmProof AffirmProof
  | ProofTailCannedProof CannedProof
  | ProofTailCase Case
  | ProofTailChoose Choose
  deriving (Show, Eq)

parseProofTail :: Parser ProofTail
parseProofTail =
  ProofTailAffirmProof <$> parseAffirmProof <||>
  ProofTailCannedProof <$> parseCannedProof <||>
  ProofTailCase <$> parseCase <||>
  ProofTailChoose <$> parseChoose

newtype CannedProof = CannedProof PhraseListProofStatement
  deriving (Show, Eq)

parseCannedProof :: Parser CannedProof
parseCannedProof = CannedProof <$> parsePhraseListProofStatement

newtype CannedPrefix = CannedPrefix [PhraseListTransition]
  deriving (Show, Eq)

parseCannedPrefix :: Parser CannedPrefix
parseCannedPrefix = CannedPrefix <$> (sep_list parsePhraseListTransition) <* option parseComma

data Case = Case Statement ProofScript
  deriving (Show, Eq)

parseCase :: Parser Case
parseCase = Case <$> (parseLit "case" *> parseStatement <* parsePeriod) <*> parseProofScript

data Choose = Choose ChoosePrefix NamedTerms ByRef ChooseJustify
  deriving (Show, Eq)

parseChoose :: Parser Choose
parseChoose = Choose <$> parseChoosePrefix <*> parseNamedTerms <*> parseByRef <* parsePeriod <*> parseChooseJustify

newtype ChoosePrefix = ChoosePrefix ThenPrefix
  deriving (Show, Eq)

parseChoosePrefix :: Parser ChoosePrefix
parseChoosePrefix = ChoosePrefix <$> parseThenPrefix

data ChooseJustify =
    ChooseJustifyProofScript (Maybe ProofScript)
  deriving (Show, Eq)

parseChooseJustify :: Parser ChooseJustify
parseChooseJustify =
  ChooseJustifyProofScript <$> option parseProofScript

data ProofMethod =
    ProofMethodContradiction
  | ProofMethodCaseAnalysis
  | ProofMethodInduction (Maybe PlainTerm)
  deriving (Show, Eq)
  
parseProofMethod :: Parser ProofMethod
parseProofMethod =
  parseLit "contradiction" *> return ProofMethodContradiction <||>
  parseLit "case" *> parseLit "analysis" *> return ProofMethodCaseAnalysis <||>
  ProofMethodInduction <$> (parseLit "induction" *> (option $ parseLit "on" *> parsePlainTerm))
