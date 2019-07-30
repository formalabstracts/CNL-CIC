{-
Author(s): Jesse Michael Han (2019)

Parsing theorems.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Theorem where

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

import CNLean.Basic.Basic
import CNLean.Assumption
import CNLean.Type

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
parseByMethod =  ByMethod <$> option $ parseLit "by" *> parseProofMethod


data StatementProof = StatementProof Statement ByRef (Maybe ProofScript)
  deriving (Show, Eq)

parseStatementProof :: Parser StatementProof
parseStatementProof =
  StatementProof <$> (parseThenPrefix *> parseStatement) <*> parseByRef <* parsePeriod <*> option parseProofScript

newtype ByRef = ByRef (Maybe RefItem)
  deriving (Show, Eq)

parseByRef :: Parser ByRef
parseByRef = ByRef <$> (option $ paren $ (parseLit "by" *> parseRefItem))

newtype RefItem = RefItem [(Maybe Text, Label)]
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
  option $ ((,,) <$> (many' $
                      ((,) <$> parseCannedPrefix <*> parseProofBody))
                               <*> parseCannedPrefix <*> parseCannedTail) <*
  parseLitQED <* parsePeriod

data ProofPreamble =
    ProofPreambleByMethod ByMethod
  | ProofPreambleIndeed
  deriving (Show, Eq)

parseProofPreamble :: Parser ProofPreamble
parseProofPreamble =
  ProofPreambleByMethod <$> (parseLit "proof" *> parseByMethod) <* parsePeriod <||>
  parseLit "indeed" *> return ProofPreambleIndeed

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

newtype CannedPrefix = CannedPrefix [PhraseListTransition]
  deriving (Show, Eq)

parseCannedPrefix :: Parser CannedPrefix
parseCannedPrefix = CannedPrefix <$> (sep_list parsePhraseListTransition) <* option parseComma

-- TODO(jesse): fill in remaining holes.
