{-
Author(s): Jesse Michael Han (2019)

Tests, for preliminary debugging. This will eventually be merged into the test suite and the absolute paths will be removed.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}


module Colada.Tests

(
    module Colada.Definition
  , module Colada.ProgramText
  , module Colada.Instr
  , module Colada.Axiom
  , module Colada.Theorem
  , module Colada.SectionPreamble
  , module Colada.Macro
  , module Colada.Type
  , module Colada.Basic.Basic
  , module Colada.Assumption
  , module Colada.Tests
  , module Colada.PhraseList
  
)

where

import Prelude hiding (Word) -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.Foldable
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import Control.Lens

import Colada.Basic.Basic
import Colada.ProgramText
import Colada.Type
import Colada.Definition
import Colada.Axiom
import Colada.Theorem
import Colada.Macro
import Colada.Instr
import Colada.SectionPreamble
import Colada.Assumption
import Colada.PhraseList

testMacro :: IO ()
testMacro = return ()

testTheorem :: IO ()
testTheorem = return ()

testAxiom :: IO ()
testAxiom = do
  test parseAxiom "Axiom The_riemann_hypothesis. Zero is not positive."
  test parseAxiomPreamble "Axiom riemann_hypothesis."
  test parseThenPrefix "therefore"
  test parseAxiom "Axiom The_Riemann_Hypothesis. One is positive."
  test parseAxiom "Axiom The_Riemann_Hypothesis. The zero is not positive."

testProgramText :: IO ()
testProgramText = return ()

examplePatts :: Pattern
examplePatts = Patts [Wd ["foo"], Wd ["bar"], Vr]

examplePatts2 :: Pattern
examplePatts2 = Patts [Wd["subsets", "subset"], Nm, Wd["of"], Vr]
-- note, prefixes must appear later in the list because they will succeed first

testType :: IO ()
testType = do
  test parseVarType "(x : Type)"
  test parseVarType "x"
  test (parsePattern examplePatts) "foo bar a1"
  test (parsePattern examplePatts2) "subset a of x0"
  test (parsePattern examplePatts2) "subsets a,b of x0"

testDefinition :: IO ()
testDefinition = do
  test parseDefinition "Definition The_Riemann_zeta_function. The riemann zeta function of x is zero. This exists and is well-defined."
  test parseDefinitionPreamble "DEFINITION foo."
  test parseDefinitionPreamble "DEF."
  test parseThisExists "this exists and is well defined and is canonical"
  test parseThisDirectiveAdjective "well-defined"
  test parseWordPattern "foo bar baz a foo b bar c baz"
  test parseClassifierDef "let scheme, schemes, stacks be classifiers"
  test (parseClassifierDef *> (use $ top.clsList)) "let scheme, schemes, stacks, derived stacks be classifiers"
  test parseClassifierDef "let lattice be a classifier"

testInstr :: IO ()
testInstr = do
  test (parseInstr *> use (allStates strSyms)) "[synonym foo/bar/baz]"
  test parseInstr "[synonym foo/bar/baz]"

testState :: IO ()
testState = do
  test ((updateStrSyms Globally ["foo"] *> (use $ top . strSyms) *> empty) <||> (updateStrSyms Globally ["bar"] *> (use $ top . strSyms))) "foo"
  test (use (allStates idCount)) "foo"

testPhraseList :: IO ()
testPhraseList = do
  test parsePhraseListFiller_aux "we have"
  test parsePhraseListFiller_aux "put"
  test (fail_iff_succeeds parsePhraseListFiller_aux) "ramalamadingdong"
  test parsePhraseListTransition_aux "without loss of generality"
  test parsePhraseListProofStatement_aux "the theorem now follows."
  test parsePhraseListProofStatement_aux "the theorem follows."

-- testCustom :: IO ()
-- testCustom = do
--   txt <- TIO.readFile "/home/pv/org/projects/jmh-CNL-CIC/colada-parser/test/test_script2.txt" -- TODO remove absolute path
--   test parseProgram txt

testTests :: IO ()
testTests = do
  testAxiom
  testDefinition
  testTheorem
  testType
  testMacro
  testInstr
  testPhraseList
  testProgramText
  testState
  testSectionPreambleHandler
  putStrLn "all tests passed"
