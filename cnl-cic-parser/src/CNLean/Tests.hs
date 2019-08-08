{-
Author(s): Jesse Michael Han (2019)

Tests, for preliminary debugging. This will eventually be merged into the test suite and the absolute paths will be removed.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Tests

(
    module CNLean.Definition
  , module CNLean.Core
  , module CNLean.Instr
  , module CNLean.Axiom
  , module CNLean.Theorem
  , module CNLean.SectionPreamble
  , module CNLean.Macro
  , module CNLean.Type
  , module CNLean.Basic.Basic
  , module CNLean.Assumption
  , module CNLean.Tests
)

where

import Prelude -- hiding (Int, Bool, String, drop)
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

import CNLean.Basic.Basic
import CNLean.Core
import CNLean.Type
import CNLean.Definition
import CNLean.Axiom
import CNLean.Theorem
import CNLean.Macro
import CNLean.Instr
import CNLean.SectionPreamble
import CNLean.Assumption

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

testCore :: IO ()
testCore = return ()

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
  test parseTokenPattern "foo bar baz a foo b bar c baz"
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

testTests :: IO ()
testTests = do
  testAxiom
  testDefinition
  testTheorem
  testType
  testMacro
  testInstr
  testCore
  testState
