{-
Author(s): Jesse Michael Han (2019)

Tests, for preliminary debugging. This will eventually be merged into the test suite and the absolute paths will be removed.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Tests where

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

-- note: the state, and all stateful side-effects, backtracks if the nearest parser fails in the orelse combinator:
-- test ((updateStrSyms Globally ["foo"] *> (use $ top . strSyms) *> empty) <||> (updateStrSyms Globally ["bar"] *> (use $ top . strSyms))) "foo"

-- test (use (allStates idCount)) "foo"

-- test parseDefinition "Definition The_Riemann_zeta_function. The riemann zeta function of x is zero. This exists and is well-defined."

-- test parseAxiom "Axiom The_riemann_hypothesis. Zero is not positive."
