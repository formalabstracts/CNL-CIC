{-
Author(s): Jesse Michael Han (2019)

Parsing instructions.
-}

{-# LANGUAGE OverloadedStrings #-}
module Colada.Instr where

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
import Control.Monad.Trans.State.Lazy (modify, gets)

import Control.Lens

import Colada.Basic.Basic

data Instr =
    InstrInstructCommand InstructCommand
  | InstrInstructSynonym InstructSynonym
  | InstrInstructString InstructString
  | InstrInstructBool InstructBool
  | InstrInstructInt InstructInt
  deriving (Show, Eq)

parseInstr :: Parser Instr
parseInstr =
  InstrInstructCommand <$> parseInstructCommand <||>
  InstrInstructSynonym <$> parseInstructSynonym <||>
  InstrInstructString <$> parseInstructString <||>
  InstrInstructBool <$> parseInstructBool <||>
  InstrInstructInt <$> parseInstructInt

data InstructCommand = InstructCommand InstructKeywordCommand
  deriving (Show, Eq)

parseInstructCommand :: Parser InstructCommand
parseInstructCommand = bracket $ InstructCommand <$> parseInstructKeywordCommand

data InstructKeywordCommand =
  LitExit
  deriving (Show, Eq)

parseInstructKeywordCommand :: Parser InstructKeywordCommand
parseInstructKeywordCommand = parseLit "exit" *> return LitExit

data InstructSynonym = InstructSynonym [Token]
  deriving (Show, Eq)

parseInstructSynonym :: Parser InstructSynonym
parseInstructSynonym = with_result (parse_synonym_main) m
  where
    parse_synonym_main = InstructSynonym <$> do
      bracket $ (parseLit "synonyms" <||> parseLit "synonym") *>
        parseToken1 >>= rest . pure
          where
            rest :: [Token] -> Parser [Token]
            rest syms = case (last syms) of
              Token txt ->
                (parseInstructSepPlural *> (rest $ syms <> [Token $ txt <> "s"])) <||>
                (parseInstructSep *> parseToken1 >>= \x -> (rest $ syms <> [x])) <||>
                return syms
    m = updateGlobalStrSyms . (\(InstructSynonym y) -> (map tokenToText y))

-- TODO: allow parsing of arbitrary postfixes after parsing a "/-"
data InstructSepPlural = InstructSepPlural
  deriving (Show, Eq)

parseInstructSepPlural :: Parser InstructSepPlural
parseInstructSepPlural = parseSlash *> parseLit "-" *> parseLit "s" *> return InstructSepPlural

data InstructSep = InstructSep
  deriving (Show, Eq)

parseInstructSep :: Parser InstructSep
parseInstructSep = parseSlash *> return InstructSep
  
data InstructString = InstructString InstructKeywordString TkString
  deriving (Show, Eq)

parseInstructString :: Parser InstructString
parseInstructString = bracket $ InstructString <$> parseInstructKeywordString <*> parseTkString
  
data InstructKeywordString =
    LitRead
  | LitLibrary
  deriving (Show, Eq)

parseInstructKeywordString :: Parser InstructKeywordString
parseInstructKeywordString =
  parseLit "read" *> return LitRead <||> parseLit "library" *> return LitLibrary

data InstructBool = InstructBool InstructKeywordBool Bool
  deriving (Show, Eq)

parseInstructBool :: Parser InstructBool
parseInstructBool = bracket $ InstructBool <$> parseInstructKeywordBool <*> parseBool
  
data InstructKeywordBool =
    LitPrintGoal
  | LitDump
  | LitOntored
  deriving (Show, Eq)

parseInstructKeywordBool :: Parser InstructKeywordBool
parseInstructKeywordBool =
  (parseLit "print" *> parseLit "goal") *> return LitPrintGoal <||>
  (parseLit "dump") *> return LitDump <||>
  (parseLit "ontored") *> return LitOntored

data InstructInt = InstructInt InstructKeywordInt Number
  deriving (Show, Eq)

parseInstructInt :: Parser InstructInt
parseInstructInt = bracket $ InstructInt <$> parseInstructKeywordInt <*> parseNumber

data InstructKeywordInt =
  LitTimeLimit
  deriving (Show, Eq)

parseInstructKeywordInt :: Parser InstructKeywordInt
parseInstructKeywordInt = parseLit "time" *> parseLit "limit" *> return LitTimeLimit
