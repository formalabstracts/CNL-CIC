{-
Author(s): Jesse Michael Han (2019)

Macros.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Macro where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, option, Label, Tokens)
import Control.Monad (guard, liftM)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Monad.Trans.State.Lazy (modify, gets)

import CNLean.Basic.Basic
import CNLean.Definition
import CNLean.Type

data Macro = Macro (Maybe InSection) [Assuming] MacroBodies
  deriving (Show, Eq)

parseMacro :: Parser Macro
parseMacro = Macro <$> (option parseInSection) <*> sep_list parseAssuming <*> parseMacroBodies

newtype InSection = InSection SectionTag
  deriving (Show, Eq)

parseInSection :: Parser InSection
parseInSection = InSection <$> (parseLit "in" *> parseLit "this" *> parseSectionTag)

newtype SectionTag = SectionTag [Text]
  deriving (Show, Eq)

parseSectionTag :: Parser SectionTag
parseSectionTag = SectionTag <$> parseLitDocument

newtype Assuming = Assuming Statement
  deriving (Show, Eq)

parseAssuming :: Parser Assuming
parseAssuming = Assuming <$> (parseLit "assuming" *> (option $ parseLit "that") *> parseStatement)

data MacroBodies = MacroBodies MacroBody [MacroBody]
  deriving (Show, Eq)

parseMacroBodies :: Parser MacroBodies
parseMacroBodies =
  MacroBodies <$> parseMacroBody <*>
    (many' $ parseSemicolon *> option (parseLit "and") *> parseMacroBody) <* parsePeriod

data MacroBody =
    MacroBodyClassifierDef ClassifierDef
  | MacroBodyTypeDef TypeDef
  | MacroBodyFunctionDef FunctionDef
  | MacroBodyPredicateDef PredicateDef
  | MacroBodyLetAnnotation LetAnnotation
  | MacroBodyWeRecordDef WeRecordDef
  deriving (Show, Eq)

parseMacroBody :: Parser MacroBody
parseMacroBody =
  MacroBodyClassifierDef <$> parseClassifierDef <||>
  MacroBodyTypeDef <$> parseTypeDef <||>
  MacroBodyFunctionDef <$> parseFunctionDef <||>
  MacroBodyPredicateDef <$> parsePredicateDef <||>
  MacroBodyLetAnnotation <$> parseLetAnnotation <||>
  MacroBodyWeRecordDef <$> parseWeRecordDef 

newtype WeRecordDef = WeRecordDef [PlainTerm]
  deriving (Show, Eq)

parseWeRecordDef :: Parser WeRecordDef
parseWeRecordDef = WeRecordDef <$> (parseLitWeRecord *> comma_nonempty_list parsePlainTerm)
