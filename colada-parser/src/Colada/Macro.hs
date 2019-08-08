{-
Author(s): Jesse Michael Han (2019)

Macros.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Macro where

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
import Control.Monad.Trans.State
import Control.Monad.Trans.State.Lazy (modify, gets)

import Control.Lens

import Colada.Basic.Basic
import Colada.Definition
import Colada.Type
import Colada.SectionPreamble

data Macro = Macro (Maybe InSection) MacroBodies
  deriving (Show, Eq)

parseMacro :: Parser Macro
parseMacro = do
  result@(Macro mis mbs) <- Macro <$> (option parseInSection) <*> parseMacroBodies
  parseMacro_aux mis mbs
  return result
  where
    parseMacro_aux :: (Maybe InSection) -> MacroBodies -> Parser ()
    parseMacro_aux mis mbs =
      case mis of
        Nothing -> parseMacro_aux (Just (InSection 0)) mbs -- if no level is given, then automatically modify global state
        (Just (InSection k)) -> registerMacroBodies mbs (AtLevel k)

newtype InSection = InSection Int
  deriving (Show, Eq)

parseInSection :: Parser InSection
parseInSection = InSection <$> (parseLit "in" *> parseLit "this" *> do {(a,b) <- parseSectionTag_aux; return b})

newtype Assuming = Assuming Statement
  deriving (Show, Eq)

parseAssuming :: Parser Assuming
parseAssuming = Assuming <$> (parseLit "assuming" *> (option $ parseLit "that") *> parseStatement)

data MacroBodies = MacroBodies MacroBody [MacroBody]
  deriving (Show, Eq)

registerMacroBodies :: MacroBodies -> LocalGlobalFlag -> Parser ()
registerMacroBodies (MacroBodies mb mbs) lgflag =
  run_all $ map (registerMacroBody lgflag) (mb:mbs)

parseMacroBodies :: Parser MacroBodies
parseMacroBodies =
  MacroBodies <$> parseMacroBody <*>
    (many' $ parseSemicolon *> option (parseLit "and") *> parseMacroBody) <* parsePeriod

data MacroBody =
    MacroBodyClassifierDef ClassifierDef
  | MacroBodyTypeDef TypeDef
  | MacroBodyFunctionDef FunctionDef (Maybe MacroWhere)
  | MacroBodyPredicateDef PredicateDef (Maybe MacroWhere)
  | MacroBodyLetAnnotation LetAnnotation
  | MacroBodyWeRecordDef WeRecordDef
  deriving (Show, Eq)

newtype MacroWhere = MacroWhere [AnnotatedVars]
  deriving (Show, Eq)

parseMacroWhere :: Parser MacroWhere
parseMacroWhere = MacroWhere <$> ((parseLit "where") *> many1' (parseAnnotatedVars))

registerMacroBody :: LocalGlobalFlag -> MacroBody -> Parser ()
registerMacroBody lgflag mb =
  case lgflag of
    Globally -> registerMacroBody (AtLevel 0) mb
    Locally -> do {d <- depthStack <$> get; registerMacroBody (AtLevel d) mb}
    AtLevel k -> case mb of
      (MacroBodyClassifierDef c) -> registerClassifierDef (AtLevel k) c -- TODO(jesse): fix this
      (MacroBodyTypeDef x) -> registerTypeDefMacro (AtLevel k) x
      (MacroBodyFunctionDef x w) -> registerFunctionDefMacro (AtLevel k)  x
      (MacroBodyPredicateDef x w) -> registerPredicateDefMacro (AtLevel k) x
      (MacroBodyLetAnnotation x) -> empty -- currently not supported in the state
      (MacroBodyWeRecordDef x) -> empty -- currently not supported in the state

parseMacroBody :: Parser MacroBody
parseMacroBody =
  MacroBodyClassifierDef <$> parseClassifierDef <||>
  MacroBodyTypeDef <$> parseTypeDef <||>
  MacroBodyFunctionDef <$> parseFunctionDef <*> option parseMacroWhere <||>
  MacroBodyPredicateDef <$> parsePredicateDef <*> option parseMacroWhere <||>
  MacroBodyLetAnnotation <$> parseLetAnnotation <||>
  MacroBodyWeRecordDef <$> parseWeRecordDef

newtype WeRecordDef = WeRecordDef [PlainTerm]
  deriving (Show, Eq)

parseWeRecordDef :: Parser WeRecordDef
parseWeRecordDef = WeRecordDef <$> (parseLitWeRecord *> comma_nonempty_list parsePlainTerm)
