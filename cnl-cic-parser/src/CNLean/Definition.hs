{-
Author(s): Jesse Michael Han (2019)

Parsing definitions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Definition where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, option, Label, Tokens)
import Control.Monad (guard, liftM)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.List (intersperse)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Monad.Trans.State.Lazy (modify, gets)

import Control.Lens

import CNLean.Basic.Basic
import CNLean.Type
import CNLean.Assumption
import CNLean.Pattern

data Definition = Definition DefinitionPreamble [Assumption] DefinitionAffirm
  deriving (Show, Eq)

-- TODO(jesse): refactor "register" functions to accept a LocalGlobalFlag and an (isMacro :: Bool) argument

registerDefinition :: Definition -> Parser ()
registerDefinition def@(Definition dp asms (DefinitionAffirm ds _)) =
  case ds of
    (DefinitionStatementClassifier x) -> registerClassifierDef Globally x
    (DefinitionStatementTypeDef x) -> registerTypeDef Globally x
    (DefinitionStatementFunctionDef x) -> registerFunctionDef Globally x
    (DefinitionStatementPredicateDef x) -> registerPredicateDef Globally x

parseDefinition :: Parser Definition
parseDefinition =
  with_any_result parse_definition_main side_effects
  where
    parse_definition_main = Definition <$> parseDefinitionPreamble <*> (many' parseAssumption) <*> parseDefinitionAffirm

    side_effects = [registerDefinition]

newtype DefinitionPreamble = DefinitionPreamble (Maybe Label)
  deriving (Show, Eq)

parseDefinitionPreamble :: Parser DefinitionPreamble
parseDefinitionPreamble = DefinitionPreamble <$> (parseLitDef *> option parseLabel <* parsePeriod)

-- test parseDefinitionPreamble "DEFINITION foo."

-- test parseDefinitionPreamble "DEF."

data DefinitionAffirm = DefinitionAffirm DefinitionStatement (Maybe ThisExists)
  deriving (Show, Eq)

parseDefinitionAffirm :: Parser DefinitionAffirm
parseDefinitionAffirm = DefinitionAffirm <$> parseDefinitionStatement <* parsePeriod <*> option (parseThisExists)
  
newtype ThisExists = ThisExists [ThisDirectivePred]
  deriving (Show, Eq)

parseThisExists :: Parser ThisExists
parseThisExists = parseLit "this" *> (ThisExists <$> sep_list(parseThisDirectivePred))

-- test parseThisExists "this exists and is well defined and is canonical"

data ThisDirectivePred =
    ThisDirectivePredAdjective [[Text]]
  | ThisDirectivePredVerb ThisDirectiveVerb
  deriving (Show, Eq)

parseThisDirectivePred :: Parser ThisDirectivePred
parseThisDirectivePred =
  ThisDirectivePredAdjective <$> (parseLit "is" *> (sep_list1 parseThisDirectiveAdjective)) <||>
  ThisDirectivePredVerb <$> parseThisDirectiveVerb

parseThisDirectiveAdjective :: Parser [Text]
parseThisDirectiveAdjective =
  parse_any_of $ map (\x -> parse_list x parseLit) thisDirAdjList
  where
    thisDirAdjList :: [[Text]]
    thisDirAdjList =
      [
        ["unique"],
        ["canonical"],
        ["welldefined"],
        ["well-defined"],
        ["well", "defined"],
        ["total"],
        ["well", "propped"],
        ["exhaustive"]
      ]

-- test parseThisDirectiveAdjective "well-defined"

newtype ThisDirectiveVerb = ThisDirectiveVerbExists (Maybe ThisDirectiveRightAttr)
  deriving (Show, Eq)

parseThisDirectiveVerb = parseLit "exists" *> (ThisDirectiveVerbExists <$> option parseThisDirectiveRightAttr)

newtype ThisDirectiveRightAttr = ThisDirectiveRightAttr [Text]
  deriving (Show, Eq)

parseThisDirectiveRightAttr :: Parser ThisDirectiveRightAttr
parseThisDirectiveRightAttr = ThisDirectiveRightAttr <$> (parse_list ["by", "recursion"] parseLit)

data DefinitionStatement =
    DefinitionStatementClassifier ClassifierDef
  | DefinitionStatementTypeDef TypeDef
  | DefinitionStatementFunctionDef FunctionDef
  | DefinitionStatementPredicateDef PredicateDef
  -- | DefinitionStatementStructureDef  StructureDef
  -- | DefinitionStatementInductiveDef InductiveDef
  -- | DefinitionStatementMutualInductiveDef MutualInductiveDef
  deriving (Show, Eq)

parseDefinitionStatement :: Parser DefinitionStatement
parseDefinitionStatement =
  DefinitionStatementClassifier <$> parseClassifierDef <||>
  DefinitionStatementTypeDef <$> parseTypeDef <||>
  DefinitionStatementFunctionDef <$> parseFunctionDef <||>
  DefinitionStatementPredicateDef <$> parsePredicateDef--  <||>
  -- DefinitionStatementStructureDef <$> parseStructureDef <||>
  -- DefinitionStatementInductiveDef <$> parseInductiveDef <||>
  -- DefinitionStatementMutualInductiveDef <$> parseMutualInductiveDef

data PredicateDef = PredicateDef PredicateHead IffJunction Statement
  deriving (Show, Eq)

registerPrimAdjective :: LocalGlobalFlag ->  PredicateDef -> Parser () -- (* from adjective_pattern *)
registerPrimAdjective lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternAdjectivePattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimAdjective lgflag
    _ -> empty

registerPrimAdjectiveMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () -- (* from adjective_pattern *)
registerPrimAdjectiveMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternAdjectivePattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimAdjective lgflag . toMacroPatts
    _ -> empty

registerPrimAdjectiveMultiSubject :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from adjective_multisubject_pattern *)
registerPrimAdjectiveMultiSubject lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternAdjectiveMultiSubjectPattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimAdjectiveMultiSubject lgflag
    _ -> empty

registerPrimAdjectiveMultiSubjectMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from adjective_multisubject_pattern *)
registerPrimAdjectiveMultiSubjectMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternAdjectiveMultiSubjectPattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimAdjectiveMultiSubject lgflag . toMacroPatts
    _ -> empty

registerPrimVerb :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from verb_pattern *)
registerPrimVerb lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternVerbPattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimVerb lgflag
    _ -> empty

registerPrimVerbMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from verb_pattern *)
registerPrimVerbMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternVerbPattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimVerb lgflag . toMacroPatts
    _ -> empty

registerPrimVerbMultiSubject :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from verb_multiset_pattern *)
registerPrimVerbMultiSubject lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternVerbMultiSubjectPattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimVerbMultiSubject lgflag
    _ -> empty

registerPrimVerbMultiSubjectMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from verb_multiset_pattern *)
registerPrimVerbMultiSubjectMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadPredicateTokenPattern (PredicateTokenPatternVerbMultiSubjectPattern adjpatt))
      -> patternOfPredicateDef pd >>= updatePrimVerbMultiSubject lgflag . toMacroPatts
    _ -> empty

registerPrimRelation :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from predicate_def.identifier_pattern *)
registerPrimRelation lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadIdentifierPattern idpatt)
      -> patternOfPredicateDef pd >>= updatePrimRelation lgflag
    _ -> empty

registerPrimRelationMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from predicate_def.identifier_pattern *)
registerPrimRelationMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadIdentifierPattern idpatt)
      -> patternOfPredicateDef pd >>= updatePrimRelation lgflag . toMacroPatts
    _ -> empty

registerPrimPropositionalOp :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from predicate_def.symbol_pattern, with prec < 0 *)
registerPrimPropositionalOp lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern (SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- isNegativePrecedence mpl
            if b then patternOfPredicateDef pd >>= updatePrimPropositionalOp lgflag
                 else empty
    _ -> empty

registerPrimPropositionalOpMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () --  (* from predicate_def.symbol_pattern, with prec < 0 *)
registerPrimPropositionalOpMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern (SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- isNegativePrecedence mpl
            if b then patternOfPredicateDef pd >>= updatePrimPropositionalOp lgflag . toMacroPatts
                 else empty
    _ -> empty

registerPrimBinaryRelationOp :: LocalGlobalFlag ->  PredicateDef -> Parser () --   (* from predicate_def.symbol_pattern, binary infix with prec=0 or none  *)
registerPrimBinaryRelationOp lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- (|| isNothing mpl) <$> (isZeroPrecedence mpl)
            if b && (isBinarySymbolPattern sympatt) then patternOfPredicateDef pd >>= updatePrimBinaryRelationOp lgflag
                 else empty
    _ -> empty

registerPrimBinaryRelationOpMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () --   (* from predicate_def.symbol_pattern, binary infix with prec=0 or none  *)
registerPrimBinaryRelationOpMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- (|| isNothing mpl) <$> (isZeroPrecedence mpl)
            if b && (isBinarySymbolPattern sympatt) then patternOfPredicateDef pd >>= updatePrimBinaryRelationOp lgflag . toMacroPatts
                 else empty
    _ -> empty
    

registerPrimBinaryRelationControlSeq :: LocalGlobalFlag ->  PredicateDef -> Parser () -- (* from predicate_def.binary_controlseq_pattern, binary, prec=0 or none *)
registerPrimBinaryRelationControlSeq lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- (|| isNothing mpl) <$> (isZeroPrecedence mpl)
            if b && (isBinaryControlSeqSymbolPattern sympatt) then patternOfPredicateDef pd >>= updatePrimBinaryRelationControlSeq lgflag
                 else empty
    _ -> empty

registerPrimBinaryRelationControlSeqMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () -- (* from predicate_def.binary_controlseq_pattern, binary, prec=0 or none *)
registerPrimBinaryRelationControlSeqMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- (|| isNothing mpl) <$> (isZeroPrecedence mpl)
            if b && (isBinaryControlSeqSymbolPattern sympatt) then patternOfPredicateDef pd >>= updatePrimBinaryRelationControlSeq lgflag . toMacroPatts
                 else empty
    _ -> empty

registerPrimPropositionalOpControlSeq :: LocalGlobalFlag ->  PredicateDef -> Parser () -- (* from predicate_def.binary_controlseq_pattern, prec < 0 *)
registerPrimPropositionalOpControlSeq lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- (isNegativePrecedence mpl)
            if b && (isBinaryControlSeqSymbolPattern sympatt) then patternOfPredicateDef pd >>= updatePrimPropositionalOpControlSeq lgflag
                 else empty
    _ -> empty

registerPrimPropositionalOpControlSeqMacro :: LocalGlobalFlag ->  PredicateDef -> Parser () -- (* from predicate_def.binary_controlseq_pattern, prec < 0 *)
registerPrimPropositionalOpControlSeqMacro lgflag pd@(PredicateDef ph iffj stmt) =
  case ph of
    (PredicateHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl)
      -> do b <- (isNegativePrecedence mpl)
            if b && (isBinaryControlSeqSymbolPattern sympatt) then patternOfPredicateDef pd >>= updatePrimPropositionalOpControlSeq lgflag . toMacroPatts
                 else empty
    _ -> empty

registerPredicateDef :: LocalGlobalFlag -> PredicateDef -> Parser () 
registerPredicateDef lgflag pd = with_any_result (return pd) side_effects *> skip
  where
        side_effects = [
          registerPrimAdjective lgflag,
          registerPrimAdjectiveMultiSubject lgflag,
          registerPrimVerb lgflag,
          registerPrimRelation lgflag,
          registerPrimVerbMultiSubject lgflag,
          registerPrimPropositionalOp lgflag,
          registerPrimBinaryRelationOp lgflag,
          registerPrimBinaryRelationControlSeq lgflag,
          registerPrimPropositionalOpControlSeq lgflag
                       ]

registerPredicateDefMacro :: LocalGlobalFlag -> PredicateDef -> Parser () 
registerPredicateDefMacro lgflag pd = with_any_result (return pd) side_effects *> skip
  where
        side_effects = [
          registerPrimAdjectiveMacro lgflag,
          registerPrimAdjectiveMultiSubjectMacro lgflag,
          registerPrimVerbMacro lgflag,
          registerPrimRelationMacro lgflag,
          registerPrimVerbMultiSubjectMacro lgflag,
          registerPrimPropositionalOpMacro lgflag,
          registerPrimBinaryRelationOpMacro lgflag,
          registerPrimBinaryRelationControlSeqMacro lgflag,
          registerPrimPropositionalOpControlSeqMacro lgflag
                       ]


parsePredicateDef :: Parser PredicateDef
parsePredicateDef = 
          PredicateDef <$> (parseOptSay *> parsePredicateHead)<*> parseIffJunction <*> parseStatement

patternOfPredicateDef :: PredicateDef -> Parser Pattern
patternOfPredicateDef fd = case fd of
  PredicateDef predicatehead iffjunction statement -> case predicatehead of
    PredicateHeadPredicateTokenPattern (predtkpatt) -> patternOfPredicateTokenPattern predtkpatt
    PredicateHeadIdentifierPattern idpatt -> patternOfIdentifierPattern idpatt
    PredicateHeadSymbolPattern sympatt mpl -> patternOfSymbolPattern sympatt

--TODO(jesse): add side effects for predicate definitions
-- identifier pattern becomes primRelation
-- Symbolpattern becomes... binary?

data IffJunction = IffJunction
  deriving (Show, Eq)

parseIffJunction = parseLitIff *> return IffJunction

data PredicateHead =
    PredicateHeadPredicateTokenPattern PredicateTokenPattern
  | PredicateHeadSymbolPattern SymbolPattern (Maybe ParenPrecedenceLevel)
  | PredicateHeadIdentifierPattern IdentifierPattern
  -- | PredicateHeadControlSeqPattern ControlSeqPattern
  -- | PredicateHeadBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parsePredicateHead :: Parser PredicateHead
parsePredicateHead =
  PredicateHeadPredicateTokenPattern <$> parsePredicateTokenPattern <||>
  PredicateHeadSymbolPattern <$> parseSymbolPattern <*> (option parseParenPrecedenceLevel) <||>
  PredicateHeadIdentifierPattern <$> parseIdentifierPattern--  <||>
  -- PredicateHeadControlSeqPattern <$> parseControlSeqPattern <||>
  -- PredicateHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data PredicateTokenPattern =
    PredicateTokenPatternAdjectivePattern AdjectivePattern
  | PredicateTokenPatternAdjectiveMultiSubjectPattern AdjectiveMultiSubjectPattern
  | PredicateTokenPatternVerbPattern VerbPattern
  | PredicateTokenPatternVerbMultiSubjectPattern VerbMultiSubjectPattern
  deriving (Show, Eq)

parsePredicateTokenPattern :: Parser PredicateTokenPattern
parsePredicateTokenPattern =
  PredicateTokenPatternAdjectivePattern <$> parseAdjectivePattern <||>
  PredicateTokenPatternAdjectiveMultiSubjectPattern <$> parseAdjectiveMultiSubjectPattern <||>
  PredicateTokenPatternVerbPattern <$> parseVerbPattern <||>
  PredicateTokenPatternVerbMultiSubjectPattern <$> parseVerbMultiSubjectPattern

patternOfPredicateTokenPattern :: PredicateTokenPattern -> Parser Pattern
patternOfPredicateTokenPattern ptkpatt = case ptkpatt of
  (PredicateTokenPatternAdjectivePattern adjpatt) -> patternOfAdjectivePattern adjpatt
  (PredicateTokenPatternAdjectiveMultiSubjectPattern adjmspatt) -> patternOfAdjectiveMultiSubjectPattern adjmspatt
  (PredicateTokenPatternVerbPattern vpatt) -> patternOfVerbPattern vpatt
  (PredicateTokenPatternVerbMultiSubjectPattern vmspatt) -> patternOfVerbMultiSubjectPattern vmspatt

data AdjectivePattern = AdjectivePattern TVar TokenPattern
  deriving (Show, Eq)

parseAdjectivePattern :: Parser AdjectivePattern
parseAdjectivePattern =
  AdjectivePattern <$> parseTVar <*> (parseLit "is" *> (option $ parseLit "called") *> parseTokenPattern)

patternOfAdjectivePattern :: AdjectivePattern -> Parser Pattern
patternOfAdjectivePattern (AdjectivePattern tv tkpatt) =
  (<>) <$> patternOfTVar tv <*> (patternOfTokenPattern tkpatt)

data AdjectiveMultiSubjectPattern = AdjectiveMultiSubjectPattern VarMultiSubject TokenPattern
  deriving (Show, Eq)

patternOfAdjectiveMultiSubjectPattern :: AdjectiveMultiSubjectPattern -> Parser Pattern
patternOfAdjectiveMultiSubjectPattern (AdjectiveMultiSubjectPattern varms tkpatt) =
  (<>) <$> patternOfVarMultiSubject varms <*> patternOfTokenPattern tkpatt

parseAdjectiveMultiSubjectPattern :: Parser AdjectiveMultiSubjectPattern
parseAdjectiveMultiSubjectPattern = AdjectiveMultiSubjectPattern <$> parseVarMultiSubject <*> parseTokenPattern

data VerbPattern = VerbPattern TVar TokenPattern
  deriving (Show, Eq)

patternOfVerbPattern :: VerbPattern -> Parser Pattern
patternOfVerbPattern (VerbPattern tv tkpatt) =
  (<>) <$> patternOfTVar tv <*> patternOfTokenPattern tkpatt

parseVerbPattern :: Parser VerbPattern
parseVerbPattern = VerbPattern <$> parseTVar <*> parseTokenPattern

data VerbMultiSubjectPattern = VerbMultiSubjectPattern VarMultiSubject TokenPattern
  deriving (Show, Eq)

patternOfVerbMultiSubjectPattern :: VerbMultiSubjectPattern -> Parser Pattern
patternOfVerbMultiSubjectPattern (VerbMultiSubjectPattern vms tkpatt) =
  (<>) <$> patternOfVarMultiSubject vms <*> patternOfTokenPattern tkpatt

parseVerbMultiSubjectPattern :: Parser VerbMultiSubjectPattern
parseVerbMultiSubjectPattern = VerbMultiSubjectPattern <$> parseVarMultiSubject <*> parseTokenPattern

data VarMultiSubject =
    VarMultiSubjectTVar TVar TVar
  | VarMultiSubjectParen Var Var ColonType
  deriving (Show, Eq)

patternOfVarMultiSubject :: VarMultiSubject -> Parser Pattern
patternOfVarMultiSubject x = case x of
  (VarMultiSubjectTVar tv1 tv2) -> (<>) <$> patternOfTVar tv1 <*> patternOfTVar tv2
  (VarMultiSubjectParen v1 v2 ct) -> (<>) <$> patternOfVar v1 <*> patternOfVar v2

parseVarMultiSubject :: Parser VarMultiSubject
parseVarMultiSubject =
  VarMultiSubjectTVar <$> parseTVar <* parseComma <*> parseTVar <||>
  (paren $ (VarMultiSubjectParen <$> parseVar <* parseComma <*> parseVar <*> parseColonType))

data StructureDef = StructureDef IdentifierPattern Structure
  deriving (Show, Eq)

parseStructureDef :: Parser StructureDef
parseStructureDef = StructureDef <$>
  (option parseLitA *> parseIdentifierPattern) <* parseLit "is" <* parseLitA <*>
  parseStructure


data InductiveDef = InductiveDef InductiveType
  deriving (Show, Eq)

parseInductiveDef :: Parser InductiveDef
parseInductiveDef = InductiveDef <$> (parseOptDefine *> parseInductiveType)

data MutualInductiveDef = MutualInductiveDef MutualInductiveType
  deriving (Show, Eq)

parseMutualInductiveDef :: Parser MutualInductiveDef
parseMutualInductiveDef = MutualInductiveDef <$> (parseOptDefine *> parseMutualInductiveType)

data FunctionDef = FunctionDef FunctionHead Copula PlainTerm
  deriving (Show, Eq)

patternOfFunctionDef :: FunctionDef -> Parser Pattern
patternOfFunctionDef fd = case fd of
  FunctionDef functionhead copula plainterm -> case functionhead of
    FunctionHeadFunctionTokenPattern (FunctionTokenPattern tkpatt) -> patternOfTokenPattern tkpatt
    FunctionHeadIdentifierPattern idpatt -> patternOfIdentifierPattern idpatt
    FunctionHeadSymbolPattern sympatt mpl -> patternOfSymbolPattern sympatt

precOfFunctionDef :: FunctionDef -> Parser (Int, AssociativeParity)
precOfFunctionDef fd = case fd of
  FunctionDef functionhead copula plainterm -> case functionhead of
    FunctionHeadFunctionTokenPattern (FunctionTokenPattern tkpatt) -> empty
    FunctionHeadIdentifierPattern idpatt -> empty
    FunctionHeadSymbolPattern sympatt mpl -> case mpl of
      Nothing -> return defaultPrec
      Just (ParenPrecedenceLevelPrecedenceLevel (PrecedenceLevel ni mp)) -> precHandler ni mp
      Just (ParenPrecedenceLevelParen (PrecedenceLevel ni mp)) -> precHandler ni mp
      where
        precHandler ni mp = case mp of
          Nothing -> precHandler ni (Just defaultAssociativeParity)
          (Just ap) -> (,) <$> (readNumInt ni) <*> return ap

registerPrimDefiniteNoun :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimDefiniteNoun lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadFunctionTokenPattern (FunctionTokenPattern tkpatt)) ->
      patternOfTokenPattern tkpatt >>= updatePrimDefiniteNoun lgflag
    _ -> empty

registerPrimDefiniteNounMacro :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimDefiniteNounMacro lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadFunctionTokenPattern (FunctionTokenPattern tkpatt)) ->
      patternOfTokenPattern tkpatt >>= updatePrimDefiniteNoun lgflag . toMacroPatts
    _ -> empty

registerPrimIdentifierTerm :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimIdentifierTerm lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadIdentifierPattern idpatt) -> patternOfIdentifierPattern idpatt >>= updatePrimIdentifierTerm lgflag
    _ -> empty

registerPrimIdentifierTermMacro :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimIdentifierTermMacro lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadIdentifierPattern idpatt) -> patternOfIdentifierPattern idpatt >>= updatePrimIdentifierTerm lgflag . toMacroPatts
    _ -> empty

-- registerPrimPrefixFunction :: LocalGlobalFlag ->  FunctionDef -> Parser ()
-- registerPrimPrefixFunction lgflag fd@(FunctionDef fh c pt) =
--   case fh of -- TODO(jesse): change this to an identifier which only accepts one argument
--     (FunctionHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl) ->
--       case mtv1 of
--         Nothing -> if (isCSBrace slc) || (vs /= []) || (isNothing mtv2) then empty
--           else patternOfSymbolPattern sympatt >>= updatePrimPrefixFunction lgflag
--         _ -> empty
--     _ -> empty

-- registerPrimPrefixFunctionMacro :: LocalGlobalFlag ->  FunctionDef -> Parser ()
-- registerPrimPrefixFunctionMacro lgflag fd@(FunctionDef fh c pt) =
--   case fh of -- TODO(jesse): change this to an identifier which only accepts one argument
--     (FunctionHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl) ->
--       case mtv1 of
--         Nothing -> if (isCSBrace slc) || (vs /= []) || (isNothing mtv2) then empty
--           else patternOfSymbolPattern sympatt >>= updatePrimPrefixFunction lgflag . toMacroPatts
--         _ -> empty
--     _ -> empty
         
--  (* from function_def.binary_controlseq_pattern, prec > 0 )*
registerPrimTermOpControlSeq :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimTermOpControlSeq lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl) ->
      if (isBinaryControlSeqSymbolPattern sympatt)
         then do {b <- isPositivePrecedence mpl;
                  if b
                    then patternOfSymbolPattern sympatt >>= updatePrimTermOpControlSeq lgflag
                    else empty}
       else empty
    _ -> empty

registerPrimTermOpControlSeqMacro :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimTermOpControlSeqMacro lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl) ->
      if (isBinaryControlSeqSymbolPattern sympatt)
         then do {b <- isPositivePrecedence mpl;
                  if b
                    then patternOfSymbolPattern sympatt >>= updatePrimTermOpControlSeq lgflag . toMacroPatts
                    else empty}
       else empty
    _ -> empty

--  (* from function_def.controlseq_pattern, no prec *)
registerPrimTermControlSeq :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimTermControlSeq lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl) ->
      if (isCSBrace slc)
         then case mpl of
                Nothing -> patternOfSymbolPattern sympatt >>= updatePrimTermControlSeq lgflag
                _ -> empty
         else empty
    _ -> empty

registerPrimTermControlSeqMacro :: LocalGlobalFlag ->  FunctionDef -> Parser ()
registerPrimTermControlSeqMacro lgflag fd@(FunctionDef fh c pt) =
  case fh of
    (FunctionHeadSymbolPattern sympatt@(SymbolPattern mtv1 slc vs mtv2) mpl) ->
      if (isCSBrace slc)
         then case mpl of
                Nothing -> patternOfSymbolPattern sympatt >>= updatePrimTermControlSeq lgflag . toMacroPatts
                _ -> empty
         else empty
    _ -> empty

registerFunctionDef :: LocalGlobalFlag -> FunctionDef -> Parser ()
registerFunctionDef lgflag fd = with_any_result (return fd) side_effects *> skip
  where
    side_effects = [
      registerPrimDefiniteNoun lgflag,
      registerPrimIdentifierTerm lgflag,
      -- registerPrimPrefixFunction lgflag,
      registerPrimTermOpControlSeq lgflag,
      registerPrimTermControlSeq lgflag
                   ]

registerFunctionDefMacro :: LocalGlobalFlag -> FunctionDef -> Parser ()
registerFunctionDefMacro lgflag fd = with_any_result (return fd) side_effects *> skip
  where
    side_effects = [
      registerPrimDefiniteNounMacro lgflag,
      registerPrimIdentifierTermMacro lgflag,
      -- registerPrimPrefixFunctionMacro lgflag,
      registerPrimTermOpControlSeqMacro lgflag,
      registerPrimTermControlSeqMacro lgflag
                   ]                   

parseFunctionDef :: Parser FunctionDef
parseFunctionDef = FunctionDef <$> (parseOptDefine *> parseFunctionHead) <*>
                                parseCopula <* option (parseLitEqual) <* option (parseLit "the")
                                <*> parsePlainTerm
 
  --  do
  -- functiondef@(FunctionDef functionhead _ _) <- parse_function_def_main
  -- case functionhead of
  --   FunctionHeadFunctionTokenPattern (FunctionTokenPattern tkpatt) -> (patternOfFunctionDef functiondef >>= updatePrimDefiniteNoun) *> return functiondef Globally
  --   FunctionHeadIdentifierPattern idpatt -> (patternOfFunctionDef functiondef >>= updatePrimIdentifierTerm) *> return functiondef Globally
  --   FunctionHeadSymbolPattern sympatt mpl ->
  --     (do ptt <- patternOfFunctionDef functiondef
  --         updatePrimTermControlSeq ptt Globally
  --         (precOfFunctionDef functiondef >>= (uncurry $ updatePrimPrecTable ptt))) *> Globally
  --         return functiondef
  -- where
  --   parse_function_def_main = FunctionDef <$> (parseOptDefine *> parseFunctionHead) <*>
  --                               parseCopula <* option (parseLitEqual) <* option (parseLit "the")
  --                               <*> parsePlainTerm

-- note: this currently sends token patterns to prim_definite_noun,
-- identifier patterns to prim_identifier_term,
-- and all symbol patterns to prim_term_control_seq.
-- for now, we are following the rule that side-effects on the state are constraineed to top-level declaration parsers (like the branches of parseDefinition)

data FunctionHead =
    FunctionHeadFunctionTokenPattern FunctionTokenPattern
  | FunctionHeadSymbolPattern SymbolPattern (Maybe ParenPrecedenceLevel)
  | FunctionHeadIdentifierPattern IdentifierPattern
  -- | FunctionHeadControlSeqPattern ControlSeqPattern
  -- | FunctionHeadBinaryControlSeqPattern BinaryControlSeqPattern (Maybe ParenPrecedenceLevel)
  deriving (Show, Eq)

parseFunctionHead :: Parser FunctionHead
parseFunctionHead =
  FunctionHeadFunctionTokenPattern <$> parseFunctionTokenPattern <||>
  FunctionHeadSymbolPattern <$> parseSymbolPattern <*> (option parseParenPrecedenceLevel)<||>
  FunctionHeadIdentifierPattern <$> parseIdentifierPattern--  <||>
  -- FunctionHeadControlSeqPattern <$> parseControlSeqPattern <||>
  -- FunctionHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern <*> (option parseParenPrecedenceLevel)

data FunctionTokenPattern = FunctionTokenPattern TokenPattern
  deriving (Show, Eq)

parseFunctionTokenPattern :: Parser FunctionTokenPattern
parseFunctionTokenPattern = FunctionTokenPattern <$> (parseLit "the" *> parseTokenPattern)

data SymbolLowercase = -- corresponds to literal "symbol", not "SYMBOL" in the grammar specification
    SymbolLowercaseSymbol Symbol
  | SymbolLowercaseCSBrace (CSBrace ControlSequence)
  deriving (Show, Eq)

isCSBrace :: SymbolLowercase -> Bool
isCSBrace slc = case slc of
  SymbolLowercaseSymbol symb -> False
  _ -> True

isSymbol :: SymbolLowercase -> Bool
isSymbol = not . isCSBrace

patternOfSymbolLowercase :: SymbolLowercase -> Parser Pattern
patternOfSymbolLowercase x = case x of
  SymbolLowercaseSymbol symb -> patternOfSymbol symb
  SymbolLowercaseCSBrace (CSBrace cs tvars) -> (patternOfList patternOfTVar tvars) >>= patternOfControlSequence cs  

parseSymbolLowercase :: Parser SymbolLowercase
parseSymbolLowercase =
  SymbolLowercaseSymbol <$> parseSymbol <||>
  SymbolLowercaseCSBrace <$> (parseCSBrace parseControlSequence)

data SymbolPattern = SymbolPattern (Maybe TVar) SymbolLowercase [(TVar, SymbolLowercase)] (Maybe TVar)
  deriving (Show, Eq)

isBinarySymbolPattern :: SymbolPattern -> Bool
isBinarySymbolPattern (SymbolPattern mtv1 slc vs mtv2) =
  (not $ isNothing mtv1) && (not $ isCSBrace slc) && (vs == []) && (not $ isNothing mtv2)

--- currently, i'm interpreting "binary control seq pattern" to mean binary in the sense of binary infix
isBinaryControlSeqSymbolPattern :: SymbolPattern -> Bool
isBinaryControlSeqSymbolPattern (SymbolPattern mtv1 slc vs mtv2) =
  (not $ isNothing mtv1) && (isCSBrace slc) && (vs == []) && (not $ isNothing mtv2)

patternOfSymbolPattern :: SymbolPattern -> Parser Pattern
patternOfSymbolPattern (SymbolPattern mtvar symbs tvarsymbs mtvar') =
  (patternOfOption patternOfTVar mtvar) <+>
  (patternOfSymbolLowercase symbs) <+>
  (patternOfList (\(a,b) -> patternOfTVar a <+> patternOfSymbolLowercase b) tvarsymbs)
  <+> (patternOfOption patternOfTVar mtvar')

parseSymbolPattern :: Parser SymbolPattern
parseSymbolPattern = SymbolPattern <$> (option parseTVar) <*> parseSymbolLowercase <*>
                                       (many' $ (,) <$> parseTVar <*> parseSymbolLowercase) <*>
                                       (option parseTVar)   

data TypeDef = TypeDef TypeHead Copula GeneralType
  deriving (Show, Eq)

patternOfTypeDef :: TypeDef -> Parser Pattern
patternOfTypeDef (TypeDef th cpla gtp) = patternOfTypeHead th

registerPrimIdentifierType :: LocalGlobalFlag ->  TypeDef -> Parser () --  (* from type_def *) all identifiers that are types
registerPrimIdentifierType lgflag td@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadIdentifierPattern idpatt) -> patternOfTypeDef td >>= updatePrimIdentifierType lgflag
    _ -> empty

registerPrimIdentifierTypeMacro :: LocalGlobalFlag ->  TypeDef -> Parser () --  (* from type_def *) all identifiers that are types
registerPrimIdentifierTypeMacro lgflag td@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadIdentifierPattern idpatt) -> patternOfTypeDef td >>= updatePrimIdentifierType lgflag . toMacroPatts
    _ -> empty

registerPrimTypeOp :: LocalGlobalFlag ->  TypeDef -> Parser () --  (* from type_def, when infix with precedence (from tokenpattern)*)
registerPrimTypeOp lgflag td@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadTypeTokenPattern (TypeTokenPattern tkpatt)) ->
      if isBinaryTokenPattern tkpatt
        then patternOfTypeDef td >>= updatePrimTypeOp lgflag
        else empty
    _ -> empty

registerPrimTypeOpMacro :: LocalGlobalFlag ->  TypeDef -> Parser () --  (* from type_def, when infix with precedence (from tokenpattern)*)
registerPrimTypeOpMacro lgflag td@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadTypeTokenPattern (TypeTokenPattern tkpatt)) ->
      if isBinaryTokenPattern tkpatt
        then patternOfTypeDef td >>= updatePrimTypeOp lgflag . toMacroPatts
        else empty
    _ -> empty

registerPrimTypeOpControlSeq :: LocalGlobalFlag ->  TypeDef -> Parser ()
registerPrimTypeOpControlSeq lgflag pd@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadControlSeqPattern cseqpatt) -> patternOfControlSeqPattern cseqpatt >>= updatePrimTypeOpControlSeq lgflag
    _ -> empty

registerPrimTypeOpControlSeqMacro :: LocalGlobalFlag ->  TypeDef -> Parser ()
registerPrimTypeOpControlSeqMacro lgflag pd@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadControlSeqPattern cseqpatt) -> patternOfControlSeqPattern cseqpatt >>= updatePrimTypeOpControlSeq lgflag . toMacroPatts
    _ -> empty

registerPrimTypeControlSeq :: LocalGlobalFlag ->  TypeDef -> Parser ()
registerPrimTypeControlSeq lgflag pd@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadBinaryControlSeqPattern bcseqpatt) -> patternOfBinaryControlSeqPattern bcseqpatt >>= updatePrimTypeControlSeq lgflag
    _ -> empty

registerPrimTypeControlSeqMacro :: LocalGlobalFlag ->  TypeDef -> Parser ()
registerPrimTypeControlSeqMacro lgflag pd@(TypeDef th cpla gtp) =
  case th of
    (TypeHeadBinaryControlSeqPattern bcseqpatt) -> patternOfBinaryControlSeqPattern bcseqpatt >>= updatePrimTypeControlSeq lgflag . toMacroPatts
    _ -> empty

registerTypeDef :: LocalGlobalFlag ->  TypeDef -> Parser ()
registerTypeDef lgflag td = with_any_result (return td) side_effects *> skip
  where
    side_effects :: [TypeDef -> Parser ()]
    side_effects = [
      registerPrimIdentifierType lgflag,
      registerPrimTypeOp lgflag,
      registerPrimTypeOpControlSeq lgflag,
      registerPrimTypeControlSeq lgflag
                   ]

registerTypeDefMacro :: LocalGlobalFlag ->  TypeDef -> Parser ()
registerTypeDefMacro lgflag td = with_any_result (return td) side_effects *> skip
  where
    side_effects :: [TypeDef -> Parser ()]
    side_effects = [
      registerPrimIdentifierTypeMacro lgflag,
      registerPrimTypeOpMacro lgflag,
      registerPrimTypeOpControlSeqMacro lgflag,
      registerPrimTypeControlSeqMacro lgflag
                   ]

parseTypeDef :: Parser TypeDef
parseTypeDef = TypeDef <$> parseTypeHead <*> parseCopula <* parseLitA <*> parseGeneralType

data TypeHead =
    TypeHeadTypeTokenPattern TypeTokenPattern
  | TypeHeadIdentifierPattern IdentifierPattern
  | TypeHeadControlSeqPattern ControlSeqPattern
  | TypeHeadBinaryControlSeqPattern BinaryControlSeqPattern
  deriving (Show, Eq)

patternOfTypeHead :: TypeHead -> Parser Pattern
patternOfTypeHead th = case th of
  (TypeHeadTypeTokenPattern (TypeTokenPattern tkpatt)) -> patternOfTokenPattern tkpatt
  (TypeHeadIdentifierPattern idpatt) -> patternOfIdentifierPattern idpatt
  (TypeHeadControlSeqPattern cspatt) -> patternOfControlSeqPattern cspatt
  (TypeHeadBinaryControlSeqPattern bcspatt) -> patternOfBinaryControlSeqPattern bcspatt

parseTypeHead :: Parser TypeHead
parseTypeHead =
  TypeHeadTypeTokenPattern <$> parseTypeTokenPattern <||>
  TypeHeadIdentifierPattern <$> parseIdentifierPattern <||>
  TypeHeadControlSeqPattern <$> parseControlSeqPattern <||>
  TypeHeadBinaryControlSeqPattern <$> parseBinaryControlSeqPattern

data TypeTokenPattern = TypeTokenPattern TokenPattern
  deriving (Show, Eq)

parseTypeTokenPattern :: Parser TypeTokenPattern
parseTypeTokenPattern = TypeTokenPattern <$> (parseLitA *> parseTokenPattern)

 -- (* restriction: tokens in pattern cannot be a variant of
 --    "to be", "called", "iff" "a" "stand" "denote"
 --    cannot start with "the"  *)

parsePatternToken :: Parser Token
parsePatternToken = fail_iff_succeeds (lookAhead' parseCopula) *> -- note(jesse): added to ensure copula literals are not consumed by token pattern parsing
  (guard_result "forbidden token parsed, failing" parseToken $
                      \x -> not $ elem (tokenToText x) ["the", "to be", "called", "iff", "a", "stand", "denote"])

newtype Tokens = Tokens [Token]
  deriving (Show, Eq)

parseTokens :: Parser Tokens
parseTokens = Tokens <$> many1' parsePatternToken

tokensToTokens :: Tokens -> [Token]
tokensToTokens tks0@(Tokens tks) = tks

data TokenPattern = TokenPattern Tokens [(TVar, Tokens)] (Maybe TVar)
  deriving (Show, Eq)

isBinaryTokenPattern :: TokenPattern -> Bool
isBinaryTokenPattern tkpatt@(TokenPattern tks tvtks mtv) =
  tks == (Tokens []) && (length tvtks == 1) && isSomething mtv

parseTokenPattern :: Parser TokenPattern
parseTokenPattern = TokenPattern <$> parseTokens <*>
                                     (many' $ (,) <$> parseTVar <*> parseTokens) <*>
                                     (option parseTVar)

-- test parseTokenPattern "foo bar baz a foo b bar c baz"                                     

patternOfTokenPattern :: TokenPattern -> Parser Pattern
patternOfTokenPattern tkPatt@(TokenPattern (Tokens tks) tvstkss mtvar) = Patts <$>
  ((<>) <$> ( do strsyms <- concat <$> use (allStates strSyms)
                 return $ (map (Wd . tokenToText'_aux strsyms) tks) <>
                     concat (map (\(tv,tks) -> Vr : map (Wd . pure . tokenToText) (tokensToTokens tks)) tvstkss) )
            <*> ((unoption $ return mtvar) *> return [Vr] <||> return []))

data Copula =
    CopulaIsDefinedAs
  | CopulaAssign
  | CopulaDenote
  deriving (Show, Eq)

parseCopula :: Parser Copula
parseCopula =
  parseLitIs *> (option parseLitDefinedAs) *> return CopulaIsDefinedAs <||>
  parseAssign *> return CopulaAssign <||>
  parseLitDenote *> return CopulaDenote

data IdentifierPattern =
    IdentifierPattern Identifier Args (Maybe ColonType)
  | IdentifierPatternBlank Args (Maybe ColonType)
  deriving (Show, Eq)

parseIdentifierPattern :: Parser IdentifierPattern
parseIdentifierPattern =
  IdentifierPattern <$> parseIdentifier <*> parseArgs <*> (option parseColonType) <||>
  IdentifierPatternBlank <$> parseArgs <*> (option parseColonType)

patternOfIdentifierPattern :: IdentifierPattern -> Parser Pattern
patternOfIdentifierPattern idpatt =
  case idpatt of
    IdentifierPattern ident args mct -> (patternOfIdent ident) <+> (patternOfArgs args)
    IdentifierPatternBlank args mct -> (patternOfArgs args)
  -- (<>) <$> (return $ (map (Wd . pure . tokenToText) tks) <>
  --                    concat (map (\(tv,tks) -> Vr : map (Wd . pure . tokenToText) (tokensToTokens tks)) tvstkss) )
  --           <*> ((unoption $ return mtvar) *> return [Vr] <||> return [])
   
data ControlSeqPattern = ControlSeqPattern ControlSequence [TVar]
  deriving (Show, Eq)

patternOfControlSeqPattern :: ControlSeqPattern -> Parser Pattern
patternOfControlSeqPattern (ControlSeqPattern cs tvs) =
  (patternOfList patternOfTVar tvs) >>= patternOfControlSequence cs

parseControlSeqPattern :: Parser ControlSeqPattern
parseControlSeqPattern = ControlSeqPattern <$> parseControlSequence <*> (many' $ brace $ parseTVar)

data BinaryControlSeqPattern = BinaryControlSeqPattern TVar ControlSeqPattern TVar
  deriving (Show, Eq)

patternOfBinaryControlSeqPattern :: BinaryControlSeqPattern -> Parser Pattern
patternOfBinaryControlSeqPattern (BinaryControlSeqPattern tv1 cspatt tv2) =
  patternOfTVar tv1 <+> patternOfControlSeqPattern cspatt <+> patternOfTVar tv2

parseBinaryControlSeqPattern :: Parser BinaryControlSeqPattern
parseBinaryControlSeqPattern = BinaryControlSeqPattern <$> parseTVar <*> parseControlSeqPattern <*> parseTVar

data ParenPrecedenceLevel =
    ParenPrecedenceLevelPrecedenceLevel PrecedenceLevel
  | ParenPrecedenceLevelParen PrecedenceLevel 
  deriving (Show, Eq)

isPositivePrecedence :: (Maybe ParenPrecedenceLevel) -> Parser Bool
isPositivePrecedence mpl = case mpl of
  (Just (ParenPrecedenceLevelPrecedenceLevel (PrecedenceLevel numint map))) ->
    isPositivePrecedence_aux numint map
  (Just (ParenPrecedenceLevelParen (PrecedenceLevel numint map))) ->
    isPositivePrecedence_aux numint map
  _ -> return False
  where
    isPositivePrecedence_aux numint map =
          do {k <- readNumInt numint; if k > 0 then return True else return False}

isNegativePrecedence :: (Maybe ParenPrecedenceLevel) -> Parser Bool
isNegativePrecedence mpl = case mpl of
  (Just (ParenPrecedenceLevelPrecedenceLevel (PrecedenceLevel numint map))) ->
    isNegativePrecedence_aux numint map
  (Just (ParenPrecedenceLevelParen (PrecedenceLevel numint map))) ->
    isNegativePrecedence_aux numint map
  _ -> return False
  where
    isNegativePrecedence_aux numint map =
          do {k <- readNumInt numint; if k < 0 then return True else return False}

isZeroPrecedence :: (Maybe ParenPrecedenceLevel) -> Parser Bool
isZeroPrecedence mpl = case mpl of
  (Just (ParenPrecedenceLevelPrecedenceLevel (PrecedenceLevel numint map))) ->
    isZeroPrecedence_aux numint map
  (Just (ParenPrecedenceLevelParen (PrecedenceLevel numint map))) ->
    isZeroPrecedence_aux numint map
  _ -> return False
  where
    isZeroPrecedence_aux numint map =
          do {k <- readNumInt numint; if k == 0 then return True else return False}

parseParenPrecedenceLevel :: Parser ParenPrecedenceLevel
parseParenPrecedenceLevel =
  ParenPrecedenceLevelPrecedenceLevel <$> parsePrecedenceLevel <||>
  ParenPrecedenceLevelParen <$> (paren $ parsePrecedenceLevel)

parseAssociativeParity :: Parser AssociativeParity
parseAssociativeParity =
  parseLit "left" *> return AssociatesLeft <||>
  parseLit "right" *> return AssociatesRight <||>
  parseLit "no" *> return AssociatesNone

data PrecedenceLevel = PrecedenceLevel NumInt (Maybe AssociativeParity)
  deriving (Show, Eq)

parsePrecedenceLevel :: Parser PrecedenceLevel
parsePrecedenceLevel = PrecedenceLevel <$> (parseLit "with" *> parseLit "precedence" *> parseNumInt) <*> (option $ parseLit "and" *> parseAssociativeParity <* parseLit "associativity")

newtype ClassifierDef = ClassifierDef ClassTokens
  deriving (Show, Eq)

newtype ClassTokens = ClassTokens [[Token]]
  deriving (Show, Eq)

parseClassTokens = ClassTokens <$> sepby1 (many1' ((notFollowedBy (parseLitIs <* option parseLitA <* parseLitClassifier)) *> parseToken)) parseComma

parseClassifierDef =
  ClassifierDef <$> (parseLit "let" *>  parseClassTokens <* parseLitIs <* option parseLitA <* parseLitClassifier)

registerClassifierDef :: LocalGlobalFlag -> ClassifierDef -> Parser ()
registerClassifierDef lgflag clsdef@(ClassifierDef (ClassTokens tkss)) =
  updateClsList2 lgflag (map (liftM tokenToText) tkss)

-- test parseClassifierDef "let scheme, schemes, stacks be classifiers"
-- test (parseClassifierDef *> (use $ top.clsList)) "let scheme, schemes, stacks, derived stacks be classifiers"
-- test parseClassifierDef "let lattice be a classifier"
