{-
Author(s): Jesse Michael Han (2019)

Tokenization of input.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Basic.Token where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic.Basic

-- literal tokens

---- `parseLit arg` case-insensitively parses `arg`, and then consumes whitespace to the right.
parseLit :: Text -> Parser Text
parseLit arg = (str' arg) <* sc

-- auxiliary lit parsers, of the form `lit_*` in the grammar specification

parseLitLets :: Parser [Text]
parseLitLets = (do a <- parseLit "let"
                   x <- option (parseLit "us")
                   return $ case x of
                         (Just b) -> [a,b]
                         Nothing -> [a])
               <||>
               (do a <- parseLit "we"
                   x <- option (parseLit "can")
                   return $ case x of
                         (Just b) -> [a,b]
                         Nothing -> [a])

parseLitAssume :: Parser Text
parseLitAssume = parseLit "assume" <||> parseLit "suppose"

parseLitA = parseLit "an" <||> parseLit "a"

parseArticle = parseLit "the" <||> parseLit "a"

parseSepAndComma = parseLit "and" <||> parseLit ","

parseLitBinderComma = parseLit ","

parseLitDefinedAs :: Parser [Text]
parseLitDefinedAs =
       (rp $ parseLit "said") <+> (rp $ parseLit "to") <+> (rp $ parseLit "be")
  <||> (rp $ parseLit "defined") <+> (rp $ parseLit "to") <+> (rp $ parseLit "be")
  <||> (rp $ parseLit "defined") <+> (rp $ parseLit "as")
  
parseLitIff =
      (rp $ parseLit "iff") <||> (rp $ parseLit "if")
                             <+> (rp $ parseLit "and")
                             <+> (rp $ parseLit "only")
                             <+> (rp $ parseLit "if")

parseLitDenote =
  (rp $ parseLit "denote") <||> (rp $ parseLit "stand") <+> (rp $ parseLit "for")

parseLitDo =
  (rp $ parseLit "does") <||> (rp $ parseLit "does")

parseLitIs =
       (rp $ parseLit "is")
  <||> (rp $ parseLit "are")
  <||> (rp $ parseLit "to") <+> (rp $ parseLit "be")
  <||> (rp $ parseLit "be")

parseLitEqual = (rp $ parseLit "equal") <+> (rp $ parseLit "to")

parseLitHas = (rp $ parseLit "has") <||> (rp $ parseLit "have")

parseLitWith =
       (rp $ parseLit "")
  <||> (rp $ parseLit "of")
  <||> (rp $ parseLit "having")

parseLitTrue =
       (rp $ parseLit "with")
  <||> (rp $ parseLit "true")
  <||> (rp $ parseLit "yes")

parseLitFalse =
       (rp $ parseLit "off")
  <||> (rp $ parseLit "false")
  <||> (rp $ parseLit "no")

parseLitItsWrong = (rp $ parseLit "it") <+> (rp $ parseLit "is") <+> (rp $ parseLit "wrong") <+> (rp $ parseLit "that")

parseLitWeRecord =
       (rp $ parseLit "we") <+> (rp $ parseLit "record") <+> (rp $ parseLit "that")
  <||> (rp $ parseLit "we") <+> (rp $ parseLit "record")
  <||> (rp $ parseLit "we") <+> (rp $ parseLit "register") <+> (rp $ parseLit "that")
  <||> (rp $ parseLit "we") <+> (rp $ parseLit "register")
  
parseLitAny =
       (rp $ parseLit "every")
  <||> (rp $ parseLit "each")
  <||> (rp $ parseLit "each") <+> (rp $ parseLit "and") <+> (rp $ parseLit "every")
  <||> (rp $ parseLit "all")
  <||> (rp $ parseLit "any")
  <||> (rp $ parseLit "some")
  <||> (rp $ parseLit "no")

parseLitExist = (rp $ parseLit "exists") <||> (rp $ parseLit "exists")

parseLitThen =
       (rp $ parseLit "then")
  <||> (rp $ parseLit "therefore")
  <||> (rp $ parseLit "hence")

parseLitChoose = (rp $ parseLit "take") <||> (rp $ parseLit "choose")

parseLitProve = (rp $ parseLit "prove") <||> (rp $ parseLit "show")

parseLitWeSay = (rp $ parseLit "we") <+> (rp $ parseLit "say") <+> (rp $ parseLit "that")
           <||> (rp $ parseLit "we") <+> (rp $ parseLit "say")

parseLitLeft =
       (rp $ parseLit "left")
  <||> (rp $ parseLit "right")
  <||> (rp $ parseLit "no")

parseLitFieldKey =
  (rp $ parseLit "embedded")   <||>
  (rp $ parseLit "parametric") <||>
  (rp $ parseLit "typeable")   <||>
  (rp $ parseLit "applicable")

parseLitQED = (rp $ parseLit "end") <||> (rp $ parseLit "QED") <||> (rp $ parseLit "obvious") <||> (rp $ parseLit "trivial")

parseLitDocument = (rp $ parseLit "document")
              <||> (rp $ parseLit "article")
              <||> (rp $ parseLit "section")
              <||> (rp $ parseLit "subsection")
              <||> (rp $ parseLit "subsubsection")

parseLitDef = (rp $ parseLit "definition") <||> (rp $ parseLit "def")

parseLitAxiom =
  (rp $ parseLit "axiom")      <||>
  (rp $ parseLit "conjecture") <||>
  (rp $ parseLit "hypothesis")

parseLitTheorem =
  (rp $ parseLit "proposition")  <||>
  (rp $ parseLit "theorem")      <||>
  (rp $ parseLit "lemma")        <||>
  (rp $ parseLit "corollary")

parseLitLocation =
       parseLitDocument
  <||> parseLitTheorem
  <||> parseLitAxiom
  <||> parseLitDef

parseLitSort = (rp $ parseLit "type") <||> (rp $ parseLit "prop")

parseLitClassifier = (rp $ parseLit "classifiers") <||> (rp $ parseLit "classifier")

parseLitVarmod =
  (rp $ parseLit "fixed")    <||>
  (rp $ parseLit "implicit") <||>
  (rp $ parseLit "resolved") <||>
  (rp $ parseLit "remove")
      
-- data Lit =
--     A
--   | ALL
--   | AN
--   | ANALYSIS
--   | AND
--   | ANY
--   | APPLICABLE
--   | ARE
--   | ARTICLE
--   | AS
--   | AT
--   | ASSOCIATIVITY
--   | ASSUME
--   | ASSUMING
--   | AXIOM
--   | BE
--   | BY
--   | CALLED
--   | CAN
--   | CANONICAL
--   | CASE
--   | CHOOSE
--   | CLASSIFIER
--   | CLASSIFIERS
--   | CONJECTURE
--   | CONTRADICTION
--   | CONTRARY
--   | COROLLARY
--   | DEF
--   | DEFINE
--   | DEFINED
--   | DEFINITION
--   | DENOTE
--   | DO
--   | DOCUMENT
--   | DOES
--   | DUMP
--   | EACH
--   | ELSE
--   | EMBEDDED
--   | END
--   | EQUAL
--   | EVERY
--   | EXHAUSTIVE
--   | EXIST
--   | EXISTS
--   | EXIT
--   | FALSE
--   | FIXED
--   | FOR
--   | FORALL
--   | FUN
--   | FUNCTION
--   | HAS
--   | HAVE
--   | HAVING
--   | HENCE
--   | HOLDING
--   | HYPOTHESIS
--   | IF
--   | IFF
--   | IMPLICIT
--   | IN
--   | INDEED
--   | INDUCTION
--   | INDUCTIVE
--   | IS
--   | IT
--   | LEFT
--   | LEMMA
--   | LET
--   | LIBRARY
--   | MATCH
--   | MID
--   | NO
--   | NOT
--   | NOTATION
--   | NOTATIONAL
--   | OBVIOUS
--   | OF
--   | OFF
--   | ON
--   | ONLY
--   | ONTORED
--   | OR
--   | PAIRWISE
--   | PARAMETERS
--   | PARAMETRIC
--   | PRECEDENCE
--   | PRINTGOAL
--   | PROOF
--   | PROP
--   | PROVE
--   | PROPOSITION
--   | PROPPED
--   | QED
--   | QUOTIENT
--   | READ
--   | RECORD
--   | REGISTER
--   | RECURSION
--   | REMOVE
--   | RESOLVED
--   | RIGHT
--   | SAID
--   | SATISFYING
--   | SAY
--   | SECTION
--   | SHOW
--   | SOME
--   | STAND
--   | STRUCTURE
--   | SUBSECTION
--   | SUBSUBSECTION
--   | SUBTYPEMID
--   | SUCH
--   | SUPPOSE
--   | SYNONYM
--   | TAKE
--   | THAT
--   | THE
--   | THEN
--   | THEOREM
--   | THERE
--   | THEREFORE
--   | THESIS
--   | THIS
--   | TIMELIMIT
--   | TO
--   | TOTAL
--   | TRIVIAL
--   | TRUE
--   | TYPE
--   | TYPEABLE
--   | UNIQUE
--   | US
--   | WE
--   | WELL
--   | WELLDEFINED
--   | WELL_DEFINED
--   | WELL_PROPPED
--   | WITH
--   | WITHOUT
--   | WRONG
--   | YES
--   deriving (Show, Eq)

parseDataHelper :: a -> Text -> (Parser a)
parseDataHelper l arg = ((do
  x <- not_whitespace
  (if str_eq' x arg then return l else fail "parseDataHelper failed to match")) <||> fail "parseDataHelper failed to match'") <* sc

parseDataHelper0 :: a -> Text -> (Parser a)
parseDataHelper0 l arg = (str' arg >> return l) <* sc

-- parseLit_aux :: Lit -> (Parser Lit)
-- parseLit_aux l = case l of
--   A -> parseDataHelper0 A "a"
--   ALL -> parseDataHelper0 ALL "all"
--   AN -> parseDataHelper0 AN "an"
--   AT -> parseDataHelper0 AT "at"
--   ANALYSIS -> parseDataHelper0 ANALYSIS "analysis"
--   AND -> parseDataHelper0 AND "and"
--   ANY -> parseDataHelper0 ANY "any"
--   APPLICABLE -> parseDataHelper0 APPLICABLE "applicable"
--   ARE -> parseDataHelper0 ARE "are"
--   ARTICLE -> parseDataHelper0 ARTICLE "article"
--   AS -> parseDataHelper0 AS "as"
--   ASSOCIATIVITY -> parseDataHelper0 ASSOCIATIVITY "associativity"
--   ASSUME -> parseDataHelper0 ASSUME "assume"
--   ASSUMING -> parseDataHelper0 ASSUMING "assuming"
--   AXIOM -> parseDataHelper0 AXIOM "axiom"
--   BE -> parseDataHelper0 BE "be"
--   BY -> parseDataHelper0 BY "by"
--   CALLED -> parseDataHelper0 CALLED "called"
--   CAN -> parseDataHelper0 CAN "can"
--   CANONICAL -> parseDataHelper0 CANONICAL "canonical"
--   CASE -> parseDataHelper0 CASE "case"
--   CHOOSE -> parseDataHelper0 CHOOSE "choose"
--   CLASSIFIER -> parseDataHelper0 CLASSIFIER "classifier"
--   CLASSIFIERS -> parseDataHelper0 CLASSIFIERS "classifiers"
--   CONJECTURE -> parseDataHelper0 CONJECTURE "conjecture"
--   CONTRADICTION -> parseDataHelper0 CONTRADICTION "contradiction"
--   CONTRARY -> parseDataHelper0 CONTRARY "contrary"
--   COROLLARY -> parseDataHelper0 COROLLARY "corollary"
--   DEF -> parseDataHelper0 DEF "def"
--   DEFINE -> parseDataHelper0 DEFINE "define"
--   DEFINED -> parseDataHelper0 DEFINED "defined"
--   DEFINITION -> parseDataHelper0 DEFINITION "DEFINITION"
--   DENOTE -> parseDataHelper0 DENOTE "denote"
--   DO -> parseDataHelper0 DO "do"
--   DOCUMENT -> parseDataHelper0 DOCUMENT "document"
--   DOES -> parseDataHelper0 DOES "does"
--   DUMP -> parseDataHelper0 DUMP "dump"
--   EACH -> parseDataHelper0 EACH "each"
--   ELSE -> parseDataHelper0 ELSE "else"
--   EMBEDDED -> parseDataHelper0 EMBEDDED "embedded"
--   END -> parseDataHelper0 END "end"
--   EQUAL -> parseDataHelper0 EQUAL "equal"
--   EVERY -> parseDataHelper0 EVERY "every"
--   EXHAUSTIVE -> parseDataHelper0 EXHAUSTIVE "exhaustive"
--   EXIST -> parseDataHelper0 EXIST "exist"
--   EXISTS -> parseDataHelper0 EXISTS "exists"
--   EXIT -> parseDataHelper0 EXIT "exit"
--   FALSE -> parseDataHelper0 FALSE "false"
--   FIXED -> parseDataHelper0 FIXED "fixed"
--   FOR -> parseDataHelper0 FOR "for"
--   FORALL -> parseDataHelper0 FORALL "forall"
--   FUN -> parseDataHelper0 FUN "fun"
--   FUNCTION -> parseDataHelper0 FUNCTION "function"
--   HAS -> parseDataHelper0 HAS "has"
--   HAVE -> parseDataHelper0 HAVE "have"
--   HAVING -> parseDataHelper0 HAVING "having"
--   HENCE -> parseDataHelper0 HENCE "hence"
--   HOLDING -> parseDataHelper0 HOLDING "holding"
--   HYPOTHESIS -> parseDataHelper0 HYPOTHESIS "hypothesis"
--   IF -> parseDataHelper0 IF "if"
--   IFF -> parseDataHelper0 IFF "iff"
--   IMPLICIT -> parseDataHelper0 IMPLICIT "implicit"
--   IN -> parseDataHelper0 IN "in"
--   INDEED -> parseDataHelper0 INDEED "indeed"
--   INDUCTION -> parseDataHelper0 INDUCTION "induction"
--   INDUCTIVE -> parseDataHelper0 INDUCTIVE "inductive"
--   IS -> parseDataHelper0 IS "is"
--   IT -> parseDataHelper0 IT "it"
--   LEFT -> parseDataHelper0 LEFT "left"
--   LEMMA -> parseDataHelper0 LEMMA "lemma"
--   LET -> parseDataHelper0 LET "let"
--   LIBRARY -> parseDataHelper0 LIBRARY "library"
--   MATCH -> parseDataHelper0 MATCH "match"
--   MID -> parseDataHelper0 MID "mid"
--   NO -> parseDataHelper0 NO "no"
--   NOT -> parseDataHelper0 NOT "not"
--   NOTATION -> parseDataHelper0 NOTATION "notation"
--   NOTATIONAL -> parseDataHelper0 NOTATIONAL "notational"
--   OBVIOUS -> parseDataHelper0 OBVIOUS "obvious"
--   OF -> parseDataHelper0 OF "of"
--   OFF -> parseDataHelper0 OFF "off"
--   ON -> parseDataHelper0 ON "on"
--   ONLY -> parseDataHelper0 ONLY "only"
--   ONTORED -> parseDataHelper0 ONTORED "ontored"
--   OR -> parseDataHelper0 OR "or"
--   PAIRWISE -> parseDataHelper0 PAIRWISE "pairwise"
--   PARAMETERS -> parseDataHelper0 PARAMETERS "parameters"
--   PARAMETRIC -> parseDataHelper0 PARAMETRIC "parametric"
--   PRECEDENCE -> parseDataHelper0 PRECEDENCE "precedence"
--   PRINTGOAL -> parseDataHelper0 PRINTGOAL "printgoal"
--   PROOF -> parseDataHelper0 PROOF "proof"
--   PROP -> parseDataHelper0 PROP "prop"
--   PROVE -> parseDataHelper0 PROVE "prove"
--   PROPOSITION -> parseDataHelper0 PROPOSITION "proposition"
--   PROPPED -> parseDataHelper0 PROPPED "propped"
--   QED -> parseDataHelper0 QED "qed"
--   QUOTIENT -> parseDataHelper0 QUOTIENT "quotient"
--   READ -> parseDataHelper0 READ "read"
--   RECORD -> parseDataHelper0 RECORD "record"
--   REGISTER -> parseDataHelper0 REGISTER "register"
--   RECURSION -> parseDataHelper0 RECURSION "recursion"
--   REMOVE -> parseDataHelper0 REMOVE "remove"
--   RESOLVED -> parseDataHelper0 RESOLVED "resolved"
--   RIGHT -> parseDataHelper0 RIGHT "right"
--   SAID -> parseDataHelper0 SAID "said"
--   SATISFYING -> parseDataHelper0 SATISFYING "satisfying"
--   SAY -> parseDataHelper0 SAY "say"
--   SECTION -> parseDataHelper0 SECTION "section"
--   SHOW -> parseDataHelper0 SHOW "show"
--   SOME -> parseDataHelper0 SOME "some"
--   STAND -> parseDataHelper0 STAND "stand"
--   STRUCTURE -> parseDataHelper0 STRUCTURE "structure"
--   SUBSECTION -> parseDataHelper0 SUBSECTION "subsection"
--   SUBSUBSECTION -> parseDataHelper0 SUBSUBSECTION "subsubsection"
--   SUBTYPEMID -> parseDataHelper0 SUBTYPEMID "subtypemid"
--   SUCH -> parseDataHelper0 SUCH "such"
--   SUPPOSE -> parseDataHelper0 SUPPOSE "suppose"
--   SYNONYM -> parseDataHelper0 SYNONYM "synonym"
--   TAKE -> parseDataHelper0 TAKE "take"
--   THAT -> parseDataHelper0 THAT "that"
--   THE -> parseDataHelper0 THE "the"
--   THEN -> parseDataHelper0 THEN "then"
--   THEOREM -> parseDataHelper0 THEOREM "theorem"
--   THERE -> parseDataHelper0 THERE "there"
--   THEREFORE -> parseDataHelper0 THEREFORE "therefore"
--   THESIS -> parseDataHelper0 THESIS "thesis"
--   THIS -> parseDataHelper0 THIS "this"
--   TIMELIMIT -> parseDataHelper0 TIMELIMIT "timelimit"
--   TO -> parseDataHelper0 TO "to"
--   TOTAL -> parseDataHelper0 TOTAL "total"
--   TRIVIAL -> parseDataHelper0 TRIVIAL "trivial"
--   TRUE -> parseDataHelper0 TRUE "true"
--   TYPE -> parseDataHelper0 TYPE "type"
--   TYPEABLE -> parseDataHelper0 TYPEABLE "typeable"
--   UNIQUE -> parseDataHelper0 UNIQUE "unique"
--   US -> parseDataHelper0 US "us"
--   WE -> parseDataHelper0 WE "we"
--   WELL -> parseDataHelper0 WELL "well"
--   WELLDEFINED -> parseDataHelper0 WELLDEFINED "welldefined"
--   WELL_DEFINED -> parseDataHelper0 WELL_DEFINED "well_defined"
--   WELL_PROPPED -> parseDataHelper0 WELL_PROPPED "well_propped"
--   WITH -> parseDataHelper0 WITH "with"
--   WITHOUT -> parseDataHelper0 WITHOUT "without"
--   WRONG -> parseDataHelper0 WRONG "wrong"
--   YES -> parseDataHelper0 YES "yes"

-- memsOfLit :: [Lit]
-- memsOfLit = [A
--   , ALL
--   , AN
--   , AT
--   , ANALYSIS
--   , AND
--   , ANY
--   , APPLICABLE
--   , ARE
--   , ARTICLE
--   , AS
--   , ASSOCIATIVITY
--   , ASSUME
--   , ASSUMING
--   , AXIOM
--   , BE
--   , BY
--   , CALLED
--   , CAN
--   , CANONICAL
--   , CASE
--   , CHOOSE
--   , CLASSIFIER
--   , CLASSIFIERS
--   , CONJECTURE
--   , CONTRADICTION
--   , CONTRARY
--   , COROLLARY
--   , DEF
--   , DEFINE
--   , DEFINED
--   , DEFINITION
--   , DENOTE
--   , DO
--   , DOCUMENT
--   , DOES
--   , DUMP
--   , EACH
--   , ELSE
--   , EMBEDDED
--   , END
--   , EQUAL
--   , EVERY
--   , EXHAUSTIVE
--   , EXIST
--   , EXISTS
--   , EXIT
--   , FALSE
--   , FIXED
--   , FOR
--   , FORALL
--   , FUN
--   , FUNCTION
--   , HAS
--   , HAVE
--   , HAVING
--   , HENCE
--   , HOLDING
--   , HYPOTHESIS
--   , IF
--   , IFF
--   , IMPLICIT
--   , IN
--   , INDEED
--   , INDUCTION
--   , INDUCTIVE
--   , IS
--   , IT
--   , LEFT
--   , LEMMA
--   , LET
--   , LIBRARY
--   , MATCH
--   , MID
--   , NO
--   , NOT
--   , NOTATION
--   , NOTATIONAL
--   , OBVIOUS
--   , OF
--   , OFF
--   , ON
--   , ONLY
--   , ONTORED
--   , OR
--   , PAIRWISE
--   , PARAMETERS
--   , PARAMETRIC
--   , PRECEDENCE
--   , PRINTGOAL
--   , PROOF
--   , PROP
--   , PROVE
--   , PROPOSITION
--   , PROPPED
--   , QED
--   , QUOTIENT
--   , READ
--   , RECORD
--   , REGISTER
--   , RECURSION
--   , REMOVE
--   , RESOLVED
--   , RIGHT
--   , SAID
--   , SATISFYING
--   , SAY
--   , SECTION
--   , SHOW
--   , SOME
--   , STAND
--   , STRUCTURE
--   , SUBSECTION
--   , SUBSUBSECTION
--   , SUBTYPEMID
--   , SUCH
--   , SUPPOSE
--   , SYNONYM
--   , TAKE
--   , THAT
--   , THE
--   , THEN
--   , THEOREM
--   , THERE
--   , THEREFORE
--   , THESIS
--   , THIS
--   , TIMELIMIT
--   , TO
--   , TOTAL
--   , TRIVIAL
--   , TRUE
--   , TYPE
--   , TYPEABLE
--   , UNIQUE
--   , US
--   , WE
--   , WELL
--   , WELLDEFINED
--   , WELL_DEFINED
--   , WELL_PROPPED
--   , WITH
--   , WITHOUT
--   , WRONG
--   , YES]

-- -- parseLit :: Parser Lit
-- -- parseLit = fold (map parseLit_aux memsOfLit)

newtype Token = Token Text

data EOF = EOF

newtype Number = Number Text

data Decimal = Decimal {dec_left :: Text, dec_right :: Text}

data Numeric = Numeric {num_left :: Text, num_right :: Text}

newtype Symbol = Symbol Text

newtype Symbol_QED = Symbol_QED Text

data LParen = LParen
data RParen = RParen
data LBrack = LBrack
data RBrack = RBrack
data LBrace = LBrace
data RBrace = RBrace

data At = At
data MapsTo = MapsTo
data Period = Period
data Comma = Comma
data Semicolon = Semicolon
data Colon = Colon
data Assign = Assign
data RArrow = RArrow
data LArrow = LArrow
data Blank = Blank
data Alt = Alt
data Slash = Slash
data SlashDash = SlashDash
newtype Var = Var Text
newtype TkString = TkString Text
newtype AtomicId = AtomicId Text
newtype HierId = HierId [AtomicId]

data AtomicId_or_Number_or_Var =
    OfAtomicId AtomicId
  | OfNumber Number
  | OfVar Var

newtype FieldAcc = FieldAcc AtomicId_or_Number_or_Var
data Coercion = Coercion
data NotImplemented = NotImplemented
data NotDebugged = NotDebugged
newtype ControlSequence = ControlSequence Text
  
-- data Token =
--     EOF
--   | Number Text
--   | Decimal Text Text
--   | Numeric Text Text
--   | Symbol Text
--   | Symbol_QED Text
--   | LParen
--   | RParen
--   | LBrack
--   | RBrack
--   | LBrace
--   | RBrace
--   | Lit Lit
--   | At
--   | MapsTo
--   | Period
--   | Comma
--   | Semicolon
--   | Colon
--   | Assign
--   | RArrow
--   | LArrow
--   | Blank
--   | Alt
--   | Slash
--   | SlashDash
--   | Var Text
--   | Tk Text
--   | String Text
--   | AtomicId Text
--   | HierId [Token]
--   | FieldAcc Token
--   | Coercion
--   | NotImplemented
--   | NotDebugged
--   | ControlSequence Text
--   deriving (Show, Eq)  

parseEOF :: Parser EOF
parseEOF = eof *> return EOF

parseNumber :: Parser Number
parseNumber = (number <* sc) >>= return . Number

parseDecimal :: Parser Decimal
parseDecimal = (do
  n1 <- (number <* ch '.')
  n2 <- (number)
  return $ Decimal n1 n2) <* sc

parseNumeric :: Parser Numeric
parseNumeric = do
  op <- ((symbol "+") <||> (symbol "-"))
  n  <- number
  return $ Numeric op n

parseSymbol :: Parser Symbol
parseSymbol = ((foldr (<||>) empty (map ch ['!', '@', '#', '$', '^', '&', '*', '-', '+', '=', '<', '>', '/', '.'])) >>= return . Symbol) <* sc

parseSymbol_QED :: Parser Symbol_QED
parseSymbol_QED = ((symbol' "qed" )
               <||> (symbol  "◽"   )
               <||> (symbol  "◻"   )
               <||> (symbol  "◾"   )
               <||> (symbol  "◼"   )
                  ) >>= return . Symbol_QED
                  
parseLParen :: Parser LParen
parseLParen = (ch '(' >> return LParen) <* sc

parseRParen :: Parser RParen
parseRParen = (ch ')' >> return RParen) <* sc

parseLBrack :: Parser LBrack
parseLBrack = (ch '[' >> return LBrack) <* sc

parseRBrack :: Parser RBrack
parseRBrack = (ch ']' >> return RBrack) <* sc

parseLBrace :: Parser LBrace
parseLBrace = (ch '{' >> return LBrace) <* sc

parseRBrace :: Parser RBrace
parseRBrace = (ch '}' >> return RBrace) <* sc

-- parseLitToken :: Parser Token
-- parseLitToken = parseLit >>= return . Lit

parseAt :: Parser At
parseAt = parseDataHelper0 At "@"

parseMapsTo :: Parser MapsTo
parseMapsTo = (parseDataHelper0 MapsTo "|->")
          <||> (parseDataHelper0 MapsTo "↦")

-- A period must be followed by at least one whitespace character, or EOF
parsePeriod :: Parser Period
parsePeriod = (do
       (ch '.')
       (((lookAhead' spaceChar) <||> (lookAhead' eof)) >> return Period)) <* sc
  -- <||> ((lookAhead eof) >> return Period)

parseComma :: Parser Comma
parseComma = parseDataHelper0 Comma ","

parseSemicolon :: Parser Semicolon
parseSemicolon = parseDataHelper0 Semicolon ";"

parseColon :: Parser Colon
parseColon = parseDataHelper0 Colon ":"

parseAssign :: Parser Assign
parseAssign = parseDataHelper0 Assign ":="

parseRArrow :: Parser RArrow
parseRArrow = parseDataHelper0 RArrow "->"
          <||> parseDataHelper0 RArrow "→"

parseLArrow :: Parser LArrow
parseLArrow = parseDataHelper0 LArrow "<-"
          <||> parseDataHelper0 LArrow "←"
          
parseBlank :: Parser Blank
parseBlank = parseDataHelper0 Blank "_"

parseAlt :: Parser Alt
parseAlt = parseDataHelper0 Alt "|"

parseSlash :: Parser Slash
parseSlash = parseDataHelper0 Slash "/"

parseSlashDash :: Parser SlashDash
parseSlashDash = parseDataHelper0 SlashDash "/-"

var :: Parser Text
var = do a <- alpha
         ts    <- (many $ digit <||> ch '_' <||> ch '\'') >>= return . join
         return $ a <> ts

parseVar :: Parser Var
parseVar = (do
  x <- var
  (lookAhead' spaceChar) <||> lookAhead' (char '.') <||> lookAhead' (parseEOF)
  return $ Var $ x) <* sc

-- TODO(jesse) later, make sure to implement check against being substring of an identifier
-- A token is a string of alpha characters which is not followed by a period and another alpha character
parseToken :: Parser Token
parseToken = do
  as <- many1 alpha <* sc
  notFollowedBy (char '.' >> (alpha <||> digit))
  return $ Token . join $ as

parseTkString :: Parser TkString
parseTkString = (between (ch '"') (ch '"') str >>= return . TkString) <* sc
  where str = (many $ (ch '\\' <+> item) <||> not_ch '"') >>= return . join
  
  -- (do
  -- as <- many1 alpha <* sc -- the nested lookAheads look insane, but seem to work
  -- b <- succeeds (lookAhead(lookAhead(char '.') >> char '.' >> (alpha) <||> (digit)) <||> fail "foo")
  -- if b then fail "parseTk failed, alpha string followed by period and alphanumeric"
  --      else return $ Tk . join $ as) <* sc

  -- (many1 alpha <* spaceChar >>= return . Tk . join)
      -- <||> (do as <- many1 alpha
      --          lookAhead (char '.' >> (lookAhead eof) <||> (lookAhead whiteChar))
      -- <||> (many1 alpha <* eof >>= return . Tk .join)
 
  -- (spaceChar >> return $ Lit YES) <|> parseEOF
  -- (spaceChar) <|> eof
  

  -- (many1 alpha) >>= return . Tk . join) <* sc

atomicid :: Parser Text
atomicid = (do
  alph <- alpha
  rest <- (many' $ alpha <||> digit <||> ch '_') >>= return . join
  -- guard $ (any C.isAlpha (unpack rest)) || (any (\x -> x == (pack . pure $ '_')) rest) -- TODO fix this
  return $ alph <> rest) <* sc

parseAtomicId :: Parser AtomicId
parseAtomicId = atomicid >>= return . AtomicId

hierid :: Parser [AtomicId]
hierid = do
  (sepby1 (atomicid) (ch '.') >>= return . (map AtomicId)) <* sc

parseHierId :: Parser HierId
parseHierId = do
  at_ids <- hierid
  return $ HierId at_ids


parseFieldAcc :: Parser FieldAcc
parseFieldAcc =
      ((do (ch '.') *> (parseAtomicId >>= return . FieldAcc . OfAtomicId))
  <||> (do (ch '.') *> (parseNumber >>= return . FieldAcc . OfNumber))
  <||> (do (ch '.') *> (parseVar >>= return . FieldAcc . OfVar))) <* sc
    
parseCoercion :: Parser Coercion
parseCoercion = (str "↑" <||> str "^|") >> return Coercion

parseNotImplemented :: Parser NotImplemented
parseNotImplemented =  parseDataHelper NotImplemented "NOT_IMPLEMENTED"

parseNotDebugged :: Parser NotDebugged
parseNotDebugged = parseDataHelper NotDebugged "NOT_DEBUGGGED"

controlsequence :: Parser Text
controlsequence = do
  bs <- ch '\\'
  as <- (many1 alpha)
  return $ bs <> (join as)

parseControlSequence :: Parser ControlSequence
parseControlSequence = (controlsequence >>= return . ControlSequence) <* sc

-- TODO(jesse) define csbrace parser
  
-- TODO(jesse) define token parser

-- parseToken parses any token but EOF
-- parseToken :: Parser Token
-- parseToken =
--        parseDecimal -- attempt to parse decimals first to disambiguate
--   <||> parseNumber
--   <||> parseNumeric
--   -- <||> parseLitToken
--   <||> parseVar
--   <||> parseTk
--   <||> parseString
--   <||> parseControlSequence
--   <||> parseFieldAcc
--   <||> parseHierId 
--   -- <||> parseAtomicId -- AtomicIds will never be parsed, but
--                         -- it is OK to extract them from singleton HierIds later
--   <||> parsePeriod -- parsePeriod succeeds iff the next two characters are a period and whitespace.
--   <||> parseSymbol -- if parsePeriod fails but the next character is a period,
--                    -- then the current state is .* where * is not a whitespace.
--                    -- if parseSymbol fails, then the current state is .* where * is not a symbol character.

--   <||> parseSymbol_QED
--   <||> parseLParen
--   <||> parseRParen
--   <||> parseLBrack
--   <||> parseRBrack
--   <||> parseLBrace
--   <||> parseRBrace
--   <||> parseAt
--   <||> parseMapsTo
--   <||> parseComma
--   <||> parseSemicolon
--   <||> parseAssign
--   <||> parseRArrow
--   <||> parseLArrow
--   <||> parseBlank
--   <||> parseAlt
--   <||> parseSlash
--   <||> parseSlashDash
--   <||> parseCoercion
--   <||> parseNotImplemented
--   <||> parseNotDebugged

-- parseTokens :: Parser [Token]
-- parseTokens =
--   sc *> ((parseEOF >>= return . pure) -- don't fail on empty input
--   <|> do tks <- many1 parseToken
--          eof <- parseEOF
--          return $ tks ++ [eof])
-- -
-- newtype Label = Label Token
--   deriving (Show, Eq)

-- parseLabel :: Parser Label
-- parseLabel = parseAtomicId >>= return . Label

litTestString :: Text
litTestString = "a any APPLICABLE induction"

tkTestString :: Text
tkTestString = "Let C := the category of semi-symplectic topological quantum paramonoids \\mathcal{P} of Rice-Paddy type satisfying the Mussolini-Rostropovich equations at infinity. Then C.objects and C.morphisms are both trivial. QED."

-- testLit :: IO ()
-- testLit = do
--   test (many1 parseLit) litTestString

-- testTk :: IO ()
-- testTk = do
--   parseTest (parseTokens) tkTestString

-- -- for interactive debugging. make sure that in ghci, you have entered
-- -- :set -XOverloadedStrings

-- -- example usage:
-- -- test_lexer "HEWWO.HEWWO"
-- test_lexer :: Text -> IO ()
-- test_lexer = parseTest parseTokens
