{-
Author(s): Jesse Michael Han (2019)

Tokenization of input.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Token (
  -- TODO(jesse) expose functions
) where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import qualified CNLean.Basic as B

type Parser = Parsec Void Text

-- literal tokens
data Lit =
    A
  | ALL
  | AN
  | ANALYSIS
  | AND
  | ANY
  | APPLICABLE
  | ARE
  | ARTICLE
  | AS
  | ASSOCIATIVITY
  | ASSUME
  | ASSUMING
  | AXIOM
  | BE
  | BY
  | CALLED
  | CAN
  | CANONICAL
  | CASE
  | CHOOSE
  | CLASSIFIER
  | CLASSIFIERS
  | CONJECTURE
  | CONTRADICTION
  | CONTRARY
  | COROLLARY
  | DEF
  | DEFINE
  | DEFINED
  | DEFINITION
  | DENOTE
  | DO
  | DOCUMENT
  | DOES
  | DUMP
  | EACH
  | ELSE
  | EMBEDDED
  | END
  | EQUAL
  | EVERY
  | EXHAUSTIVE
  | EXIST
  | EXISTS
  | EXIT
  | FALSE
  | FIXED
  | FOR
  | FORALL
  | FUN
  | FUNCTION
  | HAS
  | HAVE
  | HAVING
  | HENCE
  | HOLDING
  | HYPOTHESIS
  | IF
  | IFF
  | IMPLICIT
  | IN
  | INDEED
  | INDUCTION
  | INDUCTIVE
  | IS
  | IT
  | LEFT
  | LEMMA
  | LET
  | LIBRARY
  | MATCH
  | MID
  | NO
  | NOT
  | NOTATION
  | NOTATIONAL
  | OBVIOUS
  | OF
  | OFF
  | ON
  | ONLY
  | ONTORED
  | OR
  | PAIRWISE
  | PARAMETERS
  | PARAMETRIC
  | PRECEDENCE
  | PRINTGOAL
  | PROOF
  | PROP
  | PROVE
  | PROPOSITION
  | PROPPED
  | QED
  | QUOTIENT
  | READ
  | RECORD
  | REGISTER
  | RECURSION
  | REMOVE
  | RESOLVED
  | RIGHT
  | SAID
  | SATISFYING
  | SAY
  | SECTION
  | SHOW
  | SOME
  | STAND
  | STRUCTURE
  | SUBSECTION
  | SUBSUBSECTION
  | SUBTYPEMID
  | SUCH
  | SUPPOSE
  | SYNONYM
  | TAKE
  | THAT
  | THE
  | THEN
  | THEOREM
  | THERE
  | THEREFORE
  | THESIS
  | THIS
  | TIMELIMIT
  | TO
  | TOTAL
  | TRIVIAL
  | TRUE
  | TYPE
  | TYPEABLE
  | UNIQUE
  | US
  | WE
  | WELL
  | WELLDEFINED
  | WELL_DEFINED
  | WELL_PROPPED
  | WITH
  | WITHOUT
  | WRONG
  | YES

parseDataHelper :: a -> Text -> (Parser a)
parseDataHelper l arg = (B.symbol' arg >> return l)

parseLit :: Lit -> (Parser Lit)
parseLit l = case l of
  A -> parseDataHelper A "a"
  ALL -> parseDataHelper ALL "all"
  AN -> parseDataHelper AN "an"
  ANALYSIS -> parseDataHelper ANALYSIS "analysis"
  AND -> parseDataHelper AND "and"
  ANY -> parseDataHelper ANY "any"
  APPLICABLE -> parseDataHelper APPLICABLE "applicable"
  ARE -> parseDataHelper ARE "are"
  ARTICLE -> parseDataHelper ARTICLE "article"  
  AS -> parseDataHelper AS "as"
  ASSOCIATIVITY -> parseDataHelper ASSOCIATIVITY "associativity"
  ASSUME -> parseDataHelper ASSUME "assume"
  ASSUMING -> parseDataHelper ASSUMING "assuming"
  AXIOM -> parseDataHelper AXIOM "axiom"
  BE -> parseDataHelper BE "be"
  BY -> parseDataHelper BY "by"
  CALLED -> parseDataHelper CALLED "called"
  CAN -> parseDataHelper CAN "can"
  CANONICAL -> parseDataHelper CANONICAL "canonical"
  CASE -> parseDataHelper CASE "case"
  CHOOSE -> parseDataHelper CHOOSE "choose"
  CLASSIFIER -> parseDataHelper CLASSIFIER "classifier"
  CLASSIFIERS -> parseDataHelper CLASSIFIERS "classifiers"
  CONJECTURE -> parseDataHelper CONJECTURE "conjecture"
  CONTRADICTION -> parseDataHelper CONTRADICTION "contradiction"
  CONTRARY -> parseDataHelper CONTRARY "contrary"
  COROLLARY -> parseDataHelper COROLLARY "corollary"
  DEF -> parseDataHelper DEF "def"
  DEFINE -> parseDataHelper DEFINE "define"
  DEFINED -> parseDataHelper DEFINED "defined"
  DEFINITION -> parseDataHelper DEFINITION "DEFINITION"
  DENOTE -> parseDataHelper DENOTE "denote"
  DO -> parseDataHelper DO "do"
  DOCUMENT -> parseDataHelper DOCUMENT "document"
  DOES -> parseDataHelper DOES "does"
  DUMP -> parseDataHelper DUMP "dump"
  EACH -> parseDataHelper EACH "each"
  ELSE -> parseDataHelper ELSE "else"
  EMBEDDED -> parseDataHelper EMBEDDED "embedded"
  END -> parseDataHelper END "end"
  EQUAL -> parseDataHelper EQUAL "equal"
  EVERY -> parseDataHelper EVERY "every"
  EXHAUSTIVE -> parseDataHelper EXHAUSTIVE "exhaustive"
  EXIST -> parseDataHelper EXIST "exist"
  EXISTS -> parseDataHelper EXISTS "exists"
  EXIT -> parseDataHelper EXIT "exit"
  FALSE -> parseDataHelper FALSE "false"
  FIXED -> parseDataHelper FIXED "fixed"
  FOR -> parseDataHelper FOR "for"
  FORALL -> parseDataHelper FORALL "forall"
  FUN -> parseDataHelper FUN "fun"
  FUNCTION -> parseDataHelper FUNCTION "function"
  HAS -> parseDataHelper HAS "has"
  HAVE -> parseDataHelper HAVE "have"
  HAVING -> parseDataHelper HAVING "having"
  HENCE -> parseDataHelper HENCE "hence"
  HOLDING -> parseDataHelper HOLDING "holding"
  HYPOTHESIS -> parseDataHelper HYPOTHESIS "hypothesis"
  IF -> parseDataHelper IF "if"
  IFF -> parseDataHelper IFF "iff"
  IMPLICIT -> parseDataHelper IMPLICIT "implicit"
  IN -> parseDataHelper IN "in"
  INDEED -> parseDataHelper INDEED "indeed"
  INDUCTION -> parseDataHelper INDUCTION "induction"
  INDUCTIVE -> parseDataHelper INDUCTIVE "inductive"
  IS -> parseDataHelper IS "is"
  IT -> parseDataHelper IT "it"
  LEFT -> parseDataHelper LEFT "left"
  LEMMA -> parseDataHelper LEMMA "lemma"
  LET -> parseDataHelper LET "let"
  LIBRARY -> parseDataHelper LIBRARY "library"
  MATCH -> parseDataHelper MATCH "match"
  MID -> parseDataHelper MID "mid"
  NO -> parseDataHelper NO "no"
  NOT -> parseDataHelper NOT "not"
  NOTATION -> parseDataHelper NOTATION "notation"
  NOTATIONAL -> parseDataHelper NOTATIONAL "notational"
  OBVIOUS -> parseDataHelper OBVIOUS "obvious"
  OF -> parseDataHelper OF "of"
  OFF -> parseDataHelper OFF "off"
  ON -> parseDataHelper ON "on"
  ONLY -> parseDataHelper ONLY "only"
  ONTORED -> parseDataHelper ONTORED "ontored"
  OR -> parseDataHelper OR "or"
  PAIRWISE -> parseDataHelper PAIRWISE "pairwise"
  PARAMETERS -> parseDataHelper PARAMETERS "parameters"
  PARAMETRIC -> parseDataHelper PARAMETRIC "parametric"
  PRECEDENCE -> parseDataHelper PRECEDENCE "precedence"
  PRINTGOAL -> parseDataHelper PRINTGOAL "printgoal"
  PROOF -> parseDataHelper PROOF "proof"
  PROP -> parseDataHelper PROP "prop"
  PROVE -> parseDataHelper PROVE "prove"
  PROPOSITION -> parseDataHelper PROPOSITION "proposition"
  PROPPED -> parseDataHelper PROPPED "propped"
  QED -> parseDataHelper QED "qed"
  QUOTIENT -> parseDataHelper QUOTIENT "quotient"
  READ -> parseDataHelper READ "read"
  RECORD -> parseDataHelper RECORD "record"
  REGISTER -> parseDataHelper REGISTER "register"
  RECURSION -> parseDataHelper RECURSION "recursion"
  REMOVE -> parseDataHelper REMOVE "remove"
  RESOLVED -> parseDataHelper RESOLVED "resolved"
  RIGHT -> parseDataHelper RIGHT "right"
  SAID -> parseDataHelper SAID "said"
  SATISFYING -> parseDataHelper SATISFYING "satisfying"
  SAY -> parseDataHelper SAY "say"
  SECTION -> parseDataHelper SECTION "section"
  SHOW -> parseDataHelper SHOW "show"
  SOME -> parseDataHelper SOME "some"
  STAND -> parseDataHelper STAND "stand"
  STRUCTURE -> parseDataHelper STRUCTURE "structure"
  SUBSECTION -> parseDataHelper SUBSECTION "subsection"
  SUBSUBSECTION -> parseDataHelper SUBSUBSECTION "subsubsection"
  SUBTYPEMID -> parseDataHelper SUBTYPEMID "subtypemid"
  SUCH -> parseDataHelper SUCH "such"
  SUPPOSE -> parseDataHelper SUPPOSE "suppose"
  SYNONYM -> parseDataHelper SYNONYM "synonym"
  TAKE -> parseDataHelper TAKE "take"
  THAT -> parseDataHelper THAT "that"
  THE -> parseDataHelper THE "the"
  THEN -> parseDataHelper THEN "then"
  THEOREM -> parseDataHelper THEOREM "theorem"
  THERE -> parseDataHelper THERE "there"
  THEREFORE -> parseDataHelper THEREFORE "therefore"
  THESIS -> parseDataHelper THESIS "thesis"
  THIS -> parseDataHelper THIS "this"
  TIMELIMIT -> parseDataHelper TIMELIMIT "timelimit"
  TO -> parseDataHelper TO "to"
  TOTAL -> parseDataHelper TOTAL "total"
  TRIVIAL -> parseDataHelper TRIVIAL "trivial"
  TRUE -> parseDataHelper TRUE "true"
  TYPE -> parseDataHelper TYPE "type"
  TYPEABLE -> parseDataHelper TYPEABLE "typeable"
  UNIQUE -> parseDataHelper UNIQUE "unique"
  US -> parseDataHelper US "us"
  WE -> parseDataHelper WE "we"
  WELL -> parseDataHelper WELL "well"
  WELLDEFINED -> parseDataHelper WELLDEFINED "welldefined"
  WELL_DEFINED -> parseDataHelper WELL_DEFINED "well_defined"
  WELL_PROPPED -> parseDataHelper WELL_PROPPED "well_propped"
  WITH -> parseDataHelper WITH "with"
  WITHOUT -> parseDataHelper WITHOUT "without"
  WRONG -> parseDataHelper WRONG "wrong"
  YES -> parseDataHelper YES "yes"
  
data Token =
    EOF
  | Number Text
  | Decimal Text Text
  | Numeric Text Text
  | Symbol Text
  | Symbol_QED Text
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  | Lit Lit
  | At
  | MapsTo
  | Period
  | Comma
  | Semicolon
  | Assign
  | RArrow
  | LArrow
  | Blank
  | Alt
  | Slash
  | SlashDash
  | Var Text
  | Tk Text
  | AtomicId Text
  | HierId Text
  | FieldAcc Text
  | Coercion
  | NotImplemented
  | NotDebugged
  | ControlSequence Text

parseEOF :: Parser Token
parseEOF = eof *> return EOF

parseNumber :: Parser Token
parseNumber = B.number >>= return . Number

parseDecimal :: Parser Token
parseDecimal = do
  n1 <- (B.number <* char '.')
  n2 <- (B.number)
  return $ Decimal n1 n2

parseNumeric :: Parser Token
parseNumeric = do
  op <- ((B.symbol "+") <|> (B.symbol "-"))
  n  <- B.number
  return $ Numeric op n

parseSymbol :: Parser Token
parseSymbol = (foldr (<|>) empty (map B.char ['!', '@', '#', '$', '^', '&', '*', '-', '+', '=', '<', '>', '/', '.'])) >>= return . Symbol

parseSymbol_QED :: Parser Token
parseSymbol_QED = (
                   (B.symbol' "qed" )
               <|> (B.symbol' "qed.")
               <|> (B.symbol  "◽"   )
               <|> (B.symbol  "◻"   )
               <|> (B.symbol  "◾"   )
               <|> (B.symbol  "◼"   )
                  ) >>= return . Symbol_QED
                  
parseLParen :: Parser Token
parseLParen = parseDataHelper LParen "("

parseRParen :: Parser Token
parseRParen = parseDataHelper RParen ")"

parseLBrack :: Parser Token
parseLBrack = parseDataHelper LBrack "["

parseRBrack :: Parser Token
parseRBrack = parseDataHelper RBrack "]"

parseLBrace :: Parser Token
parseLBrace = parseDataHelper LBrace "{"

parseRBrace :: Parser Token
parseRBrace = parseDataHelper RBrace "}"

-- parseLit :: Parser Token
parseAt :: Parser Token
parseAt = parseDataHelper At "@"

parseMapsTo :: Parser Token
parseMapsTo = (parseDataHelper MapsTo "|->")
          <|> (parseDataHelper MapsTo "↦")

parsePeriod :: Parser Token
parsePeriod = (parseDataHelper Period ".")

parseComma :: Parser Token
parseComma = parseDataHelper Comma ","

parseSemicolon :: Parser Token
parseSemicolon = parseDataHelper Semicolon ";"

parseAssign :: Parser Token
parseAssign = parseDataHelper Assign ":="

parseRArrow :: Parser Token
parseRArrow = parseDataHelper RArrow "->"
          <|> parseDataHelper RArrow "→"

parseLArrow :: Parser Token
parseLArrow = parseDataHelper LArrow "<-"
          <|> parseDataHelper LArrow "←"

          
parseBlank :: Parser Token
parseBlank = parseDataHelper Blank "_"

-- TODO(jesse) fix this
parseAlt :: Parser Token
parseAlt = parseDataHelper Alt "|"

parseSlash :: Parser Token
parseSlash = parseDataHelper Slash "/"

parseSlashDash :: Parser Token
parseSlashDash = parseDataHelper SlashDash "/-"

parseVar :: Parser Token
parseVar = do
  alpha <- B.alpha
  ts    <- (B.many1 (B.digit <|> B.char '_' <|> B.char '\''))
  return $ Var $ alpha <> (B.join ts)

-- TODO(jesse) implement check against being substring of an identifier
parseTk :: Parser Token
parseTk = (B.many1 B.alpha) >>= return . Tk . B.join

atomicid :: Parser Text
atomicid = do
  alph <- B.alpha
  rest <- B.alphanum
  return $ alph <> rest

parseAtomicId :: Parser Token
parseAtomicId = atomicid >>= return . AtomicId

hierid :: Parser Text
hierid = do
  PC.sepBy (atomicid) (B.char '.') >>= return . B.join

parseHierId :: Parser Token
parseHierId = do
  hierid >>= return . HierId

parseFieldAcc :: Parser Token
parseFieldAcc = do
  p   <- (B.char '.')
  x <- (hierid <|> B.number)
  return $ FieldAcc $ p <> x
    
parseCoercion :: Parser Token
parseCoercion = (B.string "↑" <|> B.string "^|") >> return Coercion

parseNotImplemented :: Parser Token
parseNotImplemented =  parseDataHelper NotImplemented "NOT_IMPLEMENTED"

parseNotDebugged :: Parser Token
parseNotDebugged = parseDataHelper NotDebugged "NOT_DEBUGGED"

controlsequence :: Parser Text
controlsequence = do
  bs <- B.char '\\'
  as <- (B.many1 B.alpha)
  return $ bs <> (B.join as)

parseControlSequence :: Parser Token
parseControlSequence = controlsequence >>= return . ControlSequence
  
-- TODO(jesse) define token parser
