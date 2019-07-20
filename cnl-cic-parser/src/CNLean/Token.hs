{-
Author(s): Jesse Michael Han (2019)

Tokenization of input.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.Token where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic

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
  | AT
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
  deriving (Show, Eq)

parseDataHelper :: a -> Text -> (Parser a)
parseDataHelper l arg = ((do
  x <- not_whitespace
  (if str_eq' x arg then return l else fail "parseDataHelper failed to match")) <||> fail "parseDataHelper failed to match'") <* sc

parseDataHelper0 :: a -> Text -> (Parser a)
parseDataHelper0 l arg = (str' arg >> return l) <* sc

parseLit_aux :: Lit -> (Parser Lit)
parseLit_aux l = case l of
  A -> parseDataHelper A "a"
  ALL -> parseDataHelper ALL "all"
  AN -> parseDataHelper AN "an"
  AT -> parseDataHelper AT "at"
  ANALYSIS -> parseDataHelper ANALYSIS "analysis"
  AND -> parseDataHelper AND "and"
  ANY -> parseDataHelper ANY "any"
  APPLICABLE -> parseDataHelper APPLICABLE "applicable"
  ARE -> parseDataHelper ARE "are"
  ARTICLE -> parseDataHelper0 ARTICLE "article"
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
  EXIT -> parseDataHelper0 EXIT "exit"
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
  SECTION -> parseDataHelper0 SECTION "section"
  SHOW -> parseDataHelper SHOW "show"
  SOME -> parseDataHelper SOME "some"
  STAND -> parseDataHelper STAND "stand"
  STRUCTURE -> parseDataHelper STRUCTURE "structure"
  SUBSECTION -> parseDataHelper0 SUBSECTION "subsection"
  SUBSUBSECTION -> parseDataHelper0 SUBSUBSECTION "subsubsection"
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

memsOfLit :: [Lit]
memsOfLit = [A
  , ALL
  , AN
  , AT
  , ANALYSIS
  , AND
  , ANY
  , APPLICABLE
  , ARE
  , ARTICLE
  , AS
  , ASSOCIATIVITY
  , ASSUME
  , ASSUMING
  , AXIOM
  , BE
  , BY
  , CALLED
  , CAN
  , CANONICAL
  , CASE
  , CHOOSE
  , CLASSIFIER
  , CLASSIFIERS
  , CONJECTURE
  , CONTRADICTION
  , CONTRARY
  , COROLLARY
  , DEF
  , DEFINE
  , DEFINED
  , DEFINITION
  , DENOTE
  , DO
  , DOCUMENT
  , DOES
  , DUMP
  , EACH
  , ELSE
  , EMBEDDED
  , END
  , EQUAL
  , EVERY
  , EXHAUSTIVE
  , EXIST
  , EXISTS
  , EXIT
  , FALSE
  , FIXED
  , FOR
  , FORALL
  , FUN
  , FUNCTION
  , HAS
  , HAVE
  , HAVING
  , HENCE
  , HOLDING
  , HYPOTHESIS
  , IF
  , IFF
  , IMPLICIT
  , IN
  , INDEED
  , INDUCTION
  , INDUCTIVE
  , IS
  , IT
  , LEFT
  , LEMMA
  , LET
  , LIBRARY
  , MATCH
  , MID
  , NO
  , NOT
  , NOTATION
  , NOTATIONAL
  , OBVIOUS
  , OF
  , OFF
  , ON
  , ONLY
  , ONTORED
  , OR
  , PAIRWISE
  , PARAMETERS
  , PARAMETRIC
  , PRECEDENCE
  , PRINTGOAL
  , PROOF
  , PROP
  , PROVE
  , PROPOSITION
  , PROPPED
  , QED
  , QUOTIENT
  , READ
  , RECORD
  , REGISTER
  , RECURSION
  , REMOVE
  , RESOLVED
  , RIGHT
  , SAID
  , SATISFYING
  , SAY
  , SECTION
  , SHOW
  , SOME
  , STAND
  , STRUCTURE
  , SUBSECTION
  , SUBSUBSECTION
  , SUBTYPEMID
  , SUCH
  , SUPPOSE
  , SYNONYM
  , TAKE
  , THAT
  , THE
  , THEN
  , THEOREM
  , THERE
  , THEREFORE
  , THESIS
  , THIS
  , TIMELIMIT
  , TO
  , TOTAL
  , TRIVIAL
  , TRUE
  , TYPE
  , TYPEABLE
  , UNIQUE
  , US
  , WE
  , WELL
  , WELLDEFINED
  , WELL_DEFINED
  , WELL_PROPPED
  , WITH
  , WITHOUT
  , WRONG
  , YES]

parseLit :: Parser Lit
parseLit = fold (map parseLit_aux memsOfLit)
  
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
  | Colon
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
  | HierId [Token]
  | FieldAcc Token
  | Coercion
  | NotImplemented
  | NotDebugged
  | ControlSequence Text
  deriving (Show, Eq)  

parseEOF :: Parser Token
parseEOF = eof *> return EOF

parseNumber :: Parser Token
parseNumber = (number <* sc) >>= return . Number

parseDecimal :: Parser Token
parseDecimal = (do
  n1 <- (number <* ch '.')
  n2 <- (number)
  return $ Decimal n1 n2) <* sc

parseNumeric :: Parser Token
parseNumeric = do
  op <- ((symbol "+") <||> (symbol "-"))
  n  <- number
  return $ Numeric op n

parseSymbol :: Parser Token
parseSymbol = ((foldr (<||>) empty (map ch ['!', '@', '#', '$', '^', '&', '*', '-', '+', '=', '<', '>', '/', '.'])) >>= return . Symbol) <* sc

parseSymbol_QED :: Parser Token
parseSymbol_QED = ((symbol' "qed" )
               <||> (symbol  "◽"   )
               <||> (symbol  "◻"   )
               <||> (symbol  "◾"   )
               <||> (symbol  "◼"   )
                  ) >>= return . Symbol_QED
                  
parseLParen :: Parser Token
parseLParen = (ch '(' >> return LParen) <* sc

parseRParen :: Parser Token
parseRParen = (ch ')' >> return RParen) <* sc

parseLBrack :: Parser Token
parseLBrack = (ch '[' >> return LBrack) <* sc

parseRBrack :: Parser Token
parseRBrack = (ch ']' >> return RBrack) <* sc

parseLBrace :: Parser Token
parseLBrace = (ch '{' >> return LBrace) <* sc

parseRBrace :: Parser Token
parseRBrace = (ch '}' >> return RBrace) <* sc

parseLitToken :: Parser Token
parseLitToken = parseLit >>= return . Lit

parseAt :: Parser Token
parseAt = parseDataHelper At "@"

parseMapsTo :: Parser Token
parseMapsTo = (parseDataHelper MapsTo "|->")
          <||> (parseDataHelper MapsTo "↦")

-- A period must be followed by at least one whitespace character, or EOF
parsePeriod :: Parser Token
parsePeriod = (do
       (ch '.')
       (((lookAhead' spaceChar) <||> (lookAhead' eof)) >> return Period)) <* sc
  -- <||> ((lookAhead eof) >> return Period)

parseComma :: Parser Token
parseComma = parseDataHelper Comma ","

parseSemicolon :: Parser Token
parseSemicolon = parseDataHelper Semicolon ";"

parseColon :: Parser Token
parseColon = parseDataHelper Colon ":"

parseAssign :: Parser Token
parseAssign = parseDataHelper Assign ":="

parseRArrow :: Parser Token
parseRArrow = parseDataHelper RArrow "->"
          <||> parseDataHelper RArrow "→"

parseLArrow :: Parser Token
parseLArrow = parseDataHelper LArrow "<-"
          <||> parseDataHelper LArrow "←"

          
parseBlank :: Parser Token
parseBlank = parseDataHelper Blank "_"

-- TODO(jesse) fix this
parseAlt :: Parser Token
parseAlt = parseDataHelper Alt "|"

parseSlash :: Parser Token
parseSlash = parseDataHelper Slash "/"

parseSlashDash :: Parser Token
parseSlashDash = parseDataHelper SlashDash "/-"

var :: Parser Text
var = do a <- alpha
         ts    <- (many $ digit <||> ch '_' <||> ch '\'') >>= return . join
         return $ a <> ts

parseVar :: Parser Token
parseVar = (do
  x <- var
  (lookAhead' spaceChar) <||> lookAhead' (char '.') <||> lookAhead' (parseEOF)
  return $ Var $ x) <* sc



-- TODO(jesse) later, make sure to implement check against being substring of an identifier
-- A token is a string of alpha characters which is not followed by a period and another alpha character
parseTk :: Parser Token
parseTk = do
  as <- many1 alpha <* sc
  notFollowedBy (char '.' >> (alpha <||> digit))
  return $ Tk . join $ as


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

parseAtomicId :: Parser Token
parseAtomicId = atomicid >>= return . AtomicId

hierid :: Parser [Token]
hierid = do
  (sepby1 (atomicid) (ch '.') >>= return . (map AtomicId)) <* sc

parseHierId :: Parser Token
parseHierId = do
  at_ids <- hierid
  return $ HierId at_ids

parseFieldAcc :: Parser Token
parseFieldAcc = (do
  t <- (ch '.') *> (parseAtomicId <||> parseNumber <||> parseVar)
  return $ FieldAcc t) <* sc
    
parseCoercion :: Parser Token
parseCoercion = (str "↑" <||> str "^|") >> return Coercion

parseNotImplemented :: Parser Token
parseNotImplemented =  parseDataHelper NotImplemented "NOT_IMPLEMENTED"

parseNotDebugged :: Parser Token
parseNotDebugged = parseDataHelper NotDebugged "NOT_DEBUGGGED"

controlsequence :: Parser Text
controlsequence = do
  bs <- ch '\\'
  as <- (many1 alpha)
  return $ bs <> (join as)

parseControlSequence :: Parser Token
parseControlSequence = (controlsequence >>= return . ControlSequence) <* sc
  
-- TODO(jesse) define token parser

-- parseToken parses any token but EOF
parseToken :: Parser Token
parseToken =
       parseDecimal -- attempt to parse decimals first to disambiguate
  <||> parseNumber
  <||> parseNumeric
  <||> parseLitToken
  <||> parseVar
  <||> parseTk
  <||> parseControlSequence
  <||> parseFieldAcc
  <||> parseHierId 
  -- <||> parseAtomicId -- AtomicIds will never be parsed, but
                        -- it is OK to extract them from singleton HierIds later
  <||> parsePeriod -- parsePeriod succeeds iff the next two characters are a period and whitespace.
  <||> parseSymbol -- if parsePeriod fails but the next character is a period,
                   -- then the current state is .* where * is not a whitespace.
                   -- if parseSymbol fails, then the current state is .* where * is not a symbol character.

  <||> parseSymbol_QED
  <||> parseLParen
  <||> parseRParen
  <||> parseLBrack
  <||> parseRBrack
  <||> parseLBrace
  <||> parseRBrace
  <||> parseAt
  <||> parseMapsTo
  <||> parseComma
  <||> parseSemicolon
  <||> parseAssign
  <||> parseRArrow
  <||> parseLArrow
  <||> parseBlank
  <||> parseAlt
  <||> parseSlash
  <||> parseSlashDash
  <||> parseCoercion
  <||> parseNotImplemented
  <||> parseNotDebugged

parseTokens :: Parser [Token]
parseTokens =
  sc *> ((parseEOF >>= return . pure) -- don't fail on empty input
  <|> do tks <- many1 parseToken
         eof <- parseEOF
         return $ tks ++ [eof])

litTestString :: Text
litTestString = "a any APPLICABLE induction"

tkTestString :: Text
tkTestString = "Let C := the category of semi-symplectic topological quantum paramonoids \\mathcal{P} of Rice-Paddy type satisfying the Mussolini-Rostropovich equations at infinity. Then C.objects and C.morphisms are both trivial. QED."

testLit :: IO ()
testLit = do
  parseTest (many1 parseLit) litTestString

testTk :: IO ()
testTk = do
  parseTest (parseTokens) tkTestString

-- for interactive debugging. make sure that in ghci, you have entered
-- :set -XOverloadedStrings

-- example usage:
-- test_lexer "HEWWO.HEWWO"
test_lexer :: Text -> IO ()
test_lexer = parseTest parseTokens
