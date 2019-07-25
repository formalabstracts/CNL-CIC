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


-- test (parse_any_Lits [["foo", "bar"]]) "foo      bar"
parse_any_Lit :: [[Text]] -> Parser [Text]
parse_any_Lit phs = parse_any (rp . parseLit) phs

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

parseLitVarMod =
  (rp $ parseLit "fixed")    <||>
  (rp $ parseLit "implicit") <||>
  (rp $ parseLit "resolved") <||>
  (rp $ parseLit "remove")

parseLitParam = (rp $ parseLit "with") <+> (rp $ parseLit "parameters")
      
parseDataHelper :: a -> Text -> (Parser a)
parseDataHelper l arg = ((do
  x <- not_whitespace
  (if str_eq' x arg then return l else fail "parseDataHelper failed to match")) <||> fail "parseDataHelper failed to match'") <* sc

parseDataHelper0 :: a -> Text -> (Parser a)
parseDataHelper0 l arg = (str' arg >> return l) <* sc

newtype Token = Token Text
  deriving (Show, Eq)

data EOF = EOF
  deriving (Show, Eq)

newtype Number = Number Text
  deriving (Show, Eq)

data Decimal = Decimal {dec_left :: Text, dec_right :: Text}
  deriving (Show, Eq)

data Numeric = Numeric {num_left :: Text, num_right :: Text}
  deriving (Show, Eq)

newtype Symbol = Symbol Text
  deriving (Show, Eq)

newtype Symbol_QED = Symbol_QED Text
  deriving (Show, Eq)

data LParen = LParen
  deriving (Show, Eq)
data RParen = RParen
  deriving (Show, Eq)
data LBrack = LBrack
  deriving (Show, Eq)
data RBrack = RBrack
  deriving (Show, Eq)
data LBrace = LBrace
  deriving (Show, Eq)
data RBrace = RBrace
  deriving (Show, Eq)

data At = At
  deriving (Show, Eq)
data MapsTo = MapsTo
  deriving (Show, Eq)
data Period = Period
  deriving (Show, Eq)
data Comma = Comma
  deriving (Show, Eq)
data Semicolon = Semicolon
  deriving (Show, Eq)
data Colon = Colon
  deriving (Show, Eq)
data Assign = Assign
  deriving (Show, Eq)
data RArrow = RArrow
  deriving (Show, Eq)
data LArrow = LArrow
  deriving (Show, Eq)
data Blank = Blank
  deriving (Show, Eq)
data Alt = Alt
  deriving (Show, Eq)
data Slash = Slash
  deriving (Show, Eq)
data SlashDash = SlashDash
  deriving (Show, Eq)
newtype Var = Var Text
  deriving (Show, Eq)
newtype TkString = TkString Text
  deriving (Show, Eq)
newtype AtomicId = AtomicId Text
  deriving (Show, Eq)
newtype HierId = HierId [AtomicId]
  deriving (Show, Eq)

data AtomicId_or_Number_or_Var =
    OfAtomicId AtomicId
  | OfNumber Number
  | OfVar Var
  deriving (Show, Eq)

newtype FieldAcc = FieldAcc AtomicId_or_Number_or_Var
  deriving (Show, Eq)
data Coercion = Coercion
  deriving (Show, Eq)
data NotImplemented = NotImplemented
  deriving (Show, Eq)
data NotDebugged = NotDebugged
  deriving (Show, Eq)
newtype ControlSequence = ControlSequence Text
  deriving (Show, Eq)

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

parseTokenOfLit :: Text -> Parser Token -- TODO(jesse): insert guard to ensure that `arg` is Token-compliant
parseTokenOfLit arg = parseLit arg >>= return . Token

parseTokenOfToken :: Token -> Parser Token
parseTokenOfToken tk = case tk of
  Token txt -> parseTokenOfLit txt

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

parseOptParen :: Parser a -> Parser a
parseOptParen p = (between parseLParen parseRParen p) <||> p

brace_semi :: Eq a => Parser a -> Parser [a]
brace_semi p = between parseLBrace parseRBrace (sepby1 p parseSemicolon)  

-- litTestString :: Text
-- litTestString = "a any APPLICABLE induction"

-- tkTestString :: Text
-- tkTestString = "Let C := the category of semi-symplectic topological quantum paramonoids \\mathcal{P} of Rice-Paddy type satisfying the Mussolini-Rostropovich equations at infinity. Then C.objects and C.morphisms are both trivial. QED."

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
