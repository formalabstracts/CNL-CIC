{-
Author(s): Jesse Michael Han (2019)

Tokenization of input.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Basic.Token where

import Prelude hiding (Word)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option, Tokens)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack, intercalate)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Monad.Trans.State
import Control.Lens hiding (At)


import Colada.Basic.Core
import Colada.Basic.State
import Colada.Basic.ParserState

{-

   Parsers for basic terminals of the Colada grammar.

 - parseLit str will case-insensitively parse the exact string str, and consume trailing whitespace.

 - Any specialized non-alphanumeric token is given its own parser, named parseCamelCasedVersionOfName.

 - Parser for the period punctuation symbol requires it to be followed by whitespace or EOF.

 - For convenience, parser combinators for the common separated-list-type production rule combinators
   are provided with verbatim names:

    paren
    bracket
    brace
    opt_paren
    brace_semi
    sep_list
    sep_list1
    comma_nonempty_list

-}

---- `parseLit arg` case-insensitively parses `arg`, and then consumes whitespace to the right.
parseLit :: Text -> Parser Text
parseLit arg = (str' arg) <* sc


-- test (parse_any_Lits [["foo", "bar"]]) "foo      bar"
parse_any_Lit :: [[Text]] -> Parser [Text]
parse_any_Lit = parse_any ((pure <$>) . parseLit)

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

parseLitAssume :: Parser [Text]
parseLitAssume = (pure <$> parseLit "assume") <||> (pure <$> parseLit "suppose")

parseLitA = parseLit "an" <||> parseLit "a"

parseArticle = parseLit "the" <||> parseLit "a"

parseSepAndComma = parseLit "and" <||> parseLit ","

parseLitBinderComma = parseLit ","

parseOptDefine = (option $ parseLitLets) <+> (option $ pure <$> parseLit "define") <||> option (parseLitWeRecord)

parseLitDefinedAs :: Parser [Text]
parseLitDefinedAs =
       (pure <$> parseLit "said") <+> (pure <$> parseLit "to") <+> (pure <$> parseLit "be")
  <||> (pure <$> parseLit "defined") <+> (pure <$> parseLit "to") <+> (pure <$> parseLit "be")
  <||> (pure <$> parseLit "defined") <+> (pure <$> parseLit "as")

parseLitIff :: Parser [Text]
parseLitIff =
      (pure <$> parseLit "iff") <||> (pure <$> parseLit "if")
                             <+> (pure <$> parseLit "and")
                             <+> (pure <$> parseLit "only")
                             <+> (pure <$> parseLit "if")

parseLitDenote :: Parser [Text]
parseLitDenote =
  (pure <$> parseLit "denote") <||> (pure <$> parseLit "stand") <+> (pure <$> parseLit "for")

parseLitDo :: Parser [Text]
parseLitDo =
  (pure <$> parseLit "does") <||> (pure <$> parseLit "does")

parseLitIs :: Parser [Text]
parseLitIs =
       (pure <$> parseLit "is")
  <||> (pure <$> parseLit "are")
  <||> (pure <$> parseLit "to") <+> (pure <$> parseLit "be")
  <||> (pure <$> parseLit "be")

parseLitEqual :: Parser [Text]
parseLitEqual = (pure <$> parseLit "equal") <+> (pure <$> parseLit "to")

parseLitHas :: Parser [Text]
parseLitHas = (pure <$> parseLit "has") <||> (pure <$> parseLit "have")

parseLitWith :: Parser [Text]
parseLitWith =
       (pure <$> parseLit "")
  <||> (pure <$> parseLit "of")
  <||> (pure <$> parseLit "having")
  <||> (pure <$> parseLit "with")

parseLitTrue :: Parser [Text]
parseLitTrue =
       (pure <$> parseLit "on")
  <||> (pure <$> parseLit "true")
  <||> (pure <$> parseLit "yes")

parseLitFalse :: Parser [Text]
parseLitFalse =
       (pure <$> parseLit "off")
  <||> (pure <$> parseLit "false")
  <||> (pure <$> parseLit "no")

parseLitItsWrong :: Parser [Text]
parseLitItsWrong = (pure <$> parseLit "it") <+> (pure <$> parseLit "is") <+> (pure <$> parseLit "wrong") <+> (pure <$> parseLit "that")

parseLitWeRecord :: Parser [Text]
parseLitWeRecord =
  unoption $ option (pure <$> parseLit "we") <+>
             (Just <$> ((pure <$> parseLit "record") <||>
                        (pure <$> parseLit "register"))) <+>
             option (pure <$> parseLit "that")

parseLitAny :: Parser [Text]
parseLitAny =
       (pure <$> parseLit "every")
  <||> (pure <$> parseLit "each")
  <||> (pure <$> parseLit "each") <+> (pure <$> parseLit "and") <+> (pure <$> parseLit "every")
  <||> (pure <$> parseLit "all")
  <||> (pure <$> parseLit "any")
  <||> (pure <$> parseLit "some")
  <||> (pure <$> parseLit "no")

parseLitExist :: Parser [Text]
parseLitExist = (pure <$> parseLit "exists") <||> (pure <$> parseLit "exist")

parseLitThen :: Parser [Text]
parseLitThen =
       (pure <$> parseLit "then")
  <||> (pure <$> parseLit "therefore")
  <||> (pure <$> parseLit "hence")

parseLitChoose :: Parser [Text]
parseLitChoose = (pure <$> parseLit "take") <||> (pure <$> parseLit "choose")

parseLitProve :: Parser [Text]
parseLitProve = (pure <$> parseLit "prove") <||> (pure <$> parseLit "show")

parseLitWeSay :: Parser [Text]
parseLitWeSay = (pure <$> parseLit "we") <+> (pure <$> parseLit "say") <+> (pure <$> parseLit "that")
           <||> (pure <$> parseLit "we") <+> (pure <$> parseLit "say")

parseOptSay = option parseLitWeSay

parseLitLeft :: Parser [Text]
parseLitLeft =
       (pure <$> parseLit "left")
  <||> (pure <$> parseLit "right")
  <||> (pure <$> parseLit "no")

parseLitFieldKey :: Parser [Text]
parseLitFieldKey =
  (pure <$> parseLit "parameter")   <||>
  (pure <$> parseLit "type") <||>
  (pure <$> parseLit "map")

parseLitQED = ((pure <$>) parseLit "end") <||> (pure <$> parseLit "QED") <||> (pure <$> parseLit "obvious") <||> (pure <$> parseLit "trivial")

parseLitDocument_aux :: Parser ([Text], Int)
parseLitDocument_aux =
  parse_any_of_with_index $
    (:) (pure <$> (parseLit "document" <||> parseLit "article")) rest
  where
    rest :: [Parser [Text]]
    rest = map ((pure <$>) . parseLit) ["section", "subsection", "subsubsection"]


parseLitDocument = fst <$> parseLitDocument_aux

parseLitSection :: Parser [Text]
parseLitSection =  (pure <$> parseLit "section")
              <||> (pure <$> parseLit "subsection")
              <||> (pure <$> parseLit "subsubsection")

parseSubdivision_aux :: Parser ([Text], Int)
parseSubdivision_aux = (,) <$> parseLitSubdivision <*> ((+1) <$> depthStateVec <$> get)

parseLitSubdivision :: Parser [Text]
parseLitSubdivision = pure <$> parseLit "subdivision"

parseLitDef :: Parser [Text]
parseLitDef = (pure <$> parseLit "definition") <||> (pure <$> parseLit "def")

parseLitAxiom :: Parser [Text]
parseLitAxiom =
  (pure <$> parseLit "axiom")      <||>
  (pure <$> parseLit "conjecture") <||>
  (pure <$> parseLit "hypothesis") <||>
  (pure <$> parseLit "equation")   <||>
  (pure <$> parseLit "formula")

parseLitProperty :: Parser [Text]
parseLitProperty =
  (pure <$> parseLit "property") <||>
  (pure <$> parseLit "properties")

parseLitWithProperties :: Parser [Text]
parseLitWithProperties =
  parseLit "with" *> parseLitProperty

parseLitTheorem :: Parser [Text]
parseLitTheorem =
  (pure <$> parseLit "proposition")  <||>
  (pure <$> parseLit "theorem")      <||>
  (pure <$> parseLit "lemma")        <||>
  (pure <$> parseLit "corollary")

parseLitLocation :: Parser [Text]
parseLitLocation =
       parseLitDocument
  <||> parseLitTheorem
  <||> parseLitAxiom
  <||> parseLitDef

parseLitSort :: Parser [Text]
parseLitSort = (pure <$> parseLit "type") <||> (pure <$> parseLit "prop")

parseLitClassifier :: Parser [Text]
parseLitClassifier = (pure <$> parseLit "classifiers") <||> (pure <$> parseLit "classifier")

parseLitVarMod :: Parser [Text]
parseLitVarMod =
  (pure <$> parseLit "fixed")    <||>
  (pure <$> parseLit "implicit") <||>
  (pure <$> parseLit "resolved") <||>
  (pure <$> parseLit "remove")

parseLitParam :: Parser [Text]
parseLitParam = (pure <$> parseLit "with") <+> (pure <$> parseLit "parameters")

parseDataHelper :: a -> Text -> (Parser a)
parseDataHelper l arg = ((do
  x <- not_whitespace
  (if str_eq' x arg then return l else fail "parseDataHelper failed to match")) <||> fail "parseDataHelper failed to match'") <* sc

parseDataHelper0 :: a -> Text -> (Parser a)
parseDataHelper0 l arg = (str' arg >> return l) <* sc

newtype Word = Word Text
  deriving (Show, Eq)

tokenToText :: Word -> Text
tokenToText tk = case tk of
  Word txt -> txt

data EOF = EOF
  deriving (Show, Eq)

newtype Number = Number Text
  deriving (Show, Eq)

data Sign =
    Pos
  | Neg
  deriving (Show, Eq)

parseSign :: Parser Sign
parseSign =
  parseLit "+" *> return Pos <||>
  parseLit "-" *> return Neg

data NumInt = NumInt (Maybe Sign) Number
  deriving (Show, Eq)

data Decimal = Decimal {dec_left :: Text, dec_right :: Text}
  deriving (Show, Eq)

data Numeric = Numeric {num_left :: Text, num_right :: Text}
  deriving (Show, Eq)

newtype Symbol = Symbol Text
  deriving (Show, Eq)

newtype SymbolQED = SymbolQED Text
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
data ApplySub = ApplySub
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

textOfAtomicId :: AtomicId -> Text
textOfAtomicId (AtomicId txt) = txt

data VarOrNumber =
    OfNumber Number
  | OfVar Var
  deriving (Show, Eq)

parseVarOrNumber :: Parser VarOrNumber
parseVarOrNumber =
  OfNumber <$> parseNumber <||>
  OfVar <$> parseVar

data HierId = HierId [AtomicId] (Maybe VarOrNumber)
  deriving (Show, Eq)

data FieldAcc =
    FieldAccHierId HierId
  | FieldAccAtomicId AtomicId
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

parseNumInt :: Parser NumInt
parseNumInt = NumInt <$> (option parseSign) <*> parseNumber

readNumInt :: NumInt -> Parser Int
readNumInt (NumInt msgn num@(Number txt)) = case msgn of
  Nothing -> readNumInt (NumInt (Just Pos) num)
  (Just Neg) -> (*) (-1) <$> readNumInt (NumInt (Just Pos) num)
  (Just Pos) -> return $ read (unpack txt)

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

parseSymbolQED :: Parser SymbolQED
parseSymbolQED = ((symbol' "qed" )
               <||> (symbol  "◽"   )
               <||> (symbol  "◻"   )
               <||> (symbol  "◾"   )
               <||> (symbol  "◼"   )
                  ) >>= return . SymbolQED

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

parseApplySub :: Parser ApplySub
parseApplySub = parseDataHelper0 ApplySub "_"

parseAlt :: Parser Alt
parseAlt = parseDataHelper0 Alt "|"

parseSlash :: Parser Slash
parseSlash = parseDataHelper0 Slash "/"

parseSlashDash :: Parser SlashDash
parseSlashDash = parseDataHelper0 SlashDash "/-"


var_old :: Parser Text
var_old = do a <- alpha
             ts    <- (many $ digit <||> ch '_' <||> ch '\'') >>= return . join
             return $ a <> ts

var_new :: Parser Text
var_new = alpha <+> ch '_' <+> ch '_' <+> alphanum

parseVarOrWordOrAtomicIdChar :: Parser Text
parseVarOrWordOrAtomicIdChar = (alpha <||> digit <||> ch '_' <||> ch '\'')

-- only meant to be run inside of parseVarOrWordOrAtomic
-- parses either a var or a varlong
parseVar_aux :: Parser Var
parseVar_aux =
  (Var <$> (var_new <||> var_old)) <* notFollowedBy parseVarOrWordOrAtomicIdChar

isVar :: Text -> Bool
isVar tk = do
  case runParser (toParsec parseVar_aux) "" tk of
    Left _ -> False
    Right v -> True

parseWord_aux2 :: Parser Word
parseWord_aux2 = (do
  a <- alpha
  as <- many1 alpha
  notFollowedBy (char '.' >> (alpha <||> digit))
  return $ Word . join $ a:as) <* sc

-- only meant to be run inside of parseVarOrWordOrAtomic
-- parses either a var or a varlong
parseWord_aux :: Parser Word
parseWord_aux =
  parseWord_aux2 <* notFollowedBy parseVarOrWordOrAtomicIdChar

isWord :: Text -> Bool
isWord tk = do
  case runParser (toParsec parseWord_aux) "" tk of
    Left _ -> False
    Right v -> True  


atomicid :: Parser Text
atomicid = (do
  alph <- alpha
  rest <- (many' $ alpha <||> digit <||> (ch '_' <* lookAhead' (alpha <||> digit))) >>= return . join
  -- guard $ (any C.isAlpha (unpack rest)) || (any (\x -> x == (pack . pure $ '_')) rest) -- TODO fix this
  return $ alph <> rest) <* sc


-- only meant to be run inside of parseVarOrWordOrAtomic
-- parses either a var or a varlong
parseAtomicId_aux :: Parser AtomicId
parseAtomicId_aux =
  (AtomicId <$> atomicid) <* notFollowedBy parseVarOrWordOrAtomicIdChar

isAtomicId :: Text -> Bool
isAtomicId tk = do
  case runParser (toParsec parseAtomicId_aux) "" tk of
    Left _ -> False
    Right v -> True    
  
data VarOrWordOrAtomicId =
    VarOrWordOrAtomicIdVar Var
  | VarOrWordOrAtomicIdWord Word
  | VarOrWordOrAtomicIdAtomicId AtomicId
  deriving (Show, Eq)

-- corresponds to `identkey` in lexer file, but also parses varlong as part of var
parseVarOrWordOrAtomicId :: Parser VarOrWordOrAtomicId
parseVarOrWordOrAtomicId = do
  tk <- (join <$> (many $ alpha <||> digit <||> ch '_' <||> ch '\'')) <* sc
  case isVar tk of
    True -> return $ VarOrWordOrAtomicIdVar $ Var tk
    False ->
      case isWord tk of
        True -> return $ VarOrWordOrAtomicIdWord $ Word tk
        False ->
          case isAtomicId tk of
            True -> return $ VarOrWordOrAtomicIdAtomicId $ AtomicId tk
            False -> fail "failed to parse var, word, or atomic"

-- test parseVarOrWordOrAtomicId "f1a" >> test parseVarOrWordOrAtomicId "f1" >> test parseVarOrWordOrAtomicId "foobar" >> test parseVarOrWordOrAtomicId "foobar1" >> test parseVarOrWordOrAtomicId "f__bar"

-- test parseWord "foobar"
-- works as expected

-- note(jesse, October 07 2019, 01:51 PM): disabled this implementation
-- because this causes section label parsing (which expects an atomicid,
-- but which under this scheme is usually parsed as a word) to nearly always fail

-- parseVar :: Parser Var
-- parseVar = do
--   result <- parseVarOrWordOrAtomicId
--   case result of
--     VarOrWordOrAtomicIdVar v -> return v
--     _ -> fail "failed to parse var"

-- parseWord :: Parser Word    
-- parseWord = do
--   result <- parseVarOrWordOrAtomicId
--   case result of
--     VarOrWordOrAtomicIdWord w -> return w
--     _ -> fail "failed to parse word"

-- parseAtomicId :: Parser AtomicId
-- parseAtomicId = do
--   result <- parseVarOrWordOrAtomicId
--   case result of
--     VarOrWordOrAtomicIdAtomicId a -> return a
--     _ -> fail "failed to parse atomic"

parseVar :: Parser Var
parseVar = (do
  tk <- join <$> (many $ parseVarOrWordOrAtomicIdChar)
  case isVar tk of
    True -> return $ Var tk
    False -> fail "failed to parse var") <* sc

parseWord :: Parser Word    
parseWord = (do
  tk <- join <$> (many $ parseVarOrWordOrAtomicIdChar)
  case isWord tk of
    True -> return $ Word tk
    False -> fail "failed to parse word") <* sc

parseAtomicId :: Parser AtomicId
parseAtomicId = (do
  tk <- join <$> (many $ parseVarOrWordOrAtomicIdChar)
  case isAtomicId tk of
    True -> return $ AtomicId tk
    False -> fail "failed to parse atomic") <* sc


-- test parseVar "C__mathcal"

-- TODO later, make sure to implement check against being substring of an identifier
-- A token is a string of alpha characters which is not followed by a period and another alpha character

-- parseWord1 :: Parser Word
-- parseWord1 = do
--   Word <$> intercalate (pack " ") <$>
--     (many1' (do a <- alpha
--                 as <- many1 alpha 
--                 notFollowedBy (char '.' >> (alpha <||> digit))
--                 return $ join $ a :as) <* sc) -- TODO(jesse): validate placement of sc


parseWordOfLit :: Text -> Parser Word -- TODO: insert guard to ensure that `arg` is Word-compliant
parseWordOfLit arg = parseLit arg >>= return . Word

parseWordOfWord :: Word -> Parser Word
parseWordOfWord tk = case tk of
  Word txt -> parseWordOfLit txt

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

-- test parseAtomicId "foo_" -- does not parse the underscore
-- test parseAtomicId "foo123_ab" -- parses the underscore

-- hierid0 ensures that a hierid contains at least two atomicids separated by a period
hierid0 :: Parser [AtomicId]
hierid0 = (do
  aid0 <- AtomicId <$> atomicid <* (ch '.')
  (:) aid0 <$> (sepby1 (atomicid) (ch '.') >>= return . (map AtomicId))) <* sc

hierid1 :: Parser (Maybe VarOrNumber)
hierid1 = option $ OfNumber <$> parseNumber <||> OfVar <$> parseVar

parseHierId :: Parser HierId
parseHierId = HierId <$> hierid0 <*> hierid1

parseFieldAcc :: Parser FieldAcc
parseFieldAcc = ch ('.') *>
  (FieldAccHierId <$> parseHierId <||>
   FieldAccAtomicId <$> parseAtomicId)
  -- ch ('.') *> (FieldAccHierId <$> parseHierId <||>
  -- FieldAccNumber <$> parseNumber <||>
  -- FieldAccVar <$> parseVar )
  --     ((do (ch '.') *> (parseAtomicId >>= return . FieldAcc . OfAtomicId))
  -- <||> (do (ch '.') *> (parseNumber >>= return . FieldAcc . OfNumber))
  -- <||> (do (ch '.') *> (parseVar >>= return . FieldAcc . OfVar))) <* sc

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
parseControlSequence = ControlSequence <$> controlsequence <* sc

paren :: Parser a -> Parser a
paren = between parseLParen parseRParen

bracket :: Parser a -> Parser a
bracket = between parseLBrack parseRBrack

brace :: Parser a -> Parser a
brace = between parseLBrace parseRBrace

opt_paren :: Parser a -> Parser a
opt_paren p = (paren p) <||> p

brace_semi :: Eq a => Parser a -> Parser [a]
brace_semi p = between parseLBrace parseRBrace (sepby1 p parseSemicolon)

sep_list :: Parser a -> Parser [a]
sep_list p = sepby p ((parseLit "," *> parseLit "and") <||> parseLit "and" <||> parseLit ",")

sep_list1 :: Eq a => Parser a -> Parser [a]
sep_list1 p = sepby1 p ((parseLit "," *> parseLit "and") <||> parseLit "and" <||> parseLit ",")

comma_nonempty_list :: Eq a => Parser a -> Parser [a]
comma_nonempty_list p = sepby1 p (parseComma)

newtype Label = Label AtomicId
  deriving (Show, Eq)

textOfLabel :: Label -> Text
textOfLabel (Label aid) = textOfAtomicId aid

parseLabel :: Parser Label
parseLabel = Label <$> parseAtomicId

parseBool :: Parser Bool
parseBool = parseLitTrue *> return True <||> parseLitFalse *> return False

tokenToText'_aux :: [[Text]] -> Word -> [Text]
tokenToText'_aux strsyms tk = case tk of
  Word txt -> case (filter (elem txt) strsyms) of
    [] -> [txt]
    (x:xs) -> ((<>) x $ concat xs)

-- tokenToText' extracts the underlying Text of a token and adds all available synonyms from the state.
tokenToText' :: Word -> Parser [Text]
tokenToText' tk = tokenToText'_aux <$> (concat <$> (use (allStates strSyms))) <*> return tk
