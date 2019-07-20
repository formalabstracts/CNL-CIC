{-
Author(s): Jesse Michael Han (2019)

Parsing instructions.
-}

{-# LANGUAGE OverloadedStrings #-}
module CNLean.Instr where

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

import CNLean.Basic
import CNLean.Token

data KeywordCommand =
  KeywordCommandMk Token
  deriving (Show, Eq)

parseKeywordCommand :: Parser KeywordCommand
parseKeywordCommand = do
  parseLit_aux EXIT >>= return . KeywordCommandMk . Lit

data KeywordString =
  KeywordStringToken Token
  deriving (Show, Eq)

parseKeywordString :: Parser KeywordString
parseKeywordString = do
  (parseLit_aux READ <||> parseLit_aux LIBRARY) >>= return . KeywordStringToken . Lit

data KeywordBool =
  KeywordBoolToken Token
  deriving (Show, Eq)

parseKeywordBool :: Parser KeywordBool
parseKeywordBool = do
  (parseLit_aux PRINTGOAL <||> parseLit_aux DUMP <||> parseLit_aux ONTORED) >>= return . KeywordBoolToken . Lit

data KeywordInt =
  KeywordIntToken Token
  deriving (Show, Eq)

parseKeywordInt :: Parser KeywordInt
parseKeywordInt = do
  parseLit_aux TIMELIMIT >>= return . KeywordIntToken . Lit

data KeywordSynonym =
  KeywordSynonymToken CNLean.Token.Token
  deriving (Show, Eq)

parseKeywordSynonym :: Parser KeywordSynonym
parseKeywordSynonym = do
  parseLit_aux SYNONYM >>= return . KeywordSynonymToken . Lit

data InstrSep =
  InstrSepToken CNLean.Token.Token
  deriving (Show, Eq)

parseInstrSep :: Parser InstrSep
parseInstrSep = do
  (parseSlashDash <||> parseSlash) >>= return . InstrSepToken

data Instr =
    InstructCommand KeywordCommand
  | InstructSynonym KeywordSynonym [[Token]]
  | InstructString KeywordString [[Token]]
  | InstructBool KeywordBool Bool
  | InstructInt KeywordInt Token
  deriving (Show, Eq)

parseInstructCommand :: Parser Instr
parseInstructCommand = do
  parseKeywordCommand >>= return . InstructCommand

parseInstructSynonym :: Parser Instr
parseInstructSynonym = do
  s <- parseKeywordSynonym
  tks <- (sepby1 (many1' parseTk) (parseInstrSep))
  return $ InstructSynonym s tks

parseInstructString :: Parser Instr
parseInstructString = do
  s <- parseKeywordString
  tks <- (sepby1 (many1' parseTk) (parseInstrSep))
  return $ InstructString s tks

parseInstructBool :: Parser Instr
parseInstructBool = do
  k <- parseKeywordBool
  t <- (parseLit_aux TRUE <||> parseLit_aux ON <||> parseLit_aux YES >> return True)
        <||> (parseLit_aux FALSE <||> parseLit_aux OFF <||> parseLit_aux NO >> return False)
  return $ InstructBool k t

parseInstructInt :: Parser Instr
parseInstructInt = do
  k <- parseKeywordInt
  n <- parseNumber
  return $ InstructInt k n

parseInstr :: Parser Instr
parseInstr = between parseLBrack parseRBrack $
        parseInstructCommand
  <||>  parseInstructSynonym
  <||>  parseInstructString
  <||>  parseInstructBool
  <||>  parseInstructInt

-- -- example1 :: Instr
-- -- example1 = Synonym LitSynonym ["HELLO", "TOM"]

-- -- example2 :: Instr
-- -- example2 = String LitRead ["FOO","BAR"]

-- -- example3 :: Instr
-- -- example3 = Bool PrintGoal True

-- -- myToken :: Parser Text
-- -- myToken = not_space <* sc

-- -- myToken' :: Parser Text
-- -- myToken' = (many $ notFollowedBy (parseSlashDash <|> parseSlash <|> (symbol "]")) >> item) >>= return . pack

-- -- parseKeywordSynonym :: Parser KeywordSynonym
-- -- parseKeywordSynonym = do
-- --   symbol "synonym"  
-- --   return LitSynonym

-- -- parseBrackets :: Parser a -> Parser a
-- -- parseBrackets p = between (symbol "[") (symbol "]") p

-- -- parseSlash :: Parser (Tokens Text)
-- -- parseSlash = (symbol "/")  

-- -- parseSlashDash :: Parser (Tokens Text)
-- -- parseSlashDash = do (symbol "/-")

-- -- parseSynonym :: Parser Instr
-- -- parseSynonym =
-- --   parseBrackets $ do
-- --   k   <- parseKeywordSynonym
-- --   tks <- sepBy1 (myToken') $ parseSlashDash <|> parseSlash
-- --   return $ Synonym k tks

-- -- tksTest :: Parser [Text]
-- -- tksTest = sepBy1 myToken $ (parseSlashDash <|> parseSlash)

-- -- -- parseTest (tksTest <* eof) "a / b /- c / d"

-- -- parseBracketsTest :: Parser Text
-- -- parseBracketsTest =
-- --   parseBrackets $ (many $ satisfy (\_ -> True)) >>= return . pack

-- -- parseInstr :: Parser Instr
-- -- parseInstr = (many1' item) >> return example3

-- helloWorld :: IO ()
-- helloWorld = do
--   parseTest parseInstr "[synonym foo/bar]"
  -- parseTest (parseSynonym <* eof) "[ synonym number /- s ]"
  -- parseTest (parseSynonym <* eof) "[ synonym element / elements /elemental ]"
  -- parseTest (parseSynonym <* eof) "[synonym almost everywhere / ae ]"
  -- parseTest (parseSynonym <* eof) "[ synonym almost all/all but finitely many]"
