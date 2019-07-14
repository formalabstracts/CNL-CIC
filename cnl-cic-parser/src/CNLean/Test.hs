{-# LANGUAGE OverloadedStrings #-}
module CNLean.Test (
helloWorld
) where

import Prelude hiding (Int, Bool, String, drop)
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data KeywordCommand =
  LitExit
  deriving (Show, Eq)

data KeywordString =
  LitRead | LitLibrary
  deriving (Show, Eq)

data KeywordBool =
  PrintGoal | Dump | Ontored
  deriving (Show, Eq)

data KeywordInt =
  LitTimeLimit
  deriving (Show, Eq)

data KeywordSynonym =
  LitSynonym
  deriving (Show, Eq)

data InstrSep =
  Slash | SlashDash
  deriving (Show, Eq)

data Instr =
    Command KeywordCommand
  | Synonym KeywordSynonym [Tokens Text]
  | String KeywordString [Tokens Text]
  | Bool KeywordBool Prelude.Bool
  | Int KeywordInt Prelude.Int
  deriving (Show, Eq)

example1 :: Instr
example1 = Synonym LitSynonym ["HELLO", "TOM"]

example2 :: Instr
example2 = String LitRead ["FOO","BAR"]

example3 :: Instr
example3 = Bool PrintGoal True

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "---")
  (L.skipBlockComment "/--" "--/")

symbol :: Text -> Parser Text
symbol arg = L.symbol sc arg

not_space_aux :: Parser Char
not_space_aux = (satisfy (\x -> x /= ' '))

not_space :: Parser Text
not_space = (many not_space_aux) >>= return . pack

item :: Parser Char
item = satisfy (\_ -> True)

myToken :: Parser Text
myToken = not_space <* sc

myToken' :: Parser Text
myToken' = (many $ notFollowedBy (parseSlashDash <|> parseSlash <|> (symbol "]")) >> item) >>= return . pack

parseKeywordSynonym :: Parser KeywordSynonym
parseKeywordSynonym = do
  symbol "synonym"  
  return LitSynonym

parseBrackets :: Parser a -> Parser a
parseBrackets p = between (symbol "[") (symbol "]") p

parseSlash :: Parser (Tokens Text)
parseSlash = (symbol "/")
  

parseSlashDash :: Parser (Tokens Text)
parseSlashDash = do (symbol "/-")

parseSynonym :: Parser Instr
parseSynonym =
  parseBrackets $ do
  k   <- parseKeywordSynonym
  tks <- sepBy1 (myToken') $ parseSlashDash <|> parseSlash
  return $ Synonym k tks

tksTest :: Parser [Text]
tksTest = sepBy1 (myToken) $ (parseSlashDash <|> parseSlash)

-- parseTest (tksTest <* eof) "a / b /- c / d"

parseBracketsTest :: Parser Text
parseBracketsTest =
  parseBrackets $ (many $ satisfy (\_ -> True)) >>= return . pack

parseInstr :: Parser Instr
parseInstr = do
  return example3

helloWorld :: IO ()
helloWorld = do
  parseTest (parseSynonym <* eof) "[ synonym number /- s ]"
  parseTest (parseSynonym <* eof) "[ synonym element / elements /elemental ]"
  parseTest (parseSynonym <* eof) "[synonym almost everywhere / ae ]"
  parseTest (parseSynonym <* eof) "[ synonym almost all/all but finitely many]"
