{-
Author(s): Jesse Michael Han (2019)

Basic parser combinators.
-}

{-# LANGUAGE OverloadedStrings #-}
module CNLean.Basic where

import Prelude hiding (Int, Bool, String, drop)
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

join :: [Text] -> Text
join ts = foldl (<>) "" ts

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "---")
  (L.skipBlockComment "/--" "--/")

symbol :: Text -> Parser Text
symbol arg = L.symbol sc arg

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

symbol' :: Text -> Parser Text
symbol' arg = L.symbol' sc arg

not_space_aux :: Parser Char
not_space_aux = (satisfy (\x -> x /= ' '))

not_space :: Parser Text
not_space = (many1 not_space_aux) >>= return . pack

item :: Parser Char
item = satisfy (\_ -> True)

digit :: Parser Text
digit = digitChar >>= return . pack . pure

number :: Parser Text
number = (many1 digit) >>= return . join

alpha :: Parser Text
alpha = (upperChar <|> lowerChar) >>= return . pack . pure

alphanum :: Parser Text
alphanum = (many1 (alpha <|> digit)) >>= return . join

char :: Char -> Parser Text
char c = Text.Megaparsec.Char.char c >>= return . pack . pure

string :: Text -> Parser Text
string t = do
  Text.Megaparsec.Char.string t
