{-
Author(s): Jesse Michael Han (2019)

Core combinators and utility functions.
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module CNLean.Basic.Core where

import Prelude
import Control.Monad.Trans.State.Lazy
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack, toLower)
import Data.Void
import Control.Monad (guard)
import qualified Data.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import CNLean.Basic.State

type Parser0 = Parsec Void Text

type ParserSt s = StateT s (Parser0)

type Parser = ParserSt (Stack FState)

runtest0 :: Parser a -> Parser0 (a, Stack FState)
runtest0 p = (runStateT p) initialFStack

test_all :: Show a => Parser a -> Text -> IO ()
test_all p arg = parseTest (runtest0 p) arg

toParsec :: Parser a -> Parsec Void Text a
toParsec p = do (a,b) <- runtest0 p
                return a

---- `test p arg` runs `p` on `arg`, suppressing information about the FState
test :: Show a => Parser a -> Text -> IO ()
test p arg = parseTest (toParsec p) arg

repeatN :: Int -> Parser a -> Parser a
repeatN n p = foldr (>>) p $ replicate (n-1) p

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = (try p) <|> q

(<+>) :: Semigroup a => Parser a -> Parser a -> Parser a
p <+> q = (<>) <$> p <*> q

rp :: Parser a -> Parser [a]
rp p = p >>= return . pure

join :: [Text] -> Text
join ts = foldl (<>) "" ts

sc :: Parser ()
sc = L.space
  space1 -- note, space1 uses spaceChar so consumes newlines, tabs, etc also
  (L.skipLineComment "%")
  (L.skipBlockComment "%%%" "%%%")

symbol :: Text -> Parser Text
symbol arg = L.symbol sc arg

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

symbol' :: Text -> Parser Text
symbol' arg = L.symbol' sc arg

not_ch :: Char -> Parser Text
not_ch c = (satisfy $ \x -> x /= c) >>= return . pack . pure

not_space :: Parser Text
not_space = (many1 $ not_ch ' ') >>= return . join

succeeds :: Parser a -> Parser Bool
succeeds p = (p *> return True) <||> return False

fails :: Parser a -> Parser Bool
fails p = (p *> return False) <||> return True

fail_iff_succeeds :: Parser a -> Parser ()
fail_iff_succeeds p = do
  b <- fails p
  if b then return () else empty

item :: Parser Text
item = satisfy (\_ -> True) >>= return . pack . pure

not_whitespace_aux :: Parser Text
not_whitespace_aux = do
  b <- succeeds $ lookAhead spaceChar
  if b then fail "whitespace character ahead, failing"
       else item

fold :: [Parser a] -> Parser a
fold ps = foldr (<||>) empty ps

not_whitespace :: Parser Text
not_whitespace = (many1 not_whitespace_aux) >>= return . join

word :: Parser Text
word = not_whitespace <* sc

digit :: Parser Text
digit = digitChar >>= return . pack . pure

number :: Parser Text
number = (many1 digit) >>= return . join

alpha :: Parser Text
alpha = (upperChar <||> lowerChar) >>= return . pack . pure

alphanum :: Parser Text
alphanum = (many1 (alpha <||> digit)) >>= return . join

ch :: Char -> Parser Text
ch c = Text.Megaparsec.Char.char c >>= return . pack . pure

str :: Text -> Parser Text
str t = do
  Text.Megaparsec.Char.string t

str' :: Text -> Parser Text
str' t = do
  Text.Megaparsec.Char.string' t

str_eq' :: Text -> Text -> Prelude.Bool
str_eq' t1 t2 = (toLower t1) == (toLower t2)

---- backtracking version of many, used to implement backtracking versions of sepby and sepby1
many' :: Parser a -> Parser [a]
many' p = (do
  a <- p
  as <- many' p
  return (a:as) ) <||> return []

---- backtracking version of many1. fails unless if p succeeds at least once
many1' :: Parser a -> Parser [a]
many1' p = do
  a  <- p
  as <- (many' p)
  return $ a : as

sepby_aux :: Parser a -> Parser b -> Parser [a]
sepby_aux p sep =
  (:) <$> p <*> (many' (sep *> p))  

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = (sepby_aux p sep) <||> return []

guard_result :: String -> Parser a -> (a -> Bool) -> Parser a
guard_result err p pred = do
  x <- p
  (guard (pred x)) <||> fail err
  return x

fail_if_empty :: (Eq a) => Parser [a] -> Parser [a]
fail_if_empty p = guard_result "parsed empty list, failing" p (/= [])
--TODO(jesse) debug error message not displaying in favor of 'guard failed'

sepby1 :: (Eq a) => Parser a -> Parser b -> Parser [a]
sepby1 p sep = fail_if_empty $ sepby p sep

---- lookAhead' strictly looks ahead, returning no values upon success
lookAhead' :: Parser a -> Parser ()
lookAhead' p = (lookAhead p >> return ()) <||> fail "lookahead failed"

option :: Parser a -> Parser (Maybe a)
option p =
  (p >>= return . Just) <||> return Nothing

unoption :: Parser (Maybe a) -> Parser a
unoption p = do
  result <- p
  case result of
    Just x -> return x
    Nothing -> fail "failing on Nothing"

parse_any_aux :: (a -> Parser (Maybe [b])) -> [a] -> Parser (Maybe [b])
parse_any_aux m ph = case ph of
  [] -> return Nothing
  x:xs -> (m x) <+> (parse_any_aux m xs)

parse_any_maybe :: (a -> Parser (Maybe [b])) -> [[a]] -> Parser [b]
parse_any_maybe m phs = case phs of
  [] -> empty
  x:xs -> (unoption $ parse_any_aux m x) <||> (parse_any_maybe m xs)

parse_any :: (a -> Parser [b]) -> [[a]] -> Parser [b]
parse_any m = parse_any_maybe (\x -> m x >>= return . Just)

parse_any_of :: [Parser a] -> Parser a
parse_any_of ps = case ps of
  [] -> empty
  x:xs -> x <||> parse_any_of xs

parse_any_of' :: a -> [a -> Parser b] -> Parser b
parse_any_of' x ps = case ps of
  [] -> empty
  q:qs -> q x <||> parse_any_of' x qs

parse_any_of_with_index_aux :: Int -> [Parser a] -> Parser (a, Int)
parse_any_of_with_index_aux k ps = case ps of
  [] -> empty
  x:xs -> (((,) <$> x <*> (return k)) <||> (parse_any_of_with_index_aux (k+1) xs))

parse_any_of_with_index :: [Parser a] -> Parser (a, Int)
parse_any_of_with_index = parse_any_of_with_index_aux 0

parse_list :: [a] -> (a -> Parser b) -> Parser [b]
parse_list as m = case as of
  [] -> return []
  x:xs -> (rp $ m x) <+> parse_list xs m

with_result :: Parser a -> (a -> Parser b) -> Parser a
with_result p m = do
  r <- p
  m r
  return r

with_any_result :: Parser a -> [a -> Parser b] -> Parser a
with_any_result p ms = do
  r <- p
  parse_any_of' r ms
  return r

with_side_effects :: Parser a -> ([a -> Parser b]) -> Parser a
with_side_effects p ms = do
  case ms of
    [] -> p
    q:qs -> with_result (with_side_effects p qs) q

fail_if_eof :: Parser a -> Parser a
fail_if_eof p = (notFollowedBy eof *> p) <|> fail "end of file detected, failing"

-- from Parsec
chainl p op x       = chainl1 p op <||> return x

chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <||> return x

-- chainl' :: Parser a -> b  -> Parser (b -> a -> b) -> Parser b
-- chainl' p c op x       = chainl1' p c op <||> return x

chainl1' :: Parser a -> b  -> Parser (b -> a -> b) -> Parser b
chainl1' p c op        = do{ x <- p; g <- op; rest (g c x) }
                    where
                      rest x    = ((do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    })
                                <||> return x)

delete_nothings :: [Maybe a] -> [a]
delete_nothings z = case z of
  [] -> []
  x:xs -> case x of
    Just y -> y:(delete_nothings xs)
    Nothing -> delete_nothings xs

isNothing :: Maybe a -> Bool
isNothing m = case m of
  Nothing -> True
  _ -> False
