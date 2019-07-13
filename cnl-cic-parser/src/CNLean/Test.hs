module CNLean.Test (
helloWorld
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

-- :set -XOverloadedStrings

helloWorld :: IO ()
helloWorld = putStrLn "Hello World"
