{-# LANGUAGE OverloadedStrings #-}
{-
Author(s): Jesse Michael Han (2019)
-}

module Main where

import Colada.Basic.Basic
import Colada.ProgramText
import qualified Data.Text.IO as TIO
import Data.Text (Text, intercalate, pack, unpack)
import System.Environment
import System.Directory
import Control.Monad.Trans.State
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec
import System.FilePath
import Data.List.NonEmpty (fromList)

initialState :: Text -> PosState Text
initialState txt = PosState
              { pstateInput = txt
              , pstateOffset = 0
              , pstateSourcePos = initialPos ""
              , pstateTabWidth = defaultTabWidth
              , pstateLinePrefix = ""
              }

errorBundleOfParseError :: Text -> (ParseError Text e) -> ParseErrorBundle Text e
errorBundleOfParseError txt pe =
  ParseErrorBundle { bundleErrors = fromList [pe],
                     bundlePosState = initialState txt }

main :: IO ()
main = do
  file <- (head <$> getArgs)
  txt <- makeAbsolute file >>= TIO.readFile 
  case (runParser (toParsec parseRawTextItems) "" txt) of
    Left err -> fail $ errorBundlePretty err
    Right a -> do
      let output = intercalate "\n\n" (pack . (showHandler txt) <$> a)
      TIO.putStrLn output
      TIO.writeFile (replaceExtension file "parsed") output

  where showHandler :: Text -> RawResult Text SimpleError TextItem -> String
        showHandler txt raw = case raw of
          Left (err) -> "ERROR: " <> (errorBundlePretty $ errorBundleOfParseError txt err)
          Right x    -> show x
