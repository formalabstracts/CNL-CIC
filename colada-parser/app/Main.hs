{-# LANGUAGE OverloadedStrings #-}
{-
Author(s): Jesse Michael Han (2019)
-}

module Main where

import CNLean.Basic.Basic
import CNLean.Core

main :: IO ()
main = test parseProgram "[synonym foo/foos/bar/-s/baz/-s]"
