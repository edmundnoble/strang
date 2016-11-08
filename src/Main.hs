{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Strang.Interpreter
import Data.Text
import qualified Data.Text.IO as TIO
import Data.Monoid
import Data.Default

input :: IO Text
input = do
  nextLine <- TIO.getLine
  if nextLine == "nn"
    then return ""
    else do nextInput <- input
            return (nextLine <> "\n" <> nextInput)

main :: IO ()
main = do
   program <- input
   interpretProgram program def
