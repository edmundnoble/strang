{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Monoid
import           Data.Text
import qualified Data.Text.IO       as TIO
import           Strang.Interpreter

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
   runProgram program
