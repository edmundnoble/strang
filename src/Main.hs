module Main (main) where

import Strang.Interpreter
import qualified Data.Text as T

main :: IO ()
main = do
   program <- T.pack <$> getLine
   interpretProgram program
