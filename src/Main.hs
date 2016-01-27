module Main (main) where

import Strang.Interpreter
import qualified Data.ByteString as BS

main :: IO ()
main = do
   program <- BS.getLine
   interpretProgram program
