module Strang.Input(InputMode(..),singleLine,multiLine,interpretInputMode) where

import           Data.Text
import qualified Data.Text.IO as TIO

data InputMode = SingleLine | MultiLine

singleLine :: InputMode
singleLine = SingleLine

multiLine :: InputMode
multiLine = MultiLine

interpretInputMode :: InputMode -> IO Text
interpretInputMode SingleLine = TIO.getLine
interpretInputMode MultiLine = TIO.getContents
