{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification,OverloadedStrings #-}

module Strang.Base(splitCommand,printCommand,printTyped,joinCommand,leftMap,makeReplaceRegexCommand,base) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.ICU hiding (ParseError,span)
import qualified Data.Text.ICU as I (span,find)
import Control.Monad.Writer.Strict hiding (sequence)
import Strang.Command
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)
import Data.Maybe

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right

-- Split command implementation.
splitCommand :: Text -> Command Text [Text]
splitCommand t = command ("Split " ++ show t) $ T.splitOn t

-- Print command implementation.
printCommand :: ParamTy a -> Command a Text
printCommand it = Command { runCommand = \st -> let p = printTyped it st in writer (p, [p])
                            , inTy = it
                            , outTy = StringTy
                            , commandName = "Print" }

-- Actual print implementation.
printTyped :: ParamTy a -> a -> Text
printTyped StringTy str = str
printTyped (ListTy t) ts = T.pack "[" `T.append` T.intercalate (T.pack ",") (fmap (printTyped t) ts) `T.append` T.pack "]"

-- Join command implementation.
joinCommand :: Text -> Command [Text] Text
joinCommand = command "Join" . T.intercalate

replaceRegex :: Regex -> Text -> Text -> Text
replaceRegex regexFind replace input = case I.find regexFind input of
                                    Nothing -> input -- short circuit
                                    Just x -> I.span x <> replace <> fromMaybe T.empty (replaceRegex regexFind replace <$> suffix 0 x)

regexCommand :: Regex -> Text -> Command Text Text
regexCommand regexFind regexReplace = command "ReplaceRegex" $ replaceRegex regexFind regexReplace

apply :: Functor f => a -> f (a -> b) -> f b
apply a = fmap (\f -> f a)

-- Regex replacement command.
makeReplaceRegexCommand :: Text -> Text -> Either ParseError (Command Text Text)
makeReplaceRegexCommand regexFind replace = let withStringErr = apply replace (regexCommand <$> regex' [] regexFind) in
                           leftMap (flip newErrorMessage (initialPos "") . Message . show) withStringErr

-- Really more of a [Char] than a String, but I am HLint's humble servant
containsChars :: String -> Text -> Bool
containsChars cs = isJust . T.findIndex (`elem` cs)

escapeCsvField :: Text -> Text
escapeCsvField t =
  let quotesEscaped = T.replace "\"" "\"\"" t
      needsQuotes = containsChars ",\r\n\"" t
      quoted = if needsQuotes
             then '\"' `T.cons` quotesEscaped `T.snoc` '\"'
             else quotesEscaped in
                quoted

csv :: Binding
csv = Binding { bindingName = "csv"
              , commandFromBinding = AnyCommand $ command "CSV Output" (\records -> T.intercalate "\n" (T.intercalate "," <$> (fmap . fmap) escapeCsvField records)) }

base :: [Binding]
base = [csv]
