{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification,OverloadedStrings #-}

module Strang.Prelude(splitCommand,joinCommand,leftMap,makeReplaceRegexCommand,base) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.ICU hiding (ParseError,span)
import qualified Data.Text.ICU as I (span,find)
import Control.Monad.Writer.Strict hiding (sequence)
import Strang.Command
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)
import Data.Maybe
import Strang.HListFunctions

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right

-- Split command implementation.
splitCommand :: Text -> Command Text [Text]
splitCommand t = command ("Split " ++ show t) $ T.splitOn t

-- Split command binding. Syntax is:
-- s<char>
splitBinding :: FunctionBinding '[Text]
splitBinding = FunctionBinding "s" (NamedParamTy StringTy "delimiter" :-: KNil) (Right . AnyCommand . hlistEval splitCommand)

-- Join command implementation.
joinCommand :: Text -> Command [Text] Text
joinCommand = command "Join" . T.intercalate

-- Join command parser.
-- Syntax:    <other commands>j"delim"
-- Joins the elements of a list of strings, with a delimiter.
joinBinding :: FunctionBinding '[Text]
joinBinding = FunctionBinding "j" (NamedParamTy StringTy "joiner" :-: KNil) (Right . AnyCommand . hlistEval joinCommand)

-- Replacement regex command parser! Syntax is:
--    r/<regexfind>/<regexreplace>
replaceRegexBinding :: FunctionBinding '[Text, Text]
replaceRegexBinding = FunctionBinding "r" (NamedParamTy StringTy "needle" :-: NamedParamTy StringTy "replacement" :-: KNil) (\args -> AnyCommand <$> hlistEval makeReplaceRegexCommand args)

replaceRegex :: Regex -> Text -> Text -> Text
replaceRegex regexFind replace input = case I.find regexFind input of
                                    Nothing -> input -- short circuit
                                    Just x -> I.span x <> replace <> fromMaybe T.empty (replaceRegex regexFind replace <$> suffix 0 x)

replaceRegexCommand :: Regex -> Text -> Command Text Text
replaceRegexCommand regexFind regexReplace = command "ReplaceRegex" $ replaceRegex regexFind regexReplace

-- Regex replacement command.
makeReplaceRegexCommand :: Text -> Text -> Either ParseError (Command Text Text)
makeReplaceRegexCommand regexFind replace =
  let replaceFunction = replaceRegexCommand <$> regex' [] regexFind
      withStringErr = fmap (\f -> f replace) replaceFunction in
                           leftMap (flip newErrorMessage (initialPos "") . Message . show) withStringErr

-- Really more of a [Char] than a String, but I am HLint's humble servant
containsChars :: String -> Text -> Bool
containsChars cs = isJust . T.findIndex (`elem` cs)

escapeCsvField :: Text -> Text
escapeCsvField t =
  let quotesEscaped = T.replace "\"" "\"\"" t
      needsQuotes = containsChars ",\r\n\"" t
      quoted = if needsQuotes
             then "\"" <> quotesEscaped <> "\""
             else quotesEscaped in
                quoted

csv :: FunctionBinding '[]
csv = FunctionBinding { bindingName = "csv"
              , bindingArguments = KNil
              , commandFromFunctionBinding = \HNil -> Right $ AnyCommand $
                  command "CSV Output"
                    (\records -> T.intercalate "\n" (T.intercalate "," <$> (fmap . fmap) escapeCsvField records)) }

base :: FKList FunctionBinding
base = FK $ splitBinding :-: joinBinding :-: replaceRegexBinding :-: csv :-: KNil
