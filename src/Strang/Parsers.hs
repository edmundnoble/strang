{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification #-}

module Strang.Parsers (programParser) where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text
import Text.ParserCombinators.Parsec.Char (char, anyChar)
import Text.Parsec.Combinator
import Data.Functor
import Control.Monad.Writer.Strict hiding (sequence)
import Control.Applicative.Alternative
import Strang.Types
import Strang.Commands

-- A Strang command is a series of characters.
-- It starts with a mode character, 'l' for line mode or 't' for text mode.
modeParser :: Parser InputMode
modeParser = consumingParser <|> pure (pure . T.pack <$> getLine) where
                consumingParser = try (char 'l' $> (pure . T.pack <$> getLine)) <|> try (char 't' $> (pure . T.pack <$> getContents))

-- This is for passing strings to commands.
stringArg :: Parser Text
stringArg = T.pack <$> surroundedBy (char '/') anyChar where
                surroundedBy q p = q *> manyTill p (try q)

twoStringArgs :: Parser (Text, Text)
twoStringArgs = char '/' *> let p = T.pack <$> manyTill anyChar (try $ char '/') in p >*< p

-- Split command parser. Syntax is:
-- s<char>
splitParser :: Parser (Command Text [Text])
splitParser = splitCommand <$> (char 's' *> stringArg)

-- Join command parser.
-- Syntax:    <other commands>j"delim"
-- Joins the elements of a list of strings, with a delimiter.
joinParser :: Parser (Command [Text] Text)
joinParser = joinCommand <$> (char 'j' *> stringArg)

-- Direction switch command parser.
-- Not implemented, this will require grammar changes but will not be hard.
directionSwitchParser :: Parser ()
directionSwitchParser = void $ char '<'

-- Index command parser. Indexes into the farthest outer liststate.
-- Not implemented.
indexParser :: Parser a
indexParser = let x = x in x

(>*<) :: Applicative m => m a -> m b -> m (a, b)
(>*<) = liftA2 (,)

-- Replacement regex command parser! Syntax is:
--    r/<regexfind>/<regexreplace>
replaceRegexParser :: Parser (Command Text Text)
replaceRegexParser = collapseError $ uncurry makeReplaceRegexCommand <$> (char 'r' *> twoStringArgs)

-- Collapses an error from an Either into the outer Parser.
collapseError :: Show e => Parser (Either e a) -> Parser a
collapseError p = p >>= either (fail . show) pure

{-
-- Capturing regex command parser. Syntax is:
-- c"<regexstuff>"
captureRegexParser :: Parser (Command Text [Text])
captureRegexParser = collapseError $ makeRegexCommand True <$> (char 'c' *> stringArg)
-}

-- Existentials being tricky. Parser alternation operator, converting the second operand
-- to an AnyCommand.
(<||>) :: Parser AnyCommand -> Parser (Command a b) -> Parser AnyCommand
ab <||> cd = ab <|> fmap Exists cd

commandParser :: Parser AnyCommand
commandParser = parserZero <||> splitParser <||> joinParser <||> replaceRegexParser -- <||> captureRegexParser
-- what do I do with print?

compoundCommandParser :: Parser [AnyCommand]
compoundCommandParser = (join <$> (try (char '(') *> many1 compoundCommandParser <* try (char ')'))) <|> many1 commandParser

programParser :: Parser (InputMode, [AnyCommand])
programParser = do
  mode <- modeParser
  commands <- compoundCommandParser
  eof
  return (mode, commands)
