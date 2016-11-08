{-# LANGUAGE Trustworthy               #-}

module Strang.Parsers (programParser) where

import           Control.Applicative.Alternative hiding (many)
import           Data.Functor
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Strang.Base
import           Strang.Command
import           Strang.Input
import           Text.Parsec                     hiding ((<|>))
import           Text.Parsec.Text

infixr 6 `as`

as :: Functor f => f a -> b -> f b
fa `as` b = fmap (const b) fa

-- A Strang command is a series of characters.
-- It starts with a mode character, 'l' for line mode or 't' for text mode.
modeParser :: Parser InputMode
modeParser = consumingParser <|> pure singleLine where
                consumingParser = try (char 'l' `as` singleLine) <|> try (char 't' `as` multiLine)

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

-- Existentials being tricky. Parser alternation operator, converting the second operand
-- to an AnyCommand.
(<||>) :: Parser AnyCommand -> Parser (Command a b) -> Parser AnyCommand
ab <||> cd = ab <|> fmap AnyCommand cd

parserForBinding :: Binding -> Parser AnyCommand
parserForBinding Binding { bindingName = n, commandFromBinding = c} = string n $> c

parserForBindings :: [Binding] -> Parser AnyCommand
parserForBindings bs = foldl (<|>) parserZero (fmap parserForBinding bs)

commandParser :: [Binding] -> Parser AnyCommand
commandParser bs = parserForBindings bs <||> splitParser <||> joinParser <||> replaceRegexParser

equationParser :: [Binding] -> Parser Binding
equationParser bs = do
  name <- try (many1 alphaNum <* many1 space <* char '=' <* many1 space)
  command <- commandParser bs
  return $ Binding name command

compoundCommandParser :: [Binding] -> Parser [AnyCommand]
compoundCommandParser bs = many1 (commandParser bs)

recState :: ([s] -> Parser s) -> [s] -> Parser i -> Parser [s]
recState f s sep = let ourParser = try (f s) in do
  newState <- ourParser
  nextResult <- optionMaybe (try (sep *> recState f (newState:s) sep))
  return $ newState : fromMaybe [] nextResult

allBindingsParser :: Parser [Binding]
allBindingsParser = recState equationParser base (many space)

programParser :: Parser (InputMode, [AnyCommand])
programParser = do
  mode <- modeParser
  bindings <- option base (try allBindingsParser)
  many space
  commands <- compoundCommandParser bindings
  many space
  eof
  return (mode, commands)
