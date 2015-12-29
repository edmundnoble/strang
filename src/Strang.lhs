> {-# LANGUAGE DataKinds,TypeFamilies,GADTs,RankNTypes,TupleSections #-}

> module Main (
>     main
> ) where

> import Text.Regex.TDFA
> import Text.Regex.TDFA.ByteString
> import qualified Data.ByteString as BS
> import Data.ByteString (ByteString)
> import Text.Parsec.ByteString
> import qualified Text.ParserCombinators.Parsec.Char as AC
> import Text.ParserCombinators.Parsec.Prim (parse)
> import Text.Parsec.Combinator
> import Text.Parsec.Error
> import Text.Parsec.Pos (initialPos)
> import Data.Monoid
> import Data.ByteString.Internal (c2w, w2c)
> import qualified Data.ByteString.Char8 as C
> import Data.Functor
> import Control.Monad.Writer.Strict hiding (sequence)
> import Data.Functor.Identity
> import Control.Applicative.Alternative
> import Data.Either.Combinators
> import System.Environment
> import Safe
> import Data.Traversable hiding (sequence)
> import Control.Monad.Reader hiding (sequence)
> import Control.Arrow

> (>*<) :: Applicative f => f a -> f b -> f (a, b)
> (>*<) = liftA2 (,)

> leftMap :: (a -> b) -> Either a c -> Either b c
> leftMap f = either (Left . f) Right

Strang has two modes: line mode and text mode.

> data Mode = LineMode | TextMode

A Strang command is a series of characters.
It starts with a mode character, 'l' for line mode or 't' for text mode.

> modeParser :: Parser Mode
> modeParser = consumingParser <|> pure LineMode where
>                   consumingParser = (AC.char 'l' $> LineMode) <|> (AC.char 't' $> TextMode)

This is for passing strings to commands.

> stringArg :: Parser ByteString
> stringArg = inQuotes $ C.pack <$> manyTill AC.anyChar quoteChar where
>               inQuotes p = quoteChar *> p <* quoteChar
>               quoteChar = AC.char '"'

This is for passing single characters to commands.

> charArg :: Parser Char
> charArg = AC.anyChar

Interpreter state type. Note the recursion in ListState, which is used below
in `stateCata` to support arbitrarily-nested commands.

> data StrangState = StringState ByteString | ListState [StrangState]
>    deriving Show

> data StrangError = StrangTypeError ByteString deriving (Show)

> type CommandResult r = WriterT [ByteString] (Either StrangError) r

> liftError :: StrangError -> CommandResult r
> liftError = WriterT . Left

> strError :: String -> CommandResult r
> strError = liftError . StrangTypeError . C.pack

Command type. Basically a function between states, with a log.

> type Command = StrangState -> CommandResult StrangState

> type Modal a = Reader Mode a

> orElse :: CommandResult a -> CommandResult a -> CommandResult a
> orElse res1 res2
>   | isRight (runWriterT res1) = res1
>   | isRight (runWriterT res2) = res2
>   | otherwise = res1

Makes a command fold over states. Not sure exactly what the best name is for
this yet. Basically, this attempts to run commands at the highest possible
level in a nested ListState, and recurses through the levels if it fails.

> stateCata :: Command -> Command
> stateCata cmd st@(ListState bss) = let runNested = ListState <$> traverse (stateCata cmd) bss in
>                                       cmd st `orElse` runNested
> stateCata cmd st = cmd st

Split command implementation.

> splitCommand :: Char -> Command
> splitCommand ch (StringState str) = pure $ ListState (StringState <$> C.split ch str)
> splitCommand _ st = liftError (StrangTypeError $ C.pack $ "can't split " ++ show st)

Print command implementation.

> printCommand :: Command
> printCommand state = let res = C.pack $ show state in
>                        WriterT $ Right (StringState res, [res])

Almost-command that returns the string in the passed state, or fails.

> onlyString :: StrangState -> WriterT [ByteString] (Either StrangError) ByteString
> onlyString (StringState str) = pure str
> onlyString st = strError ("just wanted a string, got " ++ show st)

Join command implementation.

> joinCommand :: ByteString -> Command
> joinCommand sep (ListState bss) = traverse onlyString bss >>= join where
>                   join strs = pure $ StringState (BS.intercalate sep strs)
> joinCommand _ st = strError ("just wanted a recursive list of strings, got " ++ show st)

Regex command.

> makeRegexCommand :: ByteString -> Either ParseError Command
> makeRegexCommand reg = let withStringErr = regexCommand <$> compile defaultCompOpt defaultExecOpt reg in
>                            leftMap (flip newErrorMessage (initialPos "") . Message) withStringErr

> regexCommand :: Regex -> Command
> regexCommand reg (StringState str) = ListState <$> pure (StringState <$> res) where
>                                            res :: [ByteString]
>                                            res = matchM reg str
> regexCommand _ st = strError ("just wanted a string, got " ++ show st)

Split command parser. Syntax is:

    s<char>


> splitParser :: Parser Command
> splitParser = splitCommand <$> (AC.char 's' *> charArg)

Print command parser. Syntax is:

    <other commands>p

Basically it appends the current value to the log.

> printParser :: Parser Command
> printParser = AC.char 'p' $> printCommand

Join command parser. Joins the elements of a list of strings. Syntax is:

    <other commands>j"delim"

> joinParser :: Parser Command
> joinParser = joinCommand <$> (AC.char 'j' *> stringArg)

Regex command parser! Syntax is:

    r"<regexstuff>"

> regexParser :: Parser (Either ParseError Command)
> regexParser = makeRegexCommand <$> (AC.char 'r' *> stringArg)

Parsers that don't need to do any additional verification before succeeding.

> pureCommandParser :: Parser Command
> pureCommandParser = splitParser <|> printParser <|> joinParser

Parser for any command.

> commandParser :: Parser (Either ParseError Command)
> commandParser = (pure <$> pureCommandParser) <|> regexParser

> programParser :: Parser (Either ParseError (Mode, [Command]))
> programParser = do
>           mode <- modeParser
>           commands <- sequence <$> many1 commandParser
>           pure $ pure mode >*< commands

> runCommand :: [Command] -> ByteString -> CommandResult StrangState
> runCommand cmds start = foldl (>>=) (pure $ StringState start) (fmap stateCata cmds)

> programInputForMode :: Mode -> IO ByteString
> programInputForMode LineMode = BS.getLine
> programInputForMode TextMode = BS.getContents

> printProgramResult :: CommandResult StrangState -> IO ()
> printProgramResult = print

> printParsingError :: ParseError -> IO ()
> printParsingError = print

> putIn :: Functor m => a -> m b -> m (a, b)
> putIn x = fmap (const x &&& id)

> parseProgram :: ByteString -> Either ParseError (Mode, [Command])
> parseProgram program = join $ parse programParser "" program

> interpretProgram :: ByteString -> IO ()
> interpretProgram cmd = let programAndModeOrErr = parseProgram cmd in
>                           either printParsingError (\(mode, cmd) ->
>                               (runCommand cmd <$> programInputForMode mode) >>= printProgramResult) programAndModeOrErr

> main :: IO ()
> main = do
>   args <- getArgs
>   maybe (putStrLn "No command!") (interpretProgram . C.pack) (headMay args)





























