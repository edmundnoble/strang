> {-# LANGUAGE DataKinds,TypeFamilies,GADTs,RankNTypes #-}

> module Main (
>     main
> ) where

> import Text.Regex.TDFA.ByteString
> import qualified Data.ByteString as BS
> import Data.ByteString (ByteString)
> import Data.Attoparsec.ByteString as A
> import qualified Data.Attoparsec.ByteString.Char8 as AC
> import Data.Monoid
> import Data.ByteString.Internal (c2w, w2c)
> import qualified Data.ByteString.Char8 as C
> import Data.Functor
> import Control.Monad.Writer.Strict
> import Data.Functor.Identity
> import Control.Applicative.Alternative
> import Data.Either.Combinators
> import System.Environment
> import Safe

Most of Strang's errors are strings.

> type StringOr a = Either String a

Strang has two modes: line mode and text mode.

> data Mode = Line | Text

A raw Strang command is a series of characters.
It starts with a mode character, 'l' for line mode or 't' for text mode.
(not used yet, everything is line mode)

> modeParser :: Parser Mode
> modeParser = (AC.char 'l' $> Line) `mappend` (AC.char 't' $> Text)

This is for passing strings to commands.

> stringArg :: Parser ByteString
> stringArg = AC.char '"' *> AC.takeTill (== '"') <* AC.char '"'

This is for passing single characters to commands.

> charArg :: Parser Char
> charArg = AC.anyChar

Interpreter state type. Note the recursion in ListState, which is used below
in `stateCata` to support arbitrarily-nested commands.

> data StrangState = StringState ByteString | ListState [StrangState]
>    deriving Show

> data StrangError = StrangTypeError ByteString deriving (Show)

 instance Error StrangError where

> strError :: String -> StrangError
> strError = StrangTypeError . C.pack

> type CommandResult = WriterT [ByteString] (Either StrangError) StrangState

Command type. Basically a function between states, with a log.

> type Command =  StrangState -> CommandResult

> orElse :: CommandResult -> CommandResult -> CommandResult
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
> splitCommand _ st = WriterT $ Left (StrangTypeError $ C.pack $ "can't split " ++ show st)

Print command implementation.

> printCommand :: Command
> printCommand state = let res = C.pack $ show state in
>                        WriterT $ Right (StringState res, [res])

Almost-command that returns the string in the passed state, or fails.

> onlyString :: StrangState -> WriterT [ByteString] (Either StrangError) ByteString
> onlyString (StringState str) = pure str
> onlyString st = WriterT $ Left $ strError ("just wanted a string, got " ++ show st)

Join command implementation.

> joinCommand :: ByteString -> Command
> joinCommand sep (ListState bss) = traverse onlyString bss >>= join where
>                   join strs = pure $ StringState (BS.intercalate sep strs)
> joinCommand _ st = WriterT $ Left $ strError ("just wanted a recursive list of strings, got " ++ show st)

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

Parser for any command.

> commandParser :: Parser Command
> commandParser = splitParser `mappend` printParser `mappend` joinParser

> programParser :: Parser [Command]
> programParser = many1' commandParser

> runCommand :: [Command] -> ByteString -> CommandResult
> runCommand cmds start = foldl (>>=) (pure $ StringState start) (fmap stateCata cmds)

> interpretCommand :: ByteString -> IO ()
> interpretCommand cmd = let commandOrErr = parseOnly programParser cmd in
>           either print (\a -> let cmd = runCommand a in forever ((cmd <$> BS.getLine) >>= print)) commandOrErr


> main :: IO ()
> main = do
>   args <- getArgs
>   maybe (putStrLn "No command!") (interpretCommand . C.pack) (headMay args)
































