> {-# LANGUAGE DataKinds,TypeFamilies,GADTs,KindSignatures,RankNTypes,EmptyDataDecls,FunctionalDependencies #-}

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

Most of Strang's errors are strings.

> type StringOr a = Either String a

Strang has two modes: line mode and text mode.

> data Mode = Line | Text

A raw Strang command is a series of characters.
It starts with a mode character, 'l' for line mode or 't' for text mode.

> modeParser :: Parser Mode
> modeParser = (AC.char 'l' $> Line) `mappend` (AC.char 't' $> Text)

This is for parsing strings inside quotes.
It's used as the primary argument-passing method.

> stringArg :: Parser ByteString
> stringArg = AC.char '"' *> AC.takeTill (== '"') <* AC.char '"'

This is for passing single characters.

> charArg :: Parser Char
> charArg = AC.anyChar

Interpreter state type.

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


Makes a command fold over states.

> stateCata :: Command -> Command
> stateCata cmd st@(ListState bss) = let runNested = ListState <$> traverse (stateCata cmd) bss in
>                                       cmd st `orElse` runNested
> stateCata cmd st = cmd st

Split command.

> splitCommand :: Char -> Command
> splitCommand ch (StringState str) = WriterT $ Right (ListState (StringState <$> C.split ch str), [])
> splitCommand _ st = WriterT $ Left (StrangTypeError $ C.pack $ "can't split " ++ show st)

Print command.

> printCommand :: Command
> printCommand state = let res = C.pack $ show state in
>                        WriterT $ Right (StringState res, [res])

> onlyString :: StrangState -> WriterT [ByteString] (Either StrangError) ByteString
> onlyString (StringState str) = pure str
> onlyString st = WriterT $ Left $ strError ("just wanted a string, got " ++ show st)


Join command.

> joinCommand :: ByteString -> Command
> joinCommand sep (ListState bss) = traverse onlyString bss >>= (\strs -> WriterT $ pure (StringState $ BS.intercalate sep strs, []))
> joinCommand _ st = WriterT $ Left $ strError ("just wanted a recursive list of strings, got " ++ show st)

Split command parser. Syntax is:

    s'<char>'


> splitParser :: Parser Command
> splitParser = splitCommand <$> (AC.char 's' *> charArg)

Print command parser. Syntax is:

    <other commands>p

> printParser :: Parser Command
> printParser = AC.char 'p' $> printCommand

Join command parser. Joins the elements of a list of strings. Syntax is:

    <other commands>j"delim"

> joinParser :: Parser Command
> joinParser = joinCommand <$> (AC.char 'j' *> stringArg)

> commandParser :: Parser Command
> commandParser = splitParser `mappend` printParser `mappend` joinParser

> programParser :: Parser [Command]
> programParser = many1' commandParser

> commandRunner :: [Command] -> ByteString -> CommandResult
> commandRunner cmds start = foldl (>>=) (pure $ StringState start) (fmap stateCata cmds)


Runs a command, including the log.

> main :: IO ()
> main = do
>   line <- BS.getLine
>   let commandOrErr = parseOnly programParser line in
>           either print (\a -> print $ commandRunner a (C.pack "hello,world,fuck,you")) commandOrErr













































