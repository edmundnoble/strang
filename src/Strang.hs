{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds ,ScopedTypeVariables,TemplateHaskell,KindSignatures,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy #-}

module Main (
   main
) where

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Text.Parsec.ByteString
import Text.Parsec.Prim hiding ((<|>))
import Text.ParserCombinators.Parsec.Char (char, anyChar)
import Text.ParserCombinators.Parsec.Prim (parse)
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)
import Data.Monoid
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Char8 as C
import Data.Functor
import Control.Monad.Writer.Strict hiding (sequence)
import Data.Functor.Identity
import Control.Applicative.Alternative
import Data.Either.Combinators
import System.Environment
import Safe
import Data.Traversable hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Control.Arrow
import Data.Array
import Data.Default
import Data.Singletons
import Data.Singletons.Decide
import Unsafe.Coerce
import Data.List

(>*<) :: Applicative f => f a -> f b -> f (a, b)
(>*<) = liftA2 (,)

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right

 -- Strang has two modes: line mode and text mode.

data Mode = LineMode | TextMode

-- A Strang command is a series of characters.
-- It starts with a mode character, 'l' for line mode or 't' for text mode.
modeParser :: Parser Mode
modeParser = consumingParser <|> pure LineMode where
                 consumingParser = try (char 'l' $> LineMode) <|> try (char 't' $> TextMode)

-- This is for passing strings to commands.
stringArg :: Parser ByteString
stringArg = C.pack <$> surroundedBy (char '"') anyChar where
             surroundedBy q p = between q q $ manyTill p (try q) where

-- This is for passing single characters to commands.

charArg :: Parser Char
charArg = anyChar

-- Interpreter state type. Note the recursion in ListState, which is used below
-- in `cata` to support arbitrarily-nested commands.

data StateTy a where
    StringTy :: StateTy ByteString
    ListTy :: StateTy a -> StateTy [a]
    UnitTy :: StateTy ()
    AnyTy :: StateTy a

data UnTy = forall a. UnTy (StateTy a)

instance Eq (UnTy) where
  (==) (UnTy StringTy) (UnTy UnitTy) = False
  (==) (UnTy StringTy) (UnTy (ListTy _)) = False
  (==) (UnTy UnitTy) (UnTy (ListTy _)) = False
  (==) (UnTy (ListTy t)) (UnTy (ListTy y)) = (UnTy t == UnTy y)
  (==) (UnTy AnyTy) _ = True
  (==) _ (UnTy AnyTy) = True

instance Eq (StateTy a) where
  (==) a b = (UnTy a) == (UnTy b)

instance Show (StateTy a) where
    show StringTy = "String"
    show (ListTy ty) = "List(" ++ show ty ++ ")"
    show UnitTy = "(UNIT)"
    show AnyTy = "ANY"

instance Default (StateTy ByteString) where
  def = StringTy

instance Default (StateTy a) => Default (StateTy [a]) where
  def = ListTy def

instance Default (StateTy ()) where
  def = UnitTy

data StrangState a where
  StrangState :: (Show a, Default (StateTy a)) => (StateTy a) -> a -> StrangState a

makeState :: (Show a, Default (StateTy a)) => a -> StrangState a
makeState = StrangState def

instance Show (StrangState a) where
  show (StrangState _ s) = show s

grab :: StrangState a -> a
grab (StrangState _ a) = a

typeOf :: StrangState a -> StateTy a
typeOf (StrangState t _) = t

data StrangError = StrangTypeError ByteString deriving (Show)

type CommandResult r = WriterT [ByteString] (Either StrangError) r

liftError :: StrangError -> CommandResult r
liftError = WriterT . Left

strError :: String -> CommandResult r
strError = liftError . StrangTypeError . C.pack

-- Command type. Basically a function between states, with runtime type info and a log.
data Command i o = Command { run     :: StrangState i -> CommandResult (StrangState o)
                           , inType  :: StateTy i
                           , outType :: StateTy o }

orElse :: CommandResult a -> CommandResult a -> CommandResult a
orElse res1 res2
 | isRight (runWriterT res1) = res1
 | isRight (runWriterT res2) = res2
 | otherwise = res1

-- Makes a command fold over states. Not sure exactly what the best name is for
-- this yet. Basically, this attempts to run commands at the highest possible
-- level in a nested ListState, and recurses through the levels if it fails.

--cata cmd st@(StrangState t@(ListTy StringTy) bss) = let runNested = makeState <$> traverse (cata $ runCommand cmd) bss in
                               --runCommand cmd st `orElse` runNested
cata cmd st = undefined --cmd st

commandR :: (Default (StateTy a), Default (StateTy b)) => (StrangState a -> CommandResult (StrangState b)) -> Command a b
commandR f = Command { run = f, inType = def, outType = def }

command :: (Default (StateTy a), Default (StateTy b)) => (StrangState a -> StrangState b) -> Command a b
command f = commandR (pure . f)

-- Split command implementation.

splitCommand :: Char -> Command ByteString [ByteString]
splitCommand ch = command (\(StrangState StringTy str) -> makeState (C.split ch str))

-- Print command implementation.

printCommand :: Command i i
printCommand = Command { run = (\st -> WriterT $ (Right (st, [C.pack $ show st]))), inType = AnyTy, outType = AnyTy }

-- Join command implementation.

joinCommand :: ByteString -> Command [ByteString] ByteString
joinCommand sep = command (\(StrangState _ bss) -> makeState (BS.intercalate sep bss))

-- Regex command.

makeRegexCommand :: Bool -> ByteString -> Either ParseError (Command ByteString [ByteString])
makeRegexCommand captureGroups reg = let execOptions = ExecOption { captureGroups }
                                         withStringErr = regexCommand <$> compile defaultCompOpt execOptions reg in
                           leftMap (flip newErrorMessage (initialPos "") . Message) withStringErr

matchRegex :: Regex -> ByteString -> StrangState [ByteString]
matchRegex reg str = (makeState . head) $ match reg str

regexCommand :: Regex -> Command ByteString [ByteString]
regexCommand reg = command (\(StrangState _ str) -> matchRegex reg str)

-- Split command parser. Syntax is:
-- s<char>

splitParser :: Parser (Command ByteString [ByteString])
splitParser = splitCommand <$> (char 's' *> charArg)

-- Print command parser. Syntax is:

--    <other commands>p

-- Basically it appends the current value to the log.

printParser :: Parser (Command i i)
printParser = char 'p' $> printCommand

-- Join command parser. Joins the elements of a list of strings. Syntax is:

--    <other commands>j"delim"

joinParser :: Parser (Command [ByteString] ByteString)
joinParser = joinCommand <$> (char 'j' *> stringArg)

-- Direction switch command parser.

directionSwitchParser :: Parser ()
directionSwitchParser = void $ char '<'

-- Index command parser. Indexes into the farthest outer liststate.

indexParser :: Parser a
indexParser = fail "index parser not implemented"

-- Non-capturing regex command parser! Syntax is:
--    r"<regexstuff>"

noCaptureRegexParser :: Parser (Command ByteString [ByteString])
noCaptureRegexParser = collapseError $ makeRegexCommand False <$> (char 'r' *> stringArg)

merge :: Either a a -> a
merge = either id id

collapseError :: Show e => Parser (Either e a) -> Parser a
collapseError p = p >>= (either (\e -> fail $ shows e "") pure)

-- Capturing regex command parser. Syntax is:
-- c"<regexstuff>"

captureRegexParser :: Parser (Command ByteString [ByteString])
captureRegexParser = collapseError $ makeRegexCommand True <$> (char 'c' *> stringArg)

(<||>) :: Parser (AnyCommand) -> Parser (Command a b) -> Parser (AnyCommand)
ab <||> cd = ab <|> (fmap Exists cd)

commandParser :: Parser AnyCommand
commandParser = (parserZero <||> splitParser <||> printParser <||> joinParser <||> noCaptureRegexParser <||> captureRegexParser)

data AnyCommand = forall a b. Exists { runAny :: Command a b }

inTypeAny :: AnyCommand -> UnTy
inTypeAny Exists { runAny = Command { inType = inTy } } = UnTy inTy

outTypeAny :: AnyCommand -> UnTy
outTypeAny Exists { runAny = Command { outType = outTy } } = UnTy outTy

bracketedCommandParser :: Parser [AnyCommand]
bracketedCommandParser = char '(' *> ((many1 commandParser) <|> (join <$> many1 bracketedCommandParser)) <* char ')'

programParser :: Parser (Mode, [AnyCommand])
programParser = (modeParser >*< (join <$> many1 bracketedCommandParser)) <* eof

composeCommands :: Command b c -> Command a b -> Command a c
composeCommands bc ab = Command { inType = inType ab
                                , outType = outType bc
                                , run = run ab >=> run bc }

canCombineWith :: AnyCommand -> AnyCommand -> Bool
canCombineWith a1 a2 = let inTypeGeneric = (inTypeAny a1 == (UnTy AnyTy)) in
                        let outTypeGeneric = (outTypeAny a1 == (UnTy AnyTy)) in
                          False


-- This is a bad way to do this. I need a way to lift the runtime equality to type-level equality
combineCommands :: AnyCommand -> AnyCommand -> Maybe AnyCommand
combineCommands a1 a2 = if (a1 `canCombineWith` a2) then Nothing else Nothing
{-combineCommands Exists { runAny = f } Exists { runAny = g } = if UnTy (outType f) == UnTy (inType g) then Just $ Exists $ unsafeCoerce $ composeCommands (unsafeCoerce f) (unsafeCoerce g)
                      else                          Nothing-}

typecheckCommands :: [AnyCommand] -> Maybe AnyCommand
typecheckCommands [] = Nothing
typecheckCommands (x:xs) = foldM combineCommands x xs

commandWithType :: StateTy a -> StateTy b -> AnyCommand -> Maybe (Command a b)
commandWithType t1 t2 e@(Exists { runAny = Command { run = fun } } ) =
  if (inTypeAny e == UnTy t1) && (outTypeAny e == UnTy t2) then
    Just $ Command { run = unsafeCoerce fun
                    ,inType = t1
                    ,outType = t2 }
  else
    Nothing

runCommand :: [AnyCommand] -> Maybe (ByteString -> CommandResult ByteString)
runCommand [] = Nothing
runCommand cmds = do
  f <- (run <$> (typecheckCommands cmds >>= (commandWithType StringTy StringTy)))
  return $ ((<$>) grab) . f . makeState

programInputForMode :: Mode -> IO ByteString
programInputForMode LineMode = BS.getLine
programInputForMode TextMode = BS.getContents

printProgramResult :: Show a => CommandResult a -> IO ()
printProgramResult = print

printParsingError :: ParseError -> IO ()
printParsingError = print

putIn :: Functor m => a -> m b -> m (a, b)
putIn x = fmap (const x &&& id)

parseProgram :: ByteString -> Either ParseError (Mode, [AnyCommand])
parseProgram = parse programParser ""

interpretProgram :: ByteString -> IO ()
interpretProgram cmd = let programAndModeOrErr = parseProgram cmd
                           compiledProgramOrErr = (\(mode, cmd) -> (mode, runCommand cmd)) <$> programAndModeOrErr in
                             either printParsingError (\(mode, cmd) ->
                                ((maybe (return $ pure $ C.pack "Not is compile") (\prog -> prog <$> (programInputForMode mode)) cmd)) >>= printProgramResult)
                                compiledProgramOrErr

main :: IO ()
main = do
   program <- BS.getLine
   interpretProgram program
