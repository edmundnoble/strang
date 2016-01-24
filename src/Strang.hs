{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds ,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification #-}

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
             surroundedBy q p = q *> manyTill p (try q)

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

instance Show UnTy where
  show (UnTy t) = "Anonymously: " ++ show t

instance Eq UnTy where
  (==) (UnTy StringTy) (UnTy StringTy) = True
  (==) (UnTy UnitTy) (UnTy UnitTy) = True
  (==) (UnTy (ListTy t)) (UnTy (ListTy y)) = UnTy t == UnTy y
  (==) (UnTy AnyTy) (UnTy AnyTy) = True
  (==) _ _ = False

instance Eq (StateTy a) where
  (==) a b = UnTy a == UnTy b

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
  StrangState :: (Show a, Default (StateTy a)) => StateTy a -> a -> StrangState a

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
                           , outType :: StateTy o
                           , name :: String }

orElse :: CommandResult a -> CommandResult a -> CommandResult a
orElse res1 res2
 | isRight (runWriterT res1) = res1
 | isRight (runWriterT res2) = res2
 | otherwise = res1

-- Makes a command fold over states. Not sure exactly what the best name is for
-- this yet. Basically, this attempts to run commands at the highest possible
-- level in a nested ListState, and recurses through the levels if it fails.

-- lol jk

commandR :: (Default (StateTy a), Default (StateTy b)) => String -> (StrangState a -> CommandResult (StrangState b)) -> Command a b
commandR n f = Command { run = f, inType = def, outType = def, name = n }

command :: (Default (StateTy a), Default (StateTy b)) => String -> (StrangState a -> StrangState b) -> Command a b
command n f = commandR n (pure . f)

-- Split command implementation.


splitCommand :: Char -> Command ByteString [ByteString]
splitCommand ch = command "Split" (\(StrangState _ str) -> makeState (C.split ch str))

-- Print command implementation.

printCommand :: Command i i
printCommand = Command { run = \st -> WriterT (Right (st, [C.pack $ show st])), inType = AnyTy, outType = AnyTy, name = "Print" }

-- Join command implementation.

joinCommand :: ByteString -> Command [ByteString] ByteString
joinCommand sep = command "Join" (\(StrangState _ bss) -> makeState (BS.intercalate sep bss))

-- Regex command.

makeRegexCommand :: Bool -> ByteString -> Either ParseError (Command ByteString [ByteString])
makeRegexCommand captureGroups reg = let execOptions = ExecOption { captureGroups }
                                         withStringErr = regexCommand <$> compile defaultCompOpt execOptions reg in
                           leftMap (flip newErrorMessage (initialPos "") . Message) withStringErr


matchRegex :: Regex -> ByteString -> StrangState [ByteString]
matchRegex reg str = (makeState . head) $ match reg str

regexCommand :: Regex -> Command ByteString [ByteString]
regexCommand reg = command "Regex" (\(StrangState _ str) -> matchRegex reg str)

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
collapseError p = p >>= either (\e -> fail $ shows e "") pure

-- Capturing regex command parser. Syntax is:
-- c"<regexstuff>"

captureRegexParser :: Parser (Command ByteString [ByteString])
captureRegexParser = collapseError $ makeRegexCommand True <$> (char 'c' *> stringArg)

(<||>) :: Parser AnyCommand -> Parser (Command a b) -> Parser AnyCommand
ab <||> cd = ab <|> fmap Exists cd

commandParser :: Parser AnyCommand
commandParser = parserZero <||> splitParser <||> printParser <||> joinParser <||> noCaptureRegexParser <||> captureRegexParser

data AnyCommand = forall a b. Exists { runAny :: Command a b }

instance Show AnyCommand where
  show e@Exists { runAny = c } = name c ++ " :: (" ++ show (inTyAny e) ++ "->" ++ show (outTyAny e) ++ ")"

inTyAny :: AnyCommand -> UnTy
inTyAny Exists { runAny = Command { inType = inTy } } = UnTy inTy

outTyAny :: AnyCommand -> UnTy
outTyAny Exists { runAny = Command { outType = outTy } } = UnTy outTy

--bracketedCommandParser :: Parser [AnyCommand]
--bracketedCommandParser = char '(' *> (many1 commandParser <|> (join <$> many1 bracketedCommandParser)) <* char ')'

programParser :: Parser (Mode, [AnyCommand])
programParser = (modeParser >*< many1 commandParser) <* eof

composeCommands :: Command a b -> Command b c -> Command a c
composeCommands ab bc = Command { inType = inType ab
                                , outType = outType bc
                                , run = run ab >=> run bc
                                , name = "(" ++ name bc ++ " . " ++ name ab ++ ")" }

canCombineWith :: UnTy -> UnTy -> Bool
canCombineWith a1 a2 = let inTypeGeneric = (a2 == UnTy AnyTy)
                           outTypeGeneric = (a1 == UnTy AnyTy) in
                            (a2 == a1) || inTypeGeneric || outTypeGeneric

-- This is a bad way to do this. I need a way to lift the runtime equality to type-level equality
combineCommands :: AnyCommand -> AnyCommand -> Either String AnyCommand
combineCommands a1@Exists{ runAny = f } a2@Exists{ runAny = g } = if outTyAny a1 `canCombineWith` inTyAny a2 then (Right . Exists) (composeCommands (unsafeCoerce f) (unsafeCoerce g)) else Left $ "Could not unify " ++ show a1 ++ " with " ++ show a2

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Right Exists { runAny = Command { run = pure, inType = AnyTy, outType = AnyTy, name = "Identity" } }
typecheckCommands (x:xs) = foldM combineCommands x xs

commandWithType :: StateTy a -> StateTy b -> AnyCommand -> Either String (Command a b)
commandWithType t1 t2 e@Exists { runAny = c@Command { run = fun } } =
  if UnTy t1 `canCombineWith` inTyAny e && UnTy t2 `canCombineWith` outTyAny e then
    Right Command { run = unsafeCoerce fun
                  , inType = t1
                  , outType = t2
                  , name = name c }
  else
    Left $ "Could not unify (" ++ show t1 ++ " -> " ++ show t2 ++ ") with " ++ show e

runCommand :: [AnyCommand] -> Either String (ByteString -> CommandResult ByteString)
runCommand [] = Left "No command!"
runCommand cmds = do
  f <- run <$> (typecheckCommands (cmds ++ [Exists { runAny = printCommand }]) >>= commandWithType StringTy StringTy)
  return $ (<$>) grab . f . makeState

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
                           compiledProgramOrErr = second runCommand <$> programAndModeOrErr in
                             either printParsingError (\(mode, cmd) ->
                                either (\err -> return $ pure $ C.pack $ "Compilation error: " ++ err) (`fmap` programInputForMode mode) cmd >>= printProgramResult)
                                compiledProgramOrErr

main :: IO ()
main = do
   program <- BS.getLine
   interpretProgram program
