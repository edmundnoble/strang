{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds ,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification,TemplateHaskell #-}


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
import Debug.Trace
import Data.Void

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

data ParamTy a where
    StringTy :: ParamTy ByteString
    ListTy :: ParamTy a -> ParamTy [a]
    UnitTy :: ParamTy ()
    AnyTy :: ParamTy ()


data FunTy a b where
    Specified :: ParamTy a -> ParamTy b -> FunTy a b
    Forgetful :: ParamTy b -> FunTy a b
    IdLike :: FunTy a a

data UnTy = forall a. UnTy (ParamTy a)
data UnFunTy = forall a b. UnFunTy (FunTy a b)
data UnFun = forall a b. UnFun (a -> b)

instance Show UnTy where
  show (UnTy t) = "ANON: " ++ show t

instance Show UnFunTy where
  show (UnFunTy t) = "ANON: " ++ show t

instance Show (FunTy a b) where
  show (Specified a b) = "S (" ++ show a ++ " -> " ++ show b ++ ")"
  show (Forgetful b) = "S (a -> " ++ show b ++ ")"
  show IdLike = "S (a -> a)"

instance Eq UnTy where
  (==) (UnTy StringTy) (UnTy StringTy) = True
  (==) (UnTy UnitTy) (UnTy UnitTy) = True
  (==) (UnTy (ListTy t)) (UnTy (ListTy y)) = UnTy t == UnTy y
  (==) (UnTy AnyTy) (UnTy AnyTy) = True
  (==) _ _ = False

instance Eq UnFunTy where
  (UnFunTy IdLike) == (UnFunTy IdLike) = True
  (UnFunTy (Forgetful a)) == (UnFunTy (Forgetful b)) = UnTy a == UnTy b
  (UnFunTy (Specified a b)) == (UnFunTy (Specified c d)) = (UnTy a == UnTy c) && (UnTy b == UnTy d)
  (UnFunTy _) == (UnFunTy _) = False

instance Show (ParamTy a) where
    show StringTy = "String"
    show (ListTy ty) = "List[" ++ show ty ++ "]"
    show UnitTy = "Unit"
    show AnyTy = "Any"

class HasParamTy a where
  defParamTy :: ParamTy a

class HasFunTy a b where
  defFunTy :: FunTy a b

instance HasParamTy ByteString where
  defParamTy = StringTy

instance HasParamTy () where
  defParamTy = UnitTy

instance HasParamTy a => HasParamTy [a] where
  defParamTy = ListTy defParamTy

instance (HasParamTy a, HasParamTy b) => HasFunTy a b where
  defFunTy = Specified defParamTy defParamTy

data StrangState a where
  StrangState :: (HasParamTy a, Show a) => a -> StrangState a

makeState :: (Show a, HasParamTy a) => a -> StrangState a
makeState = StrangState

instance Show (StrangState a) where
  show (StrangState s) = show s

grab :: StrangState a -> a
grab (StrangState a) = a

typeOf :: StrangState a -> ParamTy a
typeOf ss@(StrangState _) = defParamTy

type CommandResult r = Writer [ByteString] r

-- Command type. Basically a function between states, with runtime type info and a log.
data Command i o = Command { run     :: i -> CommandResult o
                           , commandType :: FunTy i o
                           , name :: String }

liftCommand :: Command i o -> Command [i] [o]
liftCommand cmd@Command { commandType = Specified a b, run = f } = cmd { run = traverse f, commandType = Specified (ListTy a) (ListTy b) }
liftCommand cmd@Command { commandType = IdLike } = Command { commandType = IdLike, run = traverse $ run cmd, name = name cmd }
liftCommand cmd@Command { commandType = Forgetful a, run = f } = cmd { run = traverse f, commandType = Forgetful (ListTy a) }

composeFunTy :: FunTy a b -> FunTy b c -> FunTy a c
composeFunTy _ (Forgetful t) = Forgetful t
composeFunTy (Forgetful _) (Specified _ c) = Forgetful c
composeFunTy (Specified a _) (Specified _ c) = Specified a c
composeFunTy a IdLike = a
composeFunTy IdLike a = a

-- Makes a command map over lists. Not sure exactly what the best name is for
-- this yet. Basically, this attempts to run commands at the highest possible
-- level in a nested ListState, and recurses through the levels if it fails.

autocombine :: AnyCommand -> AnyCommand -> Either String AnyCommand
autocombine = undefined {-c1@Exists { runAny = Command { run = f } } c2@Exists { runAny = cmd@Command { run = g } } =
  let (ity1, ity2) = (inTyAny c1, inTyAny c2)
      (oty1, oty2) = (outTyAny c1, outTyAny c2) in
          if canCombineWith oty1 ity2 then combineCommands c1 c2
          else case oty1 of
              UnTy (ListTy t1) -> trace ("Recursively calling autocombine to unify " ++ show c1 ++ " with " ++ show c2) $ autocombine c1 Exists { runAny = cmd { run = unsafeCoerce (sequence . map g), commandType = commandType $ liftCommand cmd, name = "Lifted(" ++ name cmd ++ ")" } }
              otherwise -> Left $ "Could not unify " ++ show c1 ++ " with " ++ show c2-}

commandR :: FunTy a b -> String -> (a -> CommandResult b) -> Command a b
commandR ty n f = Command { run = f, commandType = ty, name = n }

commandS :: (Default (ParamTy a), Default (ParamTy b)) => String -> (a -> CommandResult b) -> Command a b
commandS = commandR (Specified def def)

command :: (Default (ParamTy a), Default (ParamTy b)) => String -> (a -> b) -> Command a b
command n f = commandS n (pure . f)

-- Split command implementation.

splitCommand :: Char -> Command ByteString [ByteString]
splitCommand = command "Split" . C.split

-- Print command implementation.

printCommand :: ParamTy a -> Command a ()
printCommand ty = Command { run = \st -> writer ((), [printTyped ty st]), commandType = Forgetful UnitTy }

printTyped :: ParamTy a -> a -> ByteString
printTyped UnitTy _ = C.pack "()"
printTyped StringTy str = str
printTyped (ListTy t) ts = C.pack "[" `C.append` BS.intercalate (C.pack ",") (fmap (printTyped t) ts) `C.append` C.pack "]"

-- Join command implementation.

joinCommand :: ByteString -> Command [ByteString] ByteString
joinCommand = command "Join" . BS.intercalate

-- Regex command.

makeRegexCommand :: Bool -> ByteString -> Either ParseError (Command ByteString [ByteString])
makeRegexCommand captureGroups reg = let execOptions = ExecOption { captureGroups }
                                         withStringErr = regexCommand <$> compile defaultCompOpt execOptions reg in
                           leftMap (flip newErrorMessage (initialPos "") . Message) withStringErr

matchRegex :: Regex -> ByteString -> [ByteString]
matchRegex reg str = head $ match reg str

regexCommand :: Regex -> Command ByteString [ByteString]
regexCommand = command "Regex" . matchRegex

-- Split command parser. Syntax is:
-- s<char>

splitParser :: Parser (Command ByteString [ByteString])
splitParser = splitCommand <$> (char 's' *> charArg)

-- Print command parser. Syntax is:

--    <other commands>p

-- Basically it appends the current value to the log.

printParser :: Parser (ParamTy a -> Command a ())
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

collapseError :: Show e => Parser (Either e a) -> Parser a
collapseError p = p >>= either (fail . show) pure

-- Capturing regex command parser. Syntax is:
-- c"<regexstuff>"

captureRegexParser :: Parser (Command ByteString [ByteString])
captureRegexParser = collapseError $ makeRegexCommand True <$> (char 'c' *> stringArg)

(<||>) :: Parser AnyCommand -> Parser (Command a b) -> Parser AnyCommand
ab <||> cd = ab <|> fmap Exists cd

commandParser :: Parser AnyCommand
commandParser = parserZero <||> splitParser <||> joinParser <||> noCaptureRegexParser <||> captureRegexParser
-- what do I do with print?

data AnyCommand = forall a b. Exists { runAny :: Command a b }

instance Show AnyCommand where
  show e@Exists { runAny = c@Command { commandType = ct } } = name c ++ " :: (" ++ show ct ++ ")"

funTyAny :: AnyCommand -> UnFunTy
funTyAny Exists { runAny = c } = (UnFunTy . commandType) c

--bracketedCommandParser :: Parser [AnyCommand]
--bracketedCommandParser = char '(' *> (many1 commandParser <|> (join <$> many1 bracketedCommandParser)) <* char ')'

programParser :: Parser (Mode, [AnyCommand])
programParser = (modeParser >*< many1 commandParser) <* eof

composeCommands :: Command a b -> Command b c -> Command a c
composeCommands ab bc = Command { commandType = composeFunTy (commandType ab) (commandType bc)
                                , run = run ab >=> run bc
                                , name = "(" ++ name bc ++ " . " ++ name ab ++ ")" }

canCombineWith :: UnTy -> UnTy -> Bool
canCombineWith a1 a2 = let inTypeGeneric = (a2 == UnTy AnyTy)
                           outTypeGeneric = (a1 == UnTy AnyTy) in
                            (a2 == a1) || inTypeGeneric || outTypeGeneric

-- This is a bad way to do this. I need a way to lift the runtime equality to type-level equality
{-combineCommands :: AnyCommand -> AnyCommand -> Either String AnyCommand
combineCommands a1@Exists{ runAny = f@Command { commandType = ct1 } } a2@Exists{ runAny = g@Command { commandType = ct2 } } = if UnFunTy ct1 `canCombineWith` UnFunTy ct2 then (Right . Exists) (composeCommands f g) else Left $ "Could not unify " ++ show a1 ++ " with " ++ show a2-}

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Right Exists { runAny = Command { run = pure, commandType = IdLike, name = "Identity" } }
typecheckCommands (x:xs) = foldM autocombine x xs

canUnify :: UnFunTy -> UnFunTy -> Bool
canUnify (UnFunTy (Specified a b)) (UnFunTy (Specified c d)) = UnTy a == UnTy c && UnTy b == UnTy d
canUnify (UnFunTy IdLike) (UnFunTy (Specified a b)) = UnTy a == UnTy b
canUnify (UnFunTy (Specified a b)) (UnFunTy IdLike) = UnTy a == UnTy b

commandWithType :: FunTy a b -> AnyCommand -> Either String (Command a b)
commandWithType ct e@Exists { runAny = c@Command { run = fun } } =
  if canUnify (UnFunTy ct) (UnFunTy (commandType c)) then
    Right Command { run = unsafeCoerce fun
                  , commandType = ct
                  , name = name c }
  else
    Left $ "Could not unify " ++ show ct ++ " with " ++ show e

runCommand :: [AnyCommand] -> Either String (ByteString -> [ByteString])
runCommand [] = Left "No command!"
runCommand cmds = do
  f <- run <$> (typecheckCommands (cmds) >>= commandCanTake StringTy)
  return $ execWriter . f

programInputForMode :: Mode -> IO ByteString
programInputForMode LineMode = BS.getLine
programInputForMode TextMode = BS.getContents

printProgramResult :: Show a => CommandResult a -> IO ()
printProgramResult = print

printParsingError :: ParseError -> IO ()
printParsingError = print

putIn :: Functor m => a -> m b -> m (a, b)
putIn x = fmap (const x &&& id)

parseProgram :: ByteString -> Either String (Mode, [AnyCommand])
parseProgram pgm = leftMap show (parse programParser "" pgm)

interpretProgram :: ByteString -> IO ()
interpretProgram cmd = let programAndModeOrErr = parseProgram cmd
                           compiledProgramOrErr = programAndModeOrErr >>= (\(m, c) -> putIn m ((\f -> foldl' BS.append BS.empty . f) <$> runCommand c)) in
                                either (\err -> C.putStrLn $ C.pack $ "Compilation error: " ++ err) (\(mode, cmd) -> do
                                  input <- programInputForMode mode
                                  C.putStrLn $ cmd input) compiledProgramOrErr

main :: IO ()
main = do
   program <- BS.getLine
   interpretProgram program

d = [| "" |]
