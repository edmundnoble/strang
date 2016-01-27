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
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)
import qualified Data.ByteString.Char8 as C
import Data.Functor
import Control.Monad.Writer.Strict hiding (sequence)
import Control.Applicative.Alternative
import Control.Arrow
import Unsafe.Coerce
import Data.List
import Data.Void
import Debug.Trace

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

data FunTy a b where
    Specified :: ParamTy a -> ParamTy b -> FunTy a b
    Constant :: ParamTy b -> FunTy a b
    IdLike :: FunTy a a

data UnTy = forall a. UnTy (ParamTy a)
data UnFunTy = forall a b. UnFunTy (FunTy a b)

instance Show UnTy where
  show (UnTy t) = "ANON: " ++ show t

instance Show UnFunTy where
  show (UnFunTy t) = "ANON: " ++ show t

instance Show (FunTy a b) where
  show (Specified a b) = "S (" ++ show a ++ " -> " ++ show b ++ ")"
  show (Constant b) = "S (a -> " ++ show b ++ ")"
  show IdLike = "S (a -> a)"

instance Eq UnTy where
  (==) (UnTy StringTy) (UnTy StringTy) = True
  (==) (UnTy (ListTy t)) (UnTy (ListTy y)) = UnTy t == UnTy y
  (==) _ _ = False

instance Eq UnFunTy where
  (UnFunTy IdLike) == (UnFunTy IdLike) = True
  (UnFunTy (Constant a)) == (UnFunTy (Constant b)) = UnTy a == UnTy b
  (UnFunTy (Specified a b)) == (UnFunTy (Specified c d)) = (UnTy a == UnTy c) && (UnTy b == UnTy d)
  (UnFunTy _) == (UnFunTy _) = False

instance Show (ParamTy a) where
    show StringTy = "String"
    show (ListTy ty) = "List[" ++ show ty ++ "]"

class HasParamTy a where
  defParamTy :: ParamTy a

class HasFunTy a b where
  defFunTy :: FunTy a b

instance HasParamTy ByteString where
  defParamTy = StringTy

instance HasParamTy a => HasParamTy [a] where
  defParamTy = ListTy defParamTy

instance (HasParamTy a, HasParamTy b) => HasFunTy a b where
  defFunTy = Specified defParamTy defParamTy

data StrangState a where
  StrangState :: (HasParamTy a, Show a) => a -> StrangState a

instance Show (StrangState a) where
  show (StrangState s) = show s

type CommandResult r = Writer [ByteString] r

-- Command type. Basically a function between states, with runtime type info and a log.
data Command i o = Command { run     :: i -> CommandResult o
                           , commandType :: FunTy i o
                           , name :: String }

liftCommand :: Command i o -> Command [i] [o]
liftCommand cmd@Command { commandType = Specified a b, run = f } = cmd { run = traverse f, commandType = Specified (ListTy a) (ListTy b) }
liftCommand cmd@Command { commandType = IdLike } = Command { commandType = IdLike, run = traverse $ run cmd, name = name cmd }
liftCommand cmd@Command { commandType = Constant a, run = f } = cmd { run = traverse f, commandType = Constant (ListTy a) }

composeFunTy :: FunTy a b -> FunTy b c -> FunTy a c
composeFunTy _ (Constant t) = Constant t
composeFunTy (Constant _) (Specified _ c) = Constant c
composeFunTy (Specified a _) (Specified _ c) = Specified a c
composeFunTy a IdLike = a
composeFunTy IdLike a = a

-- Makes a command map over lists. Not sure exactly what the best name is for
-- this yet. Basically, this attempts to run commands at the highest possible
-- level in a nested ListState, and recurses through the levels if it fails.

autocombine :: AnyCommand -> AnyCommand -> Either String AnyCommand
autocombine e1@Exists { runAny = c1 } e2@Exists { runAny = c2 } = let (ct1, ct2) = (funTyAny e1, funTyAny e2) in
          if canCombineWith ct1 ct2 then combineCommands e1 e2
          else case ct1 of
              UnFunTy (Specified _ (ListTy _)) -> trace ("Recursively calling autocombine to unify " ++ show e1 ++ " with " ++ show e2) $ autocombine Exists { runAny = c1 } Exists { runAny = c2 { run = error "Recursive autocombime" (sequence . map (run c2)), commandType = commandType $ liftCommand c2, name = "Lifted(" ++ name c2 ++ ")" } }
              _ -> Left $ "Could not unify " ++ show e1 ++ " with " ++ show e2

commandR :: FunTy a b -> String -> (a -> CommandResult b) -> Command a b
commandR ty n f = Command { run = f, commandType = ty, name = n }

commandS :: (HasParamTy a, HasParamTy b) => String -> (a -> CommandResult b) -> Command a b
commandS = commandR (Specified defParamTy defParamTy)

command :: (HasParamTy a, HasParamTy b) => String -> (a -> b) -> Command a b
command n f = commandS n (pure . f)

-- Split command implementation.

splitCommand :: Char -> Command ByteString [ByteString]
splitCommand = command "Split" . C.split

-- Print command implementation.

printCommand :: ParamTy a -> Command a ByteString
printCommand inTy = Command { run = \st -> let prant = printTyped inTy st in writer (prant, [prant]), commandType = Specified inTy StringTy, name = "Print" }

printTyped :: ParamTy a -> a -> ByteString
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

printParser :: Parser (ParamTy a -> Command a ByteString)
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

-- This is a bad way to do this. I need a way to lift the runtime equality to type-level equality
combineCommands :: AnyCommand -> AnyCommand -> Either String AnyCommand
combineCommands a1@Exists{ runAny = f@Command { commandType = ct1 } } a2@Exists{ runAny = g@Command { commandType = ct2 } } = if UnFunTy ct1 `canCombineWith` UnFunTy ct2 then (Right . Exists) (composeCommands (error "combineCommands " f) (error "combineCommands g" g)) else Left $ "Could not unify " ++ show a1 ++ " with " ++ show a2

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Right Exists { runAny = Command { run = pure, commandType = IdLike, name = "Identity" } }
typecheckCommands (x:xs) = foldM autocombine x xs

canCombineWith :: UnFunTy -> UnFunTy -> Bool
canCombineWith (UnFunTy IdLike) _ = True
canCombineWith _ (UnFunTy IdLike) = True
canCombineWith (UnFunTy (Specified a b)) (UnFunTy (Specified c d)) = UnTy a == UnTy c && UnTy b == UnTy d
canCombineWith _ (UnFunTy (Constant _)) = True
canCombineWith (UnFunTy (Constant b)) (UnFunTy (Specified _ a)) = UnTy a == UnTy b

withProgramType :: AnyCommand -> Either String (Command ByteString ByteString)
withProgramType ac@Exists { runAny = c@Command { run = f, commandType = funTy, name = n } } = case funTyAny ac of
                        (UnFunTy (Specified at ot)) -> if
                          UnTy at == UnTy StringTy
                          then Right $ composeCommands (unsafeCoerce c) (printCommand ot)
                          else Left $ "Expected program to input type ByteString, found input type " ++ show at
                        (UnFunTy IdLike) -> Right Command { run = unsafeCoerce f, commandType = IdLike, name = n }
                        (UnFunTy (Constant ot)) -> Right $ composeCommands (unsafeCoerce c) (printCommand ot)

runCommand :: [AnyCommand] -> Either String (ByteString -> CommandResult ByteString)
runCommand [] = Left "No command!"
runCommand cmds = run <$> (typecheckCommands cmds >>= withProgramType)

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
