{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds ,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification,TemplateHaskell #-}

module Strang.Interpreter (interpretProgram) where

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
import Debug.Trace
import Strang.Types

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right

 -- Strang has two builtin input modes: line mode and text mode.

type InputMode = IO [ByteString]

-- A Strang command is a series of characters.
-- It starts with a mode character, 'l' for line mode or 't' for text mode.
modeParser :: Parser InputMode
modeParser = consumingParser <|> pure (pure <$> BS.getLine) where
                consumingParser = try (char 'l' $> (pure <$> BS.getLine)) <|> try (char 't' $> (pure <$> BS.getContents))

-- This is for passing strings to commands.
stringArg :: Parser ByteString
stringArg = C.pack <$> surroundedBy (char '"') anyChar where
                surroundedBy q p = q *> manyTill p (try q)

-- This is for passing single characters to commands.

charArg :: Parser Char
charArg = anyChar


-- Interpreter state type. Note the recursion in ListState, which is used below
-- in `cata` to support arbitrarily-nested commands.
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
              UnFunTy (Specified _ (ListTy _)) -> trace ("Recursively calling autocombine to unify " ++ show e1 ++ " with " ++ show e2) $ autocombine Exists { runAny = c1 } Exists { runAny = c2 { run = trace "Recursive autocombine" (sequence . map (run c2)), commandType = commandType $ liftCommand c2, name = "Lifted(" ++ name c2 ++ ")" } }
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

regexCommand :: Regex -> Command ByteString [ByteString]
regexCommand = command "Regex" . matchRegex

matchRegex :: Regex -> ByteString -> [ByteString]
matchRegex reg str = head $ match reg str

-- Split command parser. Syntax is:
-- s<char>

splitParser :: Parser (Command ByteString [ByteString])
splitParser = splitCommand <$> (char 's' *> charArg)

-- Print command parser.
-- Syntax:   <other commands>p
-- Appends the current value to the log, converted to a string.

printParser :: Parser (ParamTy a -> Command a ByteString)
printParser = char 'p' $> printCommand

-- Join command parser.
-- Syntax:    <other commands>j"delim"
-- Joins the elements of a list of strings, with a delimiter.

joinParser :: Parser (Command [ByteString] ByteString)
joinParser = joinCommand <$> (char 'j' *> stringArg)

-- Direction switch command parser.

directionSwitchParser :: Parser ()
directionSwitchParser = void $ char '<'

-- Index command parser. Indexes into the farthest outer liststate.

indexParser :: Parser a
indexParser = error "index parser not implemented"

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
  show Exists { runAny = c@Command { commandType = ct } } = name c ++ " :: (" ++ show ct ++ ")"

funTyAny :: AnyCommand -> UnFunTy
funTyAny Exists { runAny = c } = (UnFunTy . commandType) c

--bracketedCommandParser :: Parser [AnyCommand]
--bracketedCommandParser = char '(' *> (many1 commandParser <|> (join <$> many1 bracketedCommandParser)) <* char ')'

programParser :: Parser (InputMode, [AnyCommand])
programParser = do
  m <- modeParser
  cs <- many1 commandParser
  eof
  return (m, cs)

composeCommands :: Command a b -> Command b c -> Command a c
composeCommands ab bc = Command { commandType = composeFunTy (commandType ab) (commandType bc)
                                , run = run ab >=> run bc
                                , name = "(" ++ name bc ++ " . " ++ name ab ++ ")" }

-- This is a bad way to do this. I need a way to lift the runtime equality to type-level equality
combineCommands :: AnyCommand -> AnyCommand -> Either String AnyCommand
combineCommands a1@Exists{ runAny = f@Command { commandType = ct1 } } a2@Exists{ runAny = g@Command { commandType = ct2 } } = if UnFunTy ct1 `canCombineWith` UnFunTy ct2 then (Right . Exists) (composeCommands (unsafeCoerce f) (unsafeCoerce g)) else Left $ "Could not unify " ++ show a1 ++ " with " ++ show a2

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Right Exists { runAny = Command { run = pure, commandType = IdLike, name = "Identity" } }
typecheckCommands (x:xs) = foldM autocombine x xs

canCombineWith :: UnFunTy -> UnFunTy -> Bool
canCombineWith (UnFunTy IdLike) _ = True
canCombineWith _ (UnFunTy IdLike) = True
canCombineWith (UnFunTy (Specified _ b)) (UnFunTy (Specified c _)) = UnTy b == UnTy c
canCombineWith _ (UnFunTy (Constant _)) = True
canCombineWith (UnFunTy (Constant b)) (UnFunTy (Specified _ a)) = UnTy a == UnTy b

withProgramType :: AnyCommand -> Either String (Command ByteString ByteString)
withProgramType ac@Exists { runAny = c@Command { run = f } } = case funTyAny ac of
                        (UnFunTy (Specified at ot)) -> if
                          UnTy at == UnTy StringTy
                          then Right $ composeCommands (unsafeCoerce c) (printCommand ot)
                          else Left $ "Expected program to have input type ByteString, found input type " ++ show at
                        (UnFunTy IdLike) -> Right c { run = unsafeCoerce f, commandType = IdLike }
                        (UnFunTy (Constant ot)) -> Right $ composeCommands (unsafeCoerce c) (printCommand ot)

runCommand :: [AnyCommand] -> Either String (ByteString -> CommandResult ByteString)
runCommand [] = Left "No command!"
runCommand cmds = run <$> (typecheckCommands cmds >>= withProgramType)

printProgramResult :: Show a => CommandResult a -> IO ()
printProgramResult = print

printParsingError :: ParseError -> IO ()
printParsingError = print

putIn :: Functor m => a -> m b -> m (a, b)
putIn x = fmap (const x &&& id)

parseProgram :: ByteString -> Either String (InputMode, [AnyCommand])
parseProgram pgm = leftMap show (parse programParser "" pgm)

interpretProgram :: ByteString -> IO ()
interpretProgram cmd = let programAndModeOrErr = parseProgram cmd
                           compiledProgramOrErr :: Either String (InputMode, ByteString -> ByteString)
                           compiledProgramOrErr = programAndModeOrErr >>= (\(m, c) -> putIn m ((\f -> foldl' BS.append BS.empty . f) <$> runCommand c))
                           errorHandling err = C.putStrLn $ C.pack $ "Compilation error: " ++ err
                           in either errorHandling (\(mode, cmd) -> mode >>= (sequence_ . fmap (C.putStrLn . cmd))) compiledProgramOrErr
