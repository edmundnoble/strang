{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification #-}

module Strang.Commands(splitCommand,printCommand,joinCommand,collapseCommands,leftMap,makeReplaceRegexCommand,idLike) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.ICU hiding (ParseError,span)
import qualified Data.Text.ICU as I (span,find)
import Control.Monad.Writer.Strict hiding (sequence)
import Strang.Types
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)
import Data.Maybe

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right

orElse :: Either a b -> Either a b -> Either a b
l1@(Left _) `orElse` (Left _) = l1
(Left _) `orElse` r@(Right _) = r
(Right _) `orElse` r@(Right _) = r
r@(Right _) `orElse` (Left _) = r

-- Interpreter state type. Note the recursion in ListState, which is used below
-- in `cata` to support arbitrarily-nested command mapping.
liftCommand :: Command i o -> Command [i] [o]
liftCommand cmd@Command { inTy = a, outTy = b, runCommand = f } = cmd { runCommand = traverse f, inTy = ListTy a, outTy = ListTy b }

-- Helper function for polymorphic functions.
idLike :: (forall a. a -> CommandResult a) -> String -> ParamTy i -> Command i i
idLike f n ty = Command { runCommand = f, inTy = ty, outTy = ty, commandName = n  }

-- Makes a command map over lists, in the shallowest way possible.
-- Specifically, attempts to run commands at the highest possible
-- level in a nested list, and recurses through the levels if it fails.
-- This is resolved before runtime.
autocombine :: AnyCommand -> AnyCommand -> Either String AnyCommand
autocombine e1@AnyCommand { runAny = Command { outTy = (ListTy _) } } e2@AnyCommand { runAny = c2@Command {} } =
          combineCommands e1 e2 `orElse`
          autocombine e1 AnyCommand { runAny = liftCommand c2 }
autocombine e1@AnyCommand { runAny = Command {} } e2@AnyCommand { runAny = Command {} } = combineCommands e1 e2

-- Command with fully Command type, that takes HasParamTy evidence instead of an explicit type.
commandS :: (HasParamTy a, HasParamTy b) => String -> (a -> CommandResult b) -> Command a b
commandS n f = Command { runCommand = f, commandName = n, inTy = defParamTy, outTy = defParamTy }

-- Command with fully Command type, outside of the CommandResult monad.
command :: (HasParamTy a, HasParamTy b) => String -> (a -> b) -> Command a b
command n f = commandS n (pure . f)

-- Split command implementation.
splitCommand :: Text -> Command Text [Text]
splitCommand t = command "Split" $ T.splitOn t

-- Print command implementation.
printCommand :: ParamTy a -> Command a Text
printCommand it = Command { runCommand = \st -> let p = printTyped it st in writer (p, [p])
                            , inTy = it
                            , outTy = StringTy
                            , commandName = "Print" }

-- Actual print implementation.
printTyped :: ParamTy a -> a -> Text
printTyped StringTy str = str
printTyped (ListTy t) ts = T.pack "[" `T.append` T.intercalate (T.pack ",") (fmap (printTyped t) ts) `T.append` T.pack "]"

-- Join command implementation.
joinCommand :: Text -> Command [Text] Text
joinCommand = command "Join" . T.intercalate

replaceRegex :: Regex -> Text -> Text -> Text
replaceRegex regexFind replace input = case I.find regexFind input of
                                    Nothing -> input -- short circuit
                                    Just x -> I.span x <> replace <> fromMaybe T.empty (replaceRegex regexFind replace <$> suffix 0 x)

regexCommand :: Regex -> Text -> Command Text Text
regexCommand regexFind regexReplace = command "ReplaceRegex" $ replaceRegex regexFind regexReplace

apply :: Functor f => a -> f (a -> b) -> f b
apply a = fmap (\f -> f a)

-- Regex replacement command.
makeReplaceRegexCommand :: Text -> Text -> Either ParseError (Command Text Text)
makeReplaceRegexCommand regexFind replace = let withStringErr = apply replace (regexCommand <$> regex' [] regexFind) in
                           leftMap (flip newErrorMessage (initialPos "") . Message . show) withStringErr

composeNames :: Command i1 o1 -> Command i2 o2 -> String
composeNames f g = commandName f ++ " . " ++ commandName g

composeCommands :: Command a b -> Command b c -> Command a c
composeCommands c1@Command {runCommand = f} c2@Command {runCommand = g} =
  Command { runCommand = f >=> g, commandName = composeNames c1 c2, inTy = inTy c1, outTy = outTy c2 }

-- Compose commands, or fail if we can't.
combineCommands :: AnyCommand -> AnyCommand -> Either String AnyCommand
combineCommands AnyCommand {runAny = c1@Command {outTy = ot1}}
                AnyCommand {runAny = c2@Command {inTy = it2}} =
                  case eqTy ot1 it2 of
                    Just Refl ->
                      (Right . AnyCommand) (composeCommands c1 c2)
                    Nothing -> Left $ "Could not unify " ++ show c1 ++ " with " ++ show c2

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Left "Empty program!"
typecheckCommands (x:xs) = foldM autocombine x xs

withProgramType :: AnyCommand -> Either String (Command Text Text)
withProgramType AnyCommand { runAny = c@Command { inTy = StringTy, outTy = ot } } = Right $ composeCommands c (printCommand ot)
withProgramType AnyCommand { runAny = Command { inTy = ct } } = Left $ "Expected program to have input type Text, found input type " ++ show ct

collapseCommands :: [AnyCommand] -> Either String (Text -> CommandResult Text)
collapseCommands [] = Left "No command!"
collapseCommands cmds = runCommand <$> (typecheckCommands cmds >>= withProgramType)
