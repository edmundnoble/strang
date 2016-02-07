{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification #-}

module Strang.Commands(splitCommand, printCommand, joinCommand, collapseCommands,leftMap,makeReplaceRegexCommand) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.ICU hiding (ParseError,find,span)
import qualified Data.Text.ICU as I (span)
import Control.Monad.Writer.Strict hiding (sequence)
import Unsafe.Coerce
import Strang.Types
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right

-- Interpreter state type. Note the recursion in ListState, which is used below
-- in `cata` to support arbitrarily-nested command mapping.
liftCommand :: Command i o -> Command [i] [o]
liftCommand cmd@Command { commandType = Specified a b, runCommand = f } = cmd { runCommand = traverse f, commandType = Specified (ListTy a) (ListTy b) }
liftCommand cmd@Command { commandType = IdLike } = cmd { commandType = IdLike, runCommand = traverse $ runCommand cmd }
liftCommand cmd@Command { commandType = Constant a, runCommand = f } = cmd { runCommand = traverse f, commandType = Constant (ListTy a) }

-- Compose function types!
-- IdLike is an identity. (Constant _) is a right zero.
composeFunTy :: FunTy a b -> FunTy b c -> FunTy a c
composeFunTy _ (Constant t) = Constant t
composeFunTy (Constant _) (Specified _ c) = Constant c
composeFunTy (Specified a _) (Specified _ c) = Specified a c
composeFunTy IdLike a = a
composeFunTy a IdLike = a

-- Makes a command map over lists, in the shallowest way possible.
-- Specifically, attempts to run commands at the highest possible
-- level in a nested list, and recurses through the levels if it fails.
-- This is resolved before runtime.
autocombine :: AnyCommand -> AnyCommand -> Either String AnyCommand
autocombine e1 e2@Exists { runAny = c2 } = let (ct1, ct2) = (funTyAny e1, funTyAny e2) in
          if canCombineWith ct1 ct2 then combineCommands e1 e2
          else case ct1 of
              UnFunTy (Specified _ (ListTy _)) -> autocombine e1 Exists { runAny = c2 { runCommand = traverse (runCommand c2), commandType = commandType $ liftCommand c2, commandName = "Lifted(" ++ commandName c2 ++ ")" } }
              _ -> Left $ "Could not unify " ++ show e1 ++ " with " ++ show e2

-- Command with any type, name, and implementation.
commandR :: FunTy a b -> String -> (a -> CommandResult b) -> Command a b
commandR ty n f = Command { runCommand = f, commandType = ty, commandName = n }

-- Command with fully specified type, that takes HasParamTy evidence instead of an explicit type.
commandS :: (HasParamTy a, HasParamTy b) => String -> (a -> CommandResult b) -> Command a b
commandS = commandR (Specified defParamTy defParamTy)

-- Command with fully specified type, outside of the CommandResult monad.
command :: (HasParamTy a, HasParamTy b) => String -> (a -> b) -> Command a b
command n f = commandS n (pure . f)

-- Split command implementation.
splitCommand :: Char -> Command Text [Text]
splitCommand ch = command "Split" $ T.split (== ch)

-- Print command implementation.
printCommand :: ParamTy a -> Command a Text
printCommand inTy = Command { runCommand = \st -> let p = printTyped inTy st in writer (p, [p])
                            , commandType = Specified inTy StringTy
                            , commandName = "Print" }

-- Actual print implementation.
printTyped :: ParamTy a -> a -> Text
printTyped StringTy str = str
printTyped (ListTy t) ts = T.pack "[" `T.append` T.intercalate (T.pack ",") (fmap (printTyped t) ts) `T.append` T.pack "]"

-- Join command implementation.
joinCommand :: Text -> Command [Text] Text
joinCommand = command "Join" . T.intercalate

replaceRegex :: Regex -> Text -> Text -> Text
replaceRegex regexFind replace input = case findAll regexFind input of
                                    [] -> input -- short circuit
                                    xs -> foldr (\m t -> I.span m <> replace <> t) T.empty xs -- whoop

regexCommand :: Regex -> Text -> Command Text Text
regexCommand regexFind regexReplace = command "ReplaceRegex" $ replaceRegex regexFind regexReplace

apply :: Functor f => a -> f (a -> b) -> f b
apply a = fmap (\f -> f a)

-- Regex replacement command.
makeReplaceRegexCommand :: Text -> Text -> Either ParseError (Command Text Text)
makeReplaceRegexCommand regexFind replace = let withStringErr = apply replace (regexCommand <$> regex' [] regexFind) in
                           leftMap (flip newErrorMessage (initialPos "") . Message . show) withStringErr

composeCommands :: Command a b -> Command b c -> Command a c
composeCommands ab bc = Command { commandType = composeFunTy (commandType ab) (commandType bc)
                                , runCommand = runCommand ab >=> runCommand bc
                                , commandName = "(" ++ commandName bc ++ " . " ++ commandName ab ++ ")" }

-- This is a bad way to do this. I need a way to lift the runtime equality to type-level equality
combineCommands :: AnyCommand -> AnyCommand -> Either String AnyCommand
combineCommands a1@Exists{ runAny = f@Command { commandType = ct1 } } a2@Exists{ runAny = g@Command { commandType = ct2 } } = if UnFunTy ct1 `canCombineWith` UnFunTy ct2 then (Right . Exists) (composeCommands (unsafeCoerce f) (unsafeCoerce g)) else Left $ "Could not unify " ++ show a1 ++ " with " ++ show a2

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Right Exists { runAny = Command { runCommand = pure, commandType = IdLike, commandName = "Identity" } }
typecheckCommands (x:xs) = foldM autocombine x xs

-- Returns true iff Commands with the two given UnFunTys could be combined with combineCommands.
canCombineWith :: UnFunTy -> UnFunTy -> Bool
canCombineWith (UnFunTy IdLike) _ = True
canCombineWith _ (UnFunTy IdLike) = True
canCombineWith (UnFunTy (Specified _ b)) (UnFunTy (Specified c _)) = UnTy b == UnTy c
canCombineWith _ (UnFunTy (Constant _)) = True
canCombineWith (UnFunTy (Constant b)) (UnFunTy (Specified _ a)) = UnTy a == UnTy b

withProgramType :: AnyCommand -> Either String (Command Text Text)
withProgramType ac@Exists { runAny = c@Command { runCommand = f } } = case funTyAny ac of
                        (UnFunTy (Specified at ot)) -> if
                          UnTy at == UnTy StringTy -- can I make this introduce an evidence term?
                          then Right $ composeCommands (unsafeCoerce c) (printCommand ot)
                          else Left $ "Expected program to have input type Text, found input type " ++ show at
                        (UnFunTy IdLike) -> Right c { runCommand = unsafeCoerce f, commandType = IdLike }
                        (UnFunTy (Constant ot)) -> Right $ composeCommands (unsafeCoerce c) (printCommand ot)

collapseCommands :: [AnyCommand] -> Either String (Text -> CommandResult Text)
collapseCommands [] = Left "No command!"
collapseCommands cmds = runCommand <$> (typecheckCommands cmds >>= withProgramType)
