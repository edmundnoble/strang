{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification #-}

module Strang.Commands(splitCommand,printCommand,joinCommand,collapseCommands,leftMap,makeReplaceRegexCommand) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.ICU hiding (ParseError,span)
import qualified Data.Text.ICU as I (span,find)
import Control.Monad.Writer.Strict hiding (sequence)
import Unsafe.Coerce
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
liftCommand cmd@Specified { inTy = a, outTy = b, runSpecified = f } = cmd { runSpecified = traverse f, inTy = ListTy a, outTy = ListTy b }
liftCommand IdLike { idLikeName = n, runIdLike = f } = IdLike { runIdLike = f, idLikeName = "Lifted(" ++ n ++ ")" }

-- Makes a command map over lists, in the shallowest way possible.
-- Specifically, attempts to run commands at the highest possible
-- level in a nested list, and recurses through the levels if it fails.
-- This is resolved before runtime.
autocombine :: AnyCommand -> AnyCommand -> Either String AnyCommand
autocombine e1@Exists {runAny = IdLike {}} e2 = combineCommands e1 e2
autocombine e1 e2@Exists {runAny = IdLike {}} = combineCommands e1 e2
autocombine e1@Exists { runAny = Specified { outTy = (ListTy _) } } e2@Exists { runAny = c2@Specified {} } =
          combineCommands e1 e2 `orElse`
          autocombine e1 Exists { runAny = liftCommand c2 }
autocombine e1@Exists { runAny = Specified {} } e2@Exists { runAny = Specified {} } = combineCommands e1 e2

-- Command with fully specified type, that takes HasParamTy evidence instead of an explicit type.
commandS :: (HasParamTy a, HasParamTy b) => String -> (a -> CommandResult b) -> Command a b
commandS n f = Specified { runSpecified = f, specifiedName = n, inTy = defParamTy, outTy = defParamTy }

-- Command with fully specified type, outside of the CommandResult monad.
command :: (HasParamTy a, HasParamTy b) => String -> (a -> b) -> Command a b
command n f = commandS n (pure . f)

-- Split command implementation.
splitCommand :: Text -> Command Text [Text]
splitCommand t = command "Split" $ T.splitOn t

-- Print command implementation.
printCommand :: ParamTy a -> Command a Text
printCommand it = Specified { runSpecified = \st -> let p = printTyped it st in writer (p, [p])
                            , inTy = it
                            , outTy = StringTy
                            , specifiedName = "Print" }

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
composeCommands c1@IdLike {runIdLike = f} c2@IdLike {runIdLike = g} = IdLike { runIdLike = f >=> g, idLikeName = composeNames c1 c2 }
composeCommands c1@IdLike {runIdLike = f} c2@Specified {runSpecified = g} = c2 { runSpecified = f >=> g, specifiedName = composeNames c1 c2 }
composeCommands c1@Specified {runSpecified = f} c2@IdLike {runIdLike = g} = c1 { runSpecified = f >=> g, specifiedName = composeNames c1 c2 }
composeCommands c1@Specified {runSpecified = f} c2@Specified {runSpecified = g} =
  Specified { runSpecified = f >=> g, specifiedName = composeNames c1 c2, inTy = inTy c1, outTy = outTy c2 }

-- I would really like to remove the unsafeCoerce call here.
-- I'll need a way to recover existential type equality from value equality.
combineCommands :: AnyCommand -> AnyCommand -> Either String AnyCommand
combineCommands Exists {runAny = c1@IdLike {runIdLike = f}} Exists { runAny = c2@IdLike {runIdLike = g} } =
  (Right . Exists) IdLike { runIdLike = f >=> g, idLikeName = composeNames c1 c2 }
combineCommands Exists {runAny = c1@IdLike {runIdLike = f}} Exists { runAny = c2@Specified {runSpecified = g, inTy = it, outTy = ot} } =
  (Right . Exists) Specified { runSpecified = f >=> g, specifiedName = composeNames c1 c2, inTy = it, outTy = ot }
combineCommands Exists {runAny = c1@Specified {runSpecified = f, inTy = it, outTy = ot}} Exists {runAny = c2@IdLike {runIdLike = g}}=
  (Right . Exists) Specified { runSpecified = f >=> g, specifiedName = composeNames c1 c2, inTy = it, outTy = ot }
combineCommands Exists {runAny = c1@Specified {runSpecified = f, inTy = it1, outTy = ot1}}
                Exists {runAny = c2@Specified {runSpecified = g, inTy = it2, outTy = ot2}}
                  | UnTy ot1 == UnTy it2 =
                    (Right . Exists) Specified { runSpecified = f >=> unsafeCoerce g, specifiedName = composeNames c1 c2, inTy = it1, outTy = ot2 }
                  | otherwise = Left $ "Could not unify " ++ show c1 ++ " with " ++ show c2

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Right Exists { runAny = IdLike { runIdLike = pure, idLikeName = "Identity" } }
typecheckCommands (x:xs) = foldM autocombine x xs

withProgramType :: AnyCommand -> Either String (Command Text Text)
withProgramType Exists { runAny = IdLike { runIdLike = f, idLikeName = n } } = Right IdLike { runIdLike = f, idLikeName = n }
withProgramType Exists { runAny = c@Specified { inTy = StringTy, outTy = ot } } = Right $ composeCommands c (printCommand ot)
withProgramType Exists { runAny = Specified { inTy = ct } } = Left $ "Expected program to have input type Text, found input type " ++ show ct

collapseCommands :: [AnyCommand] -> Either String (Text -> CommandResult Text)
collapseCommands [] = Left "No command!"
collapseCommands cmds = runCommand <$> (typecheckCommands cmds >>= withProgramType)
