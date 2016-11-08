{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Strang.Command (ParamTy(..),AnyCommand(..),HasParamTy(..),Command(..),NamedUnTy(..)
                     ,combineCommands,composeCommands,typecheckCommands,command,commandS
                     ,CommandResult,InputMode,eqTy,(:=:)(..),NamedParamTy(..),withProgramType
                     ,FunctionBinding(..),FFunctionBinding(..),TypedValue(..)
                     ,collapseCommands) where

import Strang.Exists(Exists(..))
import Strang.Iso(Iso(..),Uncurry(..))
import Strang.Lists(KList,HList)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Writer.Strict hiding (sequence)
import Text.Parsec.Error

orElse :: Either a b -> Either a b -> Either a b
l1@(Left _) `orElse` (Left _) = l1
(Left _) `orElse` r@(Right _) = r
(Right _) `orElse` r@(Right _) = r
r@(Right _) `orElse` (Left _) = r

type InputMode = IO [Text]

data ParamTy a where
    StringTy :: ParamTy Text
    ListTy :: ParamTy a -> ParamTy [a]

data (:=:) (a :: k) (b :: k) where
  Refl :: (a :=: a)

eqTy :: ParamTy a -> ParamTy b -> Maybe (a :=: b)
eqTy StringTy StringTy = Just (Refl :: Text :=: Text)
eqTy (ListTy t1) (ListTy t2) = (\Refl -> Refl) <$> eqTy t1 t2
eqTy StringTy _ = Nothing
eqTy (ListTy _) _ = Nothing

instance Show (ParamTy a) where
    show StringTy = "String"
    show (ListTy ty) = "List[" ++ show ty ++ "]"

class HasParamTy a where
  defParamTy :: ParamTy a

instance HasParamTy Text where
  defParamTy = StringTy

instance HasParamTy a => HasParamTy [a] where
  defParamTy = ListTy defParamTy

data TypedValue a = TypedValue (ParamTy a) a

data NamedUnTy = forall a. NamedUnTy (ParamTy a, Text)

type CommandResult r = Writer [Text] r

-- Command type.
data Command i o where
  Command :: { runCommand :: i -> CommandResult o
             , inTy :: ParamTy i
             , outTy :: ParamTy o
             , commandName :: String } -> Command i o

-- Existential commands.
data AnyCommand = forall a b. AnyCommand { runAny :: Command a b }
data CommandTaking i = forall o. CommandTaking (Command i o)

instance Iso (Exists (Uncurry Command)) AnyCommand where
  to (Exists (Uncurry c)) = AnyCommand c
  from (AnyCommand c) = Exists (Uncurry c)

instance Show AnyCommand where
  show AnyCommand { runAny = c } = show c

instance Show (Command i o) where
  show = commandName

data NamedParamTy a = NamedParamTy (ParamTy a) Text

instance Eq (NamedParamTy a) where
  (NamedParamTy _ n1) == (NamedParamTy _ n2) = n1 == n2

data FunctionBinding args = FunctionBinding { bindingName :: Text
                                            , bindingArguments :: KList NamedParamTy args
                                            , commandFromFunctionBinding :: HList args -> Either ParseError AnyCommand }

data FFunctionBinding = forall args. FFunctionBinding (FunctionBinding args)

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

-- Creates a command, takes HasParamTy evidence instead of an explicit type.
commandS :: (HasParamTy a, HasParamTy b) => String -> (a -> CommandResult b) -> Command a b
commandS n f = Command { runCommand = f, commandName = n, inTy = defParamTy, outTy = defParamTy }

-- Creates a command from a pure function.
command :: (HasParamTy a, HasParamTy b) => String -> (a -> b) -> Command a b
command n f = commandS n (pure . f)

-- Lifts a command into the List functor.
liftCommand :: Command i o -> Command [i] [o]
liftCommand Command { inTy = a, outTy = b, runCommand = f, commandName = n } =
  Command { runCommand = traverse f, inTy = ListTy a, outTy = ListTy b, commandName = "Lifted(" ++ n ++ ")" }

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Left "Empty program!"
typecheckCommands (x:xs) = foldM autocombine x xs

-- Nests a command into the List functor as deeply as is needed to combine it with another command.
-- Specifically, attempts to run commands at the highest possible
-- level in a nested list, and recurses through the levels if it fails.
-- This is resolved before runtime.
autocombine :: AnyCommand -> AnyCommand -> Either String AnyCommand
autocombine e1@AnyCommand { runAny = Command { outTy = (ListTy _) } } e2@AnyCommand { runAny = c2@Command {} } =
          combineCommands e1 e2 `orElse`
          autocombine e1 AnyCommand { runAny = liftCommand c2 }
autocombine e1@AnyCommand { runAny = Command {} } e2@AnyCommand { runAny = Command {} } = combineCommands e1 e2

withInputType :: forall i. ParamTy i -> AnyCommand -> Either String (CommandTaking i)
withInputType targetInTy AnyCommand { runAny = cmd } =
    case eqTy (inTy cmd) targetInTy of
        Just Refl -> Right (CommandTaking cmd)
        _ -> Left $ "Expected program to have input type " ++ show targetInTy ++
                    ", but it had input type " ++ show (inTy cmd)

withProgramType :: AnyCommand -> Either String (Command Text Text)
withProgramType c = do
  CommandTaking cmd <- withInputType StringTy c
  Right $ composeCommands cmd (printCommand (outTy cmd))

-- Print command implementation.
printCommand :: ParamTy a -> Command a Text
printCommand it = Command { runCommand = pure . printTyped . TypedValue it
                            , inTy = it
                            , outTy = StringTy
                            , commandName = "Print" }

-- Actual print implementation.
printTyped :: TypedValue a -> Text
printTyped (TypedValue StringTy str) = str
printTyped (TypedValue (ListTy t) ts) = T.pack "[" `T.append` T.intercalate (T.pack ",") (fmap (printTyped . TypedValue t) ts) `T.append` T.pack "]"

collapseCommands :: [AnyCommand] -> Either String (Text -> CommandResult Text)
collapseCommands [] = Left "Empty program!"
collapseCommands cmds = runCommand <$> (typecheckCommands cmds >>= withProgramType)
