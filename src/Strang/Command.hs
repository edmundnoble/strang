{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Safe                      #-}
{-# LANGUAGE TypeOperators             #-}

module Strang.Command (ParamTy(..),AnyCommand(..),HasParamTy(..),Command(..),Binding(..),
                     combineCommands,composeCommands,typecheckCommands,command,commandS,CommandResult,OutputMode(..),output,eqTy,(:=:)(..)) where

import           Control.Monad.Writer.Strict hiding (sequence)
import           Data.Text                   (Text)

orElse :: Either a b -> Either a b -> Either a b
l1@(Left _) `orElse` (Left _) = l1
(Left _) `orElse` r@(Right _) = r
(Right _) `orElse` r@(Right _) = r
r@(Right _) `orElse` (Left _) = r

data OutputMode a = OutputMode (ParamTy a) (a -> Text)

output :: HasParamTy a => (a -> Text) -> OutputMode a
output = OutputMode defParamTy

data ParamTy a where
    StringTy :: ParamTy Text
    ListTy :: ParamTy a -> ParamTy [a]

data (a :=: b) where
  Refl :: (a :=: a)

eqTy :: ParamTy a -> ParamTy b -> Maybe (a :=: b)
eqTy StringTy StringTy = Just Refl
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

type CommandResult r = Writer [Text] r

-- Command type.
data Command i o where
  Command :: { runCommand :: i -> CommandResult o
             , inTy :: ParamTy i
             , outTy :: ParamTy o
             , commandName :: String } -> Command i o

-- Existential command.
data AnyCommand = forall a b. AnyCommand { runAny :: Command a b }

instance Show AnyCommand where
  show AnyCommand { runAny = c } = show c

instance Show (Command i o) where
  show = commandName

data Binding = Binding { bindingName        :: String
                       , commandFromBinding :: AnyCommand }

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


-- Command with fully Command type, that takes HasParamTy evidence instead of an explicit type.
commandS :: (HasParamTy a, HasParamTy b) => String -> (a -> CommandResult b) -> Command a b
commandS n f = Command { runCommand = f, commandName = n, inTy = defParamTy, outTy = defParamTy }

-- Command with fully Command type, outside of the CommandResult monad.
command :: (HasParamTy a, HasParamTy b) => String -> (a -> b) -> Command a b
command n f = commandS n (pure . f)

-- Interpreter state type. Note the recursion in ListState, which is used below
-- in `cata` to support arbitrarily-nested command mapping.
liftCommand :: Command i o -> Command [i] [o]
liftCommand cmd@Command { inTy = a, outTy = b, runCommand = f } = cmd { runCommand = traverse f, inTy = ListTy a, outTy = ListTy b }

typecheckCommands :: [AnyCommand] -> Either String AnyCommand
typecheckCommands [] = Left "Empty program!"
typecheckCommands (x:xs) = foldM autocombine x xs

-- Makes a command map over lists, in the shallowest way possible.
-- Specifically, attempts to run commands at the highest possible
-- level in a nested list, and recurses through the levels if it fails.
-- This is resolved before runtime.
autocombine :: AnyCommand -> AnyCommand -> Either String AnyCommand
autocombine e1@AnyCommand { runAny = Command { outTy = (ListTy _) } } e2@AnyCommand { runAny = c2@Command {} } =
          combineCommands e1 e2 `orElse`
          autocombine e1 AnyCommand { runAny = liftCommand c2 }
autocombine e1@AnyCommand { runAny = Command {} } e2@AnyCommand { runAny = Command {} } = combineCommands e1 e2
