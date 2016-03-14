{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,ExistentialQuantification,ConstraintKinds #-}

module Strang.Command (ParamTy(..),AnyCommand(..),HasParamTy(..),Command(..),NamedUnTy(..)
                     ,combineCommands,composeCommands,typecheckCommands,command,commandS,fkcons,(-:-)
                     ,CommandResult,InputMode,eqTy,(:=:)(..),HList(..),KList(..),FKList(..),NamedParamTy(..)
                     ,FunctionBinding(..),FFunctionBinding(..),kmap,fkmap,TypedValue(..),klistElim,forgetIn1,forgetOut1
                     ,forgetIn2,forgetOut2,(+|+),PlusPlus,collapseCommands) where

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

data (:=:) (a :: *) (b :: *) where
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

-- Existential command.
data AnyCommand = forall a b. AnyCommand { runAny :: Command a b }

instance Show AnyCommand where
  show AnyCommand { runAny = c } = show c

instance Show (Command i o) where
  show = commandName

infixr 2 :-:

data HList (l :: [*]) where
  HNil :: HList '[]
  HCons :: a -> HList xs -> HList (a ': xs)

-- Higher-kinded list.
data KList (f :: k -> *) (l :: [k]) where
  KNil :: KList f '[]
  (:-:) :: forall (f :: k -> *) (a :: k) (as :: [k]). f a -> KList f as -> KList f (a ': as)

-- Forgetful higher-kinded list. Mostly useful with GADTs.
data FKList (f :: k -> *) = forall args. FK { unFK :: KList f args }

fkcons :: forall (f :: k -> *) (a :: k). f a -> FKList f -> FKList f
fkcons fa (FK fs) = FK (fa :-: fs)

infixr 2 -:-

(-:-) :: forall (f :: k -> *) (a :: k). f a -> FKList f -> FKList f
(-:-) = fkcons

type family PlusPlus (a :: [k]) (b :: [k]) :: [k] where
  PlusPlus as '[] = as
  PlusPlus as (b ': bs) = b ': (PlusPlus as bs)

forgetIn1 :: forall f. (forall a. KList f a -> FKList f) -> FKList f -> FKList f
forgetIn1 f (FK l) = f l

forgetOut1 :: forall f a b. (KList f a -> KList f b) -> KList f a -> FKList f
forgetOut1 f l = FK (f l)

forgetIn2 :: forall f. (forall a b. KList f a -> KList f b -> FKList f) -> FKList f -> FKList f -> FKList f
forgetIn2 f (FK fs) (FK gs) = f fs gs

forgetOut2 :: forall f a b c. (KList f a -> KList f b -> KList f c) -> KList f a -> KList f b -> FKList f
forgetOut2 f = (.) FK . f

(+|+) :: forall f args args2. KList f args -> KList f args2 -> KList f (args `PlusPlus` args2)
fs +|+ KNil = fs
fs +|+ (k :-: ks) = k :-: (fs +|+ ks)

klistElim :: (forall a. f a -> s) -> KList f args -> [s]
klistElim _ KNil = []
klistElim f (fa :-: ks) = f fa : klistElim f ks

-- Higher-kinded map.
kmap :: (forall a. f a -> g a) -> KList f args -> KList g args
kmap _ KNil = KNil
kmap nt (fa :-: kl) = nt fa :-: kmap nt kl

-- Higher-kinded forgetful map.
fkmap :: (forall a. f a -> g a) -> FKList f -> FKList g
fkmap nt (FK kl) = FK (kmap nt kl)

data NamedParamTy a = NamedParamTy (ParamTy a) Text

data FunctionBinding args = FunctionBinding {
               bindingName :: Text
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

withProgramType :: AnyCommand -> Either String (Command Text Text)
withProgramType AnyCommand { runAny = c@Command { inTy = StringTy, outTy = ot } } = Right $ composeCommands c (printCommand ot)
withProgramType AnyCommand { runAny = Command { inTy = ct } } = Left $ "Expected program to have input type Text, found input type " ++ show ct

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

collapseCommands :: [AnyCommand] -> Either String (Text -> CommandResult Text)
collapseCommands [] = Left "Empty program!"
collapseCommands cmds = runCommand <$> (typecheckCommands cmds >>= withProgramType)
