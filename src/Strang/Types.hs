{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Safe,ExistentialQuantification #-}

module Strang.Types (ParamTy(..),AnyCommand(..),HasParamTy(..),Command(..)
                    ,CommandResult,InputMode,eqTy,(:=:)(..)) where

import Data.Text (Text)
import Control.Monad.Writer.Strict hiding (sequence)

 -- Strang has two builtin input modes: line mode and text mode.
type InputMode = IO [Text]

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

-- TODO: Rewrite show instance prettier
instance Show AnyCommand where
  show AnyCommand { runAny = c } = show c

instance Show (Command i o) where
  show = commandName
