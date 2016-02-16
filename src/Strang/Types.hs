{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Safe,ExistentialQuantification #-}

module Strang.Types (ParamTy(..),AnyCommand(..),UnTy(..)
                    ,HasParamTy(..),Command(..),CommandResult
                    ,InputMode,runCommand,commandName) where

import Data.Text (Text)
import Control.Monad.Writer.Strict hiding (sequence)

 -- Strang has two builtin input modes: line mode and text mode.
type InputMode = IO [Text]

data ParamTy a where
    StringTy :: ParamTy Text
    ListTy :: ParamTy a -> ParamTy [a]

data UnTy = forall a. UnTy (ParamTy a)

instance Show UnTy where
  show (UnTy t) = show t

instance Eq UnTy where
  (==) (UnTy StringTy) (UnTy StringTy) = True
  (==) (UnTy (ListTy t)) (UnTy (ListTy y)) = UnTy t == UnTy y
  (==) _ _ = False

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

-- Command type. Basically a function between states, with type info and a log.
data Command i o where
  IdLike :: { runIdLike :: forall a. a -> CommandResult a
            , idLikeName :: String } -> Command i i
  Specified :: { runSpecified :: i -> CommandResult o
            , inTy :: ParamTy i
            , outTy :: ParamTy o
            , specifiedName :: String } -> Command i o

commandName :: Command i o -> String
commandName IdLike { idLikeName = n } = n
commandName Specified { specifiedName = n } = n

runCommand :: Command i o -> i -> CommandResult o
runCommand IdLike {runIdLike = f} = f
runCommand Specified {runSpecified = f} = f

-- Existential command.
data AnyCommand = forall a b. Exists { runAny :: Command a b }

-- TODO: Rewrite show instance prettier
instance Show AnyCommand where
  show Exists { runAny = c } = show c

instance Show (Command i o) where
  show IdLike {idLikeName = n} = n
  show Specified {specifiedName = n} = n
