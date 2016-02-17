{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Safe,ExistentialQuantification #-}

module Strang.Types (ParamTy(..),AnyCommand(..),UnTy(..)
                    ,HasParamTy(..),Command(..),CommandResult
                    ,InputMode) where

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

-- Command type.
data Command i o where
  Command :: { runCommand :: i -> CommandResult o
             , inTy :: ParamTy i
             , outTy :: ParamTy o
             , commandName :: String } -> Command i o

idLike :: HasParamTy i => (forall a. a -> CommandResult a) -> String -> Command i i
idLike f n = Command { runCommand = f, inTy = defParamTy, outTy = defParamTy, commandName = n  }

-- Existential command.
data AnyCommand = forall a b. AnyCommand { runAny :: Command a b }

-- TODO: Rewrite show instance prettier
instance Show AnyCommand where
  show AnyCommand { runAny = c } = show c

instance Show (Command i o) where
  show = commandName
