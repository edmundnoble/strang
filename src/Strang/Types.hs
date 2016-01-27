{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds ,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Safe,ExistentialQuantification #-}

module Strang.Types (ParamTy(..),FunTy(..),UnTy(..),UnFunTy(..),HasParamTy(..),HasFunTy(..),Command(..),CommandResult) where

import Data.ByteString (ByteString)
import Control.Monad.Writer.Strict hiding (sequence)

data ParamTy a where
    StringTy :: ParamTy ByteString
    ListTy :: ParamTy a -> ParamTy [a]

data FunTy a b where
    Specified :: ParamTy a -> ParamTy b -> FunTy a b
    Constant :: ParamTy b -> FunTy a b
    IdLike :: forall a. FunTy a a

data UnTy = forall a. UnTy (ParamTy a)
data UnFunTy = forall a b. UnFunTy (FunTy a b)

instance Show UnTy where
  show (UnTy t) = "ANON: " ++ show t

instance Show UnFunTy where
  show (UnFunTy t) = "ANON: " ++ show t

instance Show (FunTy a b) where
  show (Specified a b) = "S (" ++ show a ++ " -> " ++ show b ++ ")"
  show (Constant b) = "S (a -> " ++ show b ++ ")"
  show IdLike = "S (a -> a)"

instance Eq UnTy where
  (==) (UnTy StringTy) (UnTy StringTy) = True
  (==) (UnTy (ListTy t)) (UnTy (ListTy y)) = UnTy t == UnTy y
  (==) _ _ = False

instance Eq UnFunTy where
  (UnFunTy IdLike) == (UnFunTy IdLike) = True
  (UnFunTy (Constant a)) == (UnFunTy (Constant b)) = UnTy a == UnTy b
  (UnFunTy (Specified a b)) == (UnFunTy (Specified c d)) = (UnTy a == UnTy c) && (UnTy b == UnTy d)
  (UnFunTy _) == (UnFunTy _) = False

instance Show (ParamTy a) where
    show StringTy = "String"
    show (ListTy ty) = "List[" ++ show ty ++ "]"

class HasParamTy a where
  defParamTy :: ParamTy a

class HasFunTy a b where
  defFunTy :: FunTy a b

instance HasParamTy ByteString where
  defParamTy = StringTy

instance HasParamTy a => HasParamTy [a] where
  defParamTy = ListTy defParamTy

instance (HasParamTy a, HasParamTy b) => HasFunTy a b where
  defFunTy = Specified defParamTy defParamTy

type CommandResult r = Writer [ByteString] r

-- Command type. Basically a function between states, with runtime type info and a log.
data Command i o = Command { run     :: i -> CommandResult o
                           , commandType :: FunTy i o
                           , name :: String }
