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

module Strang.Iso(Iso(..),Curry(..),Uncurry(..)) where

newtype Curry (f :: (k1, k2) -> *) (a :: k1) (b :: k2) :: * where
  Curry :: { runCurry :: f '(a, b) } -> Curry f a b

data Uncurry (f :: k1 -> k2 -> *) (t :: (k1, k2)) :: * where
  Uncurry :: { runUncurry :: f a b } -> Uncurry f '(a, b)


class Iso (f :: *) (g :: *) where
  to :: f -> g
  from :: g -> f

instance Iso (f i o) (Uncurry f '(i, o)) where
  to = Uncurry
  from = runUncurry

instance Iso (f '(i, o)) (Curry f i o) where
  to = Curry
  from = runCurry

instance (Functor f, Iso a b) => Iso (f a) (f b) where
  to = fmap to
  from = fmap from
