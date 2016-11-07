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
{-# LANGUAGE FunctionalDependencies #-}

module Strang.Lists
  (HList(..),KList(..),FKList(..),kmap,fkmap,HLstClass(..)
  ,klistElim,forgetIn1,forgetOut1,forgetIn2,forgetOut2,(+|+),PlusPlus,fkcons,(~:~),liftK,toHlist) where

data HList (l :: [*]) where
  HNil :: HList '[]
  HCons :: a -> HList xs -> HList (a ': xs)

infixr 2 :-:

-- Higher-kinded list.
data KList (f :: k -> *) (l :: [k]) where
  KNil :: KList f '[]
  (:-:) :: forall (f :: k -> *) (a :: k) (as :: [k]). f a -> KList f as -> KList f (a ': as)

type family Map (f :: k -> *) (args :: [k]) :: [*] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

class LiftK (f :: * -> *) (args :: [*]) where
  type ExtractF f args :: [*]
  liftK :: HList args -> KList f (ExtractF f args)

instance LiftK f '[] where
  type ExtractF f '[] = '[]
  liftK HNil = KNil

instance LiftK f xs => LiftK f (f x ': xs) where
  type ExtractF f (f x ': xs) = x ': ExtractF f xs
  liftK (HCons fa hs) = fa :-: liftK hs

toHlist :: KList f args -> HList (Map f args)
toHlist KNil = HNil
toHlist (fa :-: kl) = HCons fa (toHlist kl)

-- Forgetful higher-kinded list. Mostly useful with GADTs.
data FKList (f :: k -> *) = forall args. FK { unFK :: KList f args }

fkcons :: forall (f :: k -> *) (a :: k). f a -> FKList f -> FKList f
fkcons fa (FK fs) = FK (fa :-: fs)

infixr 2 ~:~

(~:~) :: forall (f :: k -> *) (a :: k). f a -> FKList f -> FKList f
(~:~) = fkcons

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

-- Jeez. HLstClass converts any regular curried function into a function which
-- takes an HList (with the right types). Notice that the argument and result types are not uniquely
-- determined by the function type; instead the argument types and result type uniquely determine the function type.
-- That's because a function type like a -> b -> c has two possible sets of argument and result types:
-- args = '[a, b], res = c  OR  args = '[a], res = b -> c
class HLstClass (args :: [*]) (fun :: *) (res :: *) | args res -> fun where
  hlistEval :: fun -> HList args -> res

instance HLstClass '[] r r where
  hlistEval f _ = f

instance HLstClass xs f r => HLstClass (a ': xs) (a -> f) r where
  hlistEval a2f (HCons a xs) = hlistEval (a2f a) xs
