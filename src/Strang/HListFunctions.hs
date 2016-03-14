{-# LANGUAGE DataKinds,
             PolyKinds,
             MultiParamTypeClasses,
             TypeOperators,
             FlexibleContexts,
             ExplicitForAll,
             FunctionalDependencies,
             UndecidableInstances,
             TypeFamilies #-}

module Strang.HListFunctions (HLstClass(..)) where
import Strang.Command

-- Jeez. HLstClass converts any regular curried function into a function which
-- takes an HList (with the right types). Notice that the argument and result types are not uniquely
-- determined by the function type; instead the argument types and result type uniquely determine the function type.
-- That's because a function type like a -> b -> c has two possible sets of argument and result types:
-- args = '[a, b], res = c  OR  args = '[a], res = b -> c
class HLstClass (args :: [*]) (fun :: *) (res :: *) | args res -> fun where
  hlistEval :: fun -> HList args ->  res

instance HLstClass '[] r r where
  hlistEval f _ = f

instance HLstClass xs f r => HLstClass (a ': xs) (a -> f) r where
  hlistEval a2f (HCons a xs) = hlistEval (a2f a) xs
