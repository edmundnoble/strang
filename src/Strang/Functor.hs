
module Strang.Functor (putIn) where

  import Control.Arrow

  putIn :: Functor m => a -> m b -> m (a, b)
  putIn x = fmap (const x &&& id)
