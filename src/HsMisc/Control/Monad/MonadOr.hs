-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module HsMisc.Control.Monad.MonadOr
   where

import Control.Monad


{- Inspired by the need to have mplus for List monad that behaves
   like the mplus implementation for Maybe
-}
class MonadPlus m => MonadOr m where
   morelse :: (Eq (m a)) => m a -> m a -> m a
   morelse x y
      | x == mzero = y
      | otherwise  = x

{- Can use like, let's say for lists:

   import MonadOr

   instance MonadOr []

   let x = ""
   let y = x `morelse` "default value"

   y == "default value"
-}
