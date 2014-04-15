-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   The MonadPlus instance for Maybe works like a logical or:

   > (Just x) `mplus` _ == Just x
   > Nothing  `mplus` y == y

   But the MonadPlus instance for List works like concatination:

   > [1, 2, 3] `mplus` [10, 20] == [1, 2, 3, 10, 20]
   > [] `mplus` [1, 2, 3]       == [1, 2, 3]

   I ran into a situation where I wanted the former behavior
   for lists, success and failure:

   > [1, 2, 3] `morelse` [10, 20] == [1, 2, 3]
   > [] `morelse` [10, 20]        == [10, 20]

   Perhaps to express a default value:

   > let x = ""
   > let y = x `morelse` "default value"
   > y == "default value"

   To fulfill that need, this module implements a MonadOr typeclass
   with morelse
-}
module HsMisc.Control.Monad.MonadOr
   where

import Control.Monad


{- |
   Example of use for lists:

   > import HsMisc.Control.Monad.MonadOr
   > instance MonadOr []
-}
class MonadPlus m => MonadOr m where
   morelse :: (Eq (m a)) => m a -> m a -> m a
   morelse x y
      | x == mzero = y
      | otherwise  = x
