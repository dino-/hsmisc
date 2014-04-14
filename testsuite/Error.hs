-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Error
   ( tests )
   where

import Control.Monad.Error
import Control.Monad.Identity
import Data.Map ( Map, fromList )
import Test.HUnit
   ( Test (..)
   , assertEqual
   )

import HsMisc.Control.Monad.Error


tests :: Test
tests = TestList
   [ testLookupPresent
   , testLookupMissing
   ]


someMap :: Map String Int
someMap = fromList [("foo", 42), ("bar", 11)]


testLookupPresent :: Test
testLookupPresent = TestCase $
   assertEqual "lookupE value present" (Right 42)
      (runIdentity $ runErrorT $ lookupE "foo" someMap)


testLookupMissing :: Test
testLookupMissing = TestCase $
   assertEqual "lookupE value missing"
      (Left "ERROR: key \"baz\" not found")
      (runIdentity $ runErrorT $ lookupE "baz" someMap)
