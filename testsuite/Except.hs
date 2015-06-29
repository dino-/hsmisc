-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Except
   ( tests )
   where

import Control.Monad.Except
import Control.Monad.Identity
import Data.Map ( Map, fromList )
import Test.HUnit
   ( Test (..)
   , assertEqual
   )

import HsMisc.Control.Monad.Except


tests :: Test
tests = TestList
   [ testStrLookupPresent
   , testStrLookupMissing

   , testIntLookupPresent
   , testIntLookupMissing
   ]


someMap :: Map String Int
someMap = fromList [("foo", 42), ("bar", 11)]


testStrLookupPresent :: Test
testStrLookupPresent = TestCase $
   assertEqual "lookupE value present"
      (Right 42)
      (runIdentity $ runExceptT $ lookupE "foo" someMap)


testStrLookupMissing :: Test
testStrLookupMissing = TestCase $
   assertEqual "lookupE value missing"
      (Left "ERROR: key \"baz\" not found")
      (runIdentity $ runExceptT $ lookupE "baz" someMap)


anotherMap :: Map Int Int
anotherMap = fromList [(0, 42), (1, 11)]


testIntLookupPresent :: Test
testIntLookupPresent = TestCase $
   assertEqual "lookupEWith value present"
      (Right 42)
      (runIdentity $ runExceptT $ lookupEWith show 0 anotherMap)


testIntLookupMissing :: Test
testIntLookupMissing = TestCase $
   assertEqual "lookupEWith value missing"
      (Left "2")
      (runIdentity $ runExceptT $ lookupEWith show 2 anotherMap)
