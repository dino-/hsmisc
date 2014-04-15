-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MonadOr
   ( tests )
   where

import Test.HUnit
   ( Test (..)
   , assertEqual
   )

import HsMisc.Control.Monad.MonadOr


instance MonadOr []


tests :: Test
tests = TestList
   [ testSuccess
   , testFailure
   ]


testSuccess :: Test
testSuccess = TestCase $
   assertEqual "List MonadOr instance success"
      "some value"
      ("some value" `morelse` "default value")


testFailure :: Test
testFailure = TestCase $
   assertEqual "List MonadOr instance failure"
      "default value"
      ("" `morelse` "default value")
