module Test.Main where

import Control.Monad.Aff.AVar (AVAR ())
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM ())
import Prelude (Unit, bind)
import Test.Unit (TIMER (), runTest)
import Test.Unit.Console (TESTOUTPUT ())

import Test.Data.IntMap as IntMap
import Test.Data.IntMap.Internal as IntMapInternal

main :: Eff (testOutput :: TESTOUTPUT, avar :: AVAR, timer :: TIMER, random :: RANDOM) Unit
main = runTest do
  IntMap.testAll
  IntMapInternal.testAll
