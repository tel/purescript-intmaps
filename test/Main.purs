module Test.Main where

import Control.Monad.Aff.AVar (AVAR ())
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM ())
import Prelude (Unit, bind)
import Test.Unit.Console (TESTOUTPUT ())
import Test.Unit.Main (runTest)

import Test.Data.IntMap as IntMap
import Test.Data.IntMap.Internal as IntMapInternal

main :: Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, random :: RANDOM) Unit
main = runTest do
  IntMap.testAll
  IntMapInternal.testAll
