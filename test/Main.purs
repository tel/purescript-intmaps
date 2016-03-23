module Test.Main where

import Control.Monad.Aff.AVar (AVAR ())
import Control.Monad.Eff (Eff)
import Prelude (Unit)
import Test.Data.IntMap as IntMap
import Test.Unit (TIMER (), runTest, test)
import Test.Unit.Console (TESTOUTPUT ())

main :: Eff (testOutput :: TESTOUTPUT, avar :: AVAR, timer :: TIMER) Unit
main = runTest do
  test "IntMap" IntMap.tests
