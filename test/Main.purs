
module Test.Main where

import           Control.Monad.Aff.AVar (AVAR ())
import           Control.Monad.Eff
import           Control.Monad.Eff.Random (RANDOM ())
import           Prelude
import qualified Test.Data.IntMap       as IntMap
import           Test.Data.IntMap.Internal as IntMapInternal
import           Test.Unit              (TIMER (), runTest, test)
import           Test.Unit.Console      (TESTOUTPUT ())

main :: Eff (testOutput :: TESTOUTPUT, avar :: AVAR, timer :: TIMER, random :: RANDOM) Unit
main = runTest do
  IntMap.testAll
  IntMapInternal.testAll
