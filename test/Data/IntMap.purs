module Test.Data.IntMap where

import Data.Foldable (foldMap, foldr, foldl)
import Data.Array ((:))
import           Data.IntMap
import           Data.Maybe
import           Prelude
import qualified Test.Data.IntMap.Internal as Internal
import           Test.Unit                 (Test (), test)
import           Test.Unit.Assert          as Assert
import Data.Tuple (Tuple(Tuple))

import Data.IntMap

ex0 :: IntMap Int
ex0 = empty 

ex1 :: IntMap Int
ex1 = singleton 0 1234

ex2 :: IntMap Int
ex2 = empty
      # insert 10 10
      # insert 20 20
      # insert 30 30
      # insert 0  1234

tests :: Test ()
tests = do
  test "Data.IntMap" do 
    test "lookup in empty map" $ Assert.equal Nothing (lookup 0 ex0)
    test "lookup in singleton map" $ Assert.equal (Just 1234) (lookup 0 ex1)
    test "lookup in compound map" $ Assert.equal (Just 1234) (lookup 0 ex2)
    test "sorted foldMap" $ Assert.equal [1234, 10, 20, 30] (foldMap pure ex2)
    test "sorted foldr" $ Assert.equal [1234, 10, 20, 30] (foldr (\a as -> a:as) [] ex2)
    test "sorted foldl" $ Assert.equal [30, 20, 10, 1234] (foldl (\as a -> a:as) [] ex2)
    test "simple eqs" do
      Assert.assert "ex0 == ex0" (eq ex0 ex0)
      Assert.assert "ex1 == ex1" (eq ex1 ex1)
      Assert.assert "ex2 == ex2" (eq ex2 ex2)
      Assert.assert "ex0 /= ex1" (ex0 /= ex1)
      Assert.assert "ex1 /= ex2" (ex1 /= ex2)
      Assert.assert "ex2 /= ex0" (ex2 /= ex0)
    test "insert overlaps" $ Assert.equal (Just 3) (lookup 0 (insert 0 3 ex1))
    test "filter none" $
      Assert.equal ex2 (filter (const true) ex2)
    test "filter all" $
      Assert.equal empty (filter (const false) ex2)
    test "filter by key" $
      Assert.equal (delete 20 ex2) (filterWithKey (\i _ -> i /= 20) ex2)
    testAlter
    Internal.tests

testAlter :: Test ()
testAlter = do
  test "alter" do
    test "adding" do
       Assert.equal (singleton 1 10) (alterIns 10 1 empty)
       Assert.equal (singleton 1 10) (alterIns 10 1 (singleton 1 10))
       Assert.equal (fromAssocArray [Tuple 1 10, Tuple 2 20, Tuple 3 30])
         (alterIns 20 2 (fromAssocArray [Tuple 1 10, Tuple 3 30]))
    test "deleting" do
      Assert.equal empty (alterDel 1 empty)
      Assert.equal empty (alterDel 1 (singleton 1 10))
      Assert.equal (singleton 2 20) (alterDel 1 (singleton 2 20))
      Assert.equal (singleton 2 20) (alterDel 1 (fromAssocArray [Tuple 1 10, Tuple 2 20]))
    test "updating" do
      Assert.equal empty (alterUpd 1 empty)
      Assert.equal (singleton 2 20) (alterUpd 1 (singleton 2 20))
      Assert.equal (fromAssocArray [Tuple 1 10, Tuple 2 21, Tuple 3 30])
        (alterUpd 2 (fromAssocArray [Tuple 1 10, Tuple 2 20, Tuple 3 30]))
  where
    alterIns a = alter (maybe (Just a) Just)
    alterDel   = alter (const Nothing)
    alterUpd   = alter (map (_ + 1))
