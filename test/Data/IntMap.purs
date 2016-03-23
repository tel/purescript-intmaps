
module Test.Data.IntMap where

import           Data.Foldable             (foldMap, foldr, foldl)
import Data.Array ((:))
import           Data.IntMap
import           Data.Maybe
import Data.Tuple (Tuple(Tuple))
import           Prelude
import qualified Test.Data.IntMap.Internal as Internal
import           Test.Unit                 (Test (), test)
import           Test.Unit.Assert          as Assert
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

(>|) :: forall a b . a -> (a -> b) -> b
(>|) a f = f a

ex0 :: IntMap Int
ex0 = empty 

ex1 :: IntMap Int
ex1 = singleton 0 1234

ex2 :: IntMap Int
ex2 = empty
     >| insert 10 10
     >| insert 20 20
     >| insert 30 30
     >| insert 0  1234

testAll = test "Data.IntMap" do
  test "Unit Tests" tests
  test "QuickCheck" props

tests = do
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
    test "filter by key" $
      Assert.equal (delete 20 ex2) (filterWithKey (\i _ -> i /= 20) ex2)

props = do
  test "insert then delete identity"
    $ quickCheck \(TIntMap m) k s -> delete k (insert k s m) === m
  test "insert then lookup hits"
    $ quickCheck \(TIntMap m) k s -> lookup k (insert k s m) === Just s
  test "delete then lookup misses"
    $ quickCheck \(TIntMap m) k -> lookup k (delete k m) === Nothing
  test "filter - keep all"
    $ quickCheck \(TIntMap m) -> filter (const true) m === m
  test "filter - drop all"
    $ quickCheck \(TIntMap m) -> filter (const false) m === empty

newtype TIntMap = TIntMap (IntMap String)

runTIntMap :: TIntMap -> IntMap String
runTIntMap (TIntMap t) = t

instance arbitraryTIntMap :: Arbitrary TIntMap where
  arbitrary = (TIntMap <<< fromAssocArray) <$> arbitrary

instance showTIntMap :: Show TIntMap where
  show (TIntMap t) = show t
