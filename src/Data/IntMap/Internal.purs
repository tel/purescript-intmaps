
module Data.IntMap.Internal where

import Data.Function (Fn2 (), runFn2)
import Data.Int.Bits ((.^.), complement, (.&.), (.|.))
import Prelude

-- Newtypes
----------------------------------------------------------------------------

newtype Prefix = Prefix Int

instance prefixEq :: Eq Prefix where
  eq (Prefix a) (Prefix b) = eq a b

newtype Mask = Mask Int

instance maskEq :: Eq Mask where
  eq (Mask a) (Mask b) = eq a b

instance maskOrd :: Ord Mask where
  compare (Mask a) (Mask b) = compare a b


-- Bit twiddling
----------------------------------------------------------------------------

maskLonger :: Mask -> Mask -> Boolean
maskLonger m1 m2 = m1 < m2

prefixAsKey :: Prefix -> Int
prefixAsKey (Prefix p) = p

branchLeft :: Mask -> Int -> Boolean
branchLeft (Mask m) k = m .&. k == 0

matchPrefix :: Prefix -> Mask -> Int -> Boolean
matchPrefix p m k = mask m k == p

mask :: Mask -> Int -> Prefix
mask (Mask m) k = Prefix (_mask m k)

_mask :: Int -> Int -> Int
_mask m k = (k .|. (m-1)) .&. complement m

highestBit :: Int -> Int -> Int
highestBit x m = highb (x .&. complement (m - 1)) m where
  highb :: Int -> Int -> Int
  highb x m 
    | x == m = m
    | otherwise = highb (x .&. complement m) (2 * m)

branchingBit' :: Int -> Mask -> Int -> Mask -> Mask
branchingBit' k1 (Mask m1) k2 (Mask m2) = 
  Mask (highestBit (k1 .^. k2) (runFn2 max 1 (2 * runFn2 max m1 m2)))

branchingBit :: Int -> Int -> Mask
branchingBit k1 k2 = branchingBit' k1 (Mask 0) k2 (Mask 0)

foreign import dec2bin :: Int -> String
foreign import bin2dec :: String -> Int
foreign import pow :: Fn2 Int Int Int
foreign import max :: Fn2 Int Int Int
