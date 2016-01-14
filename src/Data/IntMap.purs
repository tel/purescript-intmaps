
-- | An efficient implementation of purely functional maps from integer keys to
-- values. To be imported qualified.
-- 
-- Based on the Haskell strict IntMap implementation which is based on
-- "big-endian patricia trees".
--
-- - <https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-IntMap-Strict.html>
-- - Chris Okasaki and Andy Gill, "Fast Mergeable Integer Maps", 
--   - Workshop on ML, September 1998, pages 77-86
--   - <http://citeseer.ist.psu.edu/okasaki98fast.html>

module Data.IntMap ( 

    IntMap ()

  , empty
  , singleton

  , lookup

  , insert
  , insertWith
  , insertWithKey

  , mergeWith
  , mergeLeft
  , mergeRight
  , mergeWithKey

  , mapWithKey

  ) where

import           Data.Foldable        (Foldable)
import           Data.IntMap.Internal
import           Data.Maybe
import           Data.Monoid
import           Prelude

-- Type definition (not exported)
----------------------------------------------------------------------------

-- | `IntMap a` is the type of finite maps from integers to values at type `a`.
data IntMap a 
  = Empty
  | Lf Int a
  | Br Prefix Mask (IntMap a) (IntMap a)

instance intMapSemigroup :: (Semigroup a) => Semigroup (IntMap a) where
  append m1 m2 = mergeWith append m1 m2

instance intMapMonoid :: (Semigroup a) => Monoid (IntMap a) where
  mempty = empty

instance intMapFunctor :: Functor IntMap where
  map f = mapWithKey (\_ -> f)

instance intMapFoldable :: Foldable IntMap where
  foldMap = foldMap_
  foldr = foldr_
  foldl = foldl_

instance intMapEq :: (Eq a) => Eq (IntMap a) where
  eq Empty Empty = true
  eq (Lf k1 v1) (Lf k2 v2) = eq k1 k2 && eq v1 v2
  eq (Br p1 m1 l1 r1) (Br p2 m2 l2 r2) =
    eq m1 m2 && eq p1 p2 && eq l1 l2 && eq r1 r2
  eq _ _ = false

-- Public API
----------------------------------------------------------------------------

-- | The empty `IntMap`
empty :: forall a . IntMap a
empty = Empty

-- | An `IntMap` of a single value.
singleton :: forall a . Int -> a -> IntMap a
singleton k a = Lf k a

-- | If a value is available in an `IntMap` at a given tree then `lookup`
-- will return it. Otherwise, `Nothing`.
lookup :: forall a . Int -> IntMap a -> Maybe a
lookup _ Empty = Nothing
lookup k (Lf here v)
  | k == here = Just v
  | otherwise = Nothing
lookup k (Br prefix m l r)
  | not (matchPrefix prefix m k) = Nothing
  | branchLeft m k = lookup k l
  | otherwise = lookup k r

-- | Update an `IntMap` by ensuring that a given value exists at a given 
-- key such that for any `IntMap` `m` and integer `k`, 
--
--     lookup k (insert k a) = Just a
--
insert :: forall a . Int -> a -> IntMap a -> IntMap a
insert = insertWithKey (\_ a _ -> a) 

-- | Like `insert` but if the value already exists in the `IntMap` then it is
-- combined with the new one using a splatting function. The first argument is
-- the previous value if it exists and the second the new one.
--
--     lookup k (insertWith s k a (insert k b m)) = Just (s b a)
--
insertWith :: forall a . (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWith splat = insertWithKey (\_ -> splat)

-- | Like `insertWith` but the splatting function also has access to the 
-- map key where the conflict arose.
insertWithKey :: forall a . (Int -> a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWithKey splat k a t = go t where 
  go t =
    case t of
      Empty -> Lf k a
      Lf k0 a0
        | k0 == k -> Lf k0 (splat k a0 a) -- same key, merge with splat
        | otherwise -> join k (Mask 0) (Lf k a) k0 (Mask 0) t
      Br p m l r
        | matchPrefix p m k -> 
          if branchLeft m k 
             then Br p m (go l) r
             else Br p m l (go r)
        | otherwise -> join k (Mask 0) (Lf k a) (prefixAsKey p) m t

-- | Merges two `IntMap`s together using a splatting function. If 
-- a key is present in both constituent lists then the resulting 
-- list will be the splat of the values from each constituent. If the key
-- was available in only one constituent then it is available unmodified 
-- in the result.
mergeWith :: forall a . (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
mergeWith splat = mergeWithKey (\_ -> splat)

-- | Like `mergeWith` but where values from the left constituent always override
-- values from the right.
mergeLeft :: forall a . IntMap a -> IntMap a -> IntMap a
mergeLeft = mergeWithKey (\_ a _ -> a)

-- | Like `mergeWith` but where values from the right constituent always override
-- values from the left.
mergeRight :: forall a . IntMap a -> IntMap a -> IntMap a
mergeRight = mergeWithKey (\_ _ a -> a)

-- | Like `mergeWith` but where the splatting function has access to all of the
-- keys where conflicts arise.
mergeWithKey :: forall a . (Int -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
mergeWithKey splat = go where
  go Empty r = r
  go l Empty = l
  go (Lf k a) r = insertWithKey splat k a r
  go l (Lf k a) = insertWithKey (\k a b -> splat k b a) k a l
  go l@(Br l_p l_m l_l l_r) r@(Br r_p r_m r_l r_r)

    -- the prefixes are identical, we'll merge symmetrically
    | l_m == r_m && l_p == r_p =
      Br l_p l_m (go l_l r_l) (go l_r r_r)

    -- the left mask is longer and the right prefix is a subsequence of the left
    -- thus, the right tree is more specific and should be merged with some
    -- subtree of the left tree
    | maskLonger l_m r_m && matchPrefix l_p l_m (prefixAsKey r_p) =
      if branchLeft l_m (prefixAsKey r_p)
         then Br l_p l_m (go l_l r) l_r
         else Br l_p l_m l_l (go l_r r)

    -- opposite of last case
    | maskLonger r_m l_m && matchPrefix r_p r_m (prefixAsKey l_p) =
      if branchLeft r_m (prefixAsKey l_p)
         then Br r_p r_m (go l r_l) r_r
         else Br r_p r_m r_l (go l r_r)

    -- the prefixes disagree entirely, we'll make a new branch point
    | otherwise =
      join
        (prefixAsKey l_p) l_m l
        (prefixAsKey r_p) r_m r

-- | Transform all of the values in the map.
mapWithKey :: forall a b . (Int -> a -> b) -> IntMap a -> IntMap b
mapWithKey f = go where
  go m = 
    case m of
      Empty -> Empty
      Lf k a -> Lf k (f k a)
      Br p m l r -> Br p m (go l) (go r)

-- Private functions
----------------------------------------------------------------------------

foldMap_ :: forall a m . (Monoid m) => (a -> m) -> IntMap a -> m
foldMap_ f = go where
  go Empty = mempty
  go (Lf _ x) = f x
  go (Br _ _ l r) = go l <> go r

foldl_ :: forall a b. (b -> a -> b) -> b -> IntMap a -> b
foldl_ f = go where
  go z Empty = z
  go z (Lf _ a) = f z a
  go z (Br _ _ l r) = go (go z l) r

foldr_ :: forall a b. (a -> b -> b) -> b -> IntMap a -> b
foldr_ f = go where
  go z Empty = z
  go z (Lf _ a) = f a z
  go z (Br _ _ l r) = go (go z r) l

-- | Smart branch constructor. Compresses empty trees away.
br :: forall a . Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
br _ _ Empty Empty = Empty
br _ _ Empty t = t
br _ _ t Empty = t
br p m t1 t2 = Br p m t1 t2

-- | *Invariant*, both IntMaps must be non-empty.
join :: forall a . Int -> Mask -> IntMap a -> Int -> Mask -> IntMap a -> IntMap a
join k1 m1 t1 k2 m2 t2 =
  let m = branchingBit' k1 m1 k2 m2
   in if branchLeft m k1
         then Br (mask m k1) m t1 t2
         else Br (mask m k1) m t2 t1
