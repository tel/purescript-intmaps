
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

  , null
  , size

  , empty
  , singleton
  , member
  , indices
  , values

  , lookup
  , lookupDefault

  , insert
  , insertWith
  , insertWithKey

  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey

  , unionWith
  , unionLeft
  , unionRight
  , unionWithKey

  , mapWithKey
  , foldMapWithKey
  , foldlWithKey
  , foldrWithKey
  , traverseWithKey

  , fromAssocArray
  , fromAssocArrayWith
  , fromAssocArrayWithKey
  , toAssocArray

  ) where

import           Data.Foldable        (Foldable, foldMap, foldl)
import           Data.IntMap.Internal
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable     (Traversable)
import           Data.Tuple           (Tuple (Tuple))
import           Prelude

-- Type definition (not exported)
-- ----------------------------------------------------------------------------

-- | `IntMap a` is the type of finite maps from integers to values at type `a`.
data IntMap a 
  = Empty
  | Lf Int a
  | Br Prefix Mask (IntMap a) (IntMap a)

-- Instance definitions
-- ----------------------------------------------------------------------------

instance intMapSemigroup :: (Semigroup a) => Semigroup (IntMap a) where
  append m1 m2 = unionWith append m1 m2

instance intMapMonoid :: (Semigroup a) => Monoid (IntMap a) where
  mempty = empty

instance intMapFunctor :: Functor IntMap where
  map f = mapWithKey (\_ -> f)

instance intMapFoldable :: Foldable IntMap where
  foldMap f = foldMapWithKey (\_ -> f)
  foldr f = foldrWithKey (\_ -> f)
  foldl f = foldlWithKey (\_ -> f)

instance intMapTraversable :: Traversable IntMap where
  traverse f = traverseWithKey (\_ -> f)
  sequence = traverseWithKey (\_ -> id)

instance intMapEq :: (Eq a) => Eq (IntMap a) where
  eq Empty Empty = true
  eq (Lf k1 v1) (Lf k2 v2) = eq k1 k2 && eq v1 v2
  eq (Br p1 m1 l1 r1) (Br p2 m2 l2 r2) =
    eq m1 m2 && eq p1 p2 && eq l1 l2 && eq r1 r2
  eq _ _ = false

-- Public API
-- ----------------------------------------------------------------------------

-- | The empty `IntMap`
empty :: forall a . IntMap a
empty = Empty

-- | An `IntMap` of a single value.
singleton :: forall a . Int -> a -> IntMap a
singleton k a = Lf k a

-- | Is a given key in the map?
member :: forall a . Int -> IntMap a -> Boolean
member k m = 
  case lookup k m of
    Nothing -> false
    Just _ -> true

-- | If a value is available in an `IntMap` at a given tree then `lookup`
-- | will return it. Otherwise, `Nothing`.
lookup :: forall a . Int -> IntMap a -> Maybe a
lookup _ Empty = Nothing
lookup k (Lf here v)
  | k == here = Just v
  | otherwise = Nothing
lookup k (Br prefix m l r)
  | not (matchPrefix prefix m k) = Nothing
  | branchLeft m k = lookup k l
  | otherwise = lookup k r

-- | Like `lookup` but returning a default value if not available in the `IntMap`
lookupDefault :: forall a . Int -> a -> IntMap a -> a
lookupDefault k d m = 
  case lookup k m of
    Nothing -> d
    Just a -> a

-- | Update an `IntMap` by ensuring that a given value exists at a given 
-- | key such that for any `IntMap` `m` and integer `k`, 
-- |
-- |   lookup k (insert k a) = Just a
-- |
insert :: forall a . Int -> a -> IntMap a -> IntMap a
insert = insertWithKey (\_ _ a -> a)

-- | Like `insert` but if the value already exists in the `IntMap` then it is
-- | combined with the new one using a splatting function. The first argument is
-- | the previous value if it exists and the second the new one.
-- |
-- |     lookup k (insertWith s k a (insert k b m)) = Just (s b a)
-- |
insertWith :: forall a . (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWith splat = insertWithKey (\_ -> splat)

-- | Like `insertWith` but the splatting function also has access to the 
-- | map key where the conflict arose.
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

-- | /O(min(n,W))/. Delete a key and its value from map. When the key is not
-- | a member of the map, the original map is returned.
delete :: forall a. Int -> IntMap a -> IntMap a
delete k t = go t where
  go t =
    case t of
      Empty -> Empty
      Lf ky _
        | k==ky -> Empty
        | otherwise -> t
      Br p m l r
        | not (matchPrefix p m k) -> t
        | branchLeft m k -> br p m (go l) r
        | otherwise -> br p m l (go r)

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- | a member of the map, the original map is returned.
adjust :: forall a. (a -> a) -> Int -> IntMap a -> IntMap a
adjust f = adjustWithKey (\_ x -> f x)

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- | a member of the map, the original map is returned.
adjustWithKey :: forall a. (Int -> a -> a) -> Int -> IntMap a -> IntMap a
adjustWithKey f = updateWithKey (\k' x -> Just $ f k' x)

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- | at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- | deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: forall a. (a -> Maybe a) -> Int -> IntMap a -> IntMap a
update f = updateWithKey (\_ x -> f x)

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- | at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- | deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
updateWithKey :: forall a. (Int -> a -> Maybe a) -> Int -> IntMap a -> IntMap a
updateWithKey f k t = go t where
  go t =
    case t of
      Empty -> Empty
      Lf ky y
        | k == ky ->
          case f k y of
            Just y' -> Lf ky y'
            Nothing -> Empty
        | otherwise -> t
      Br p m l r
        | not (matchPrefix p m k) -> t
        | branchLeft m k -> br p m (go l) r
        | otherwise -> br p m l (go r)

-- | Unions two `IntMap`s together using a splatting function. If 
-- | a key is present in both constituent lists then the resulting 
-- | list will be the splat of the values from each constituent. If the key
-- | was available in only one constituent then it is available unmodified 
-- | in the result.
unionWith :: forall a . (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith splat = unionWithKey (\_ -> splat)

-- | Like `unionWith` but where values from the left constituent always override
-- | values from the right.
unionLeft :: forall a . IntMap a -> IntMap a -> IntMap a
unionLeft = unionWithKey (\_ a _ -> a)

-- | Like `unionWith` but where values from the right constituent always override
-- | values from the left.
unionRight :: forall a . IntMap a -> IntMap a -> IntMap a
unionRight = unionWithKey (\_ _ a -> a)

-- | Like `unionWith` but where the splatting function has access to all of the
-- | keys where conflicts arise.
unionWithKey :: forall a . (Int -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey splat = go where
  go Empty r = r
  go l Empty = l
  go (Lf k a) r = insertWithKey splat k a r
  go l (Lf k a) = insertWithKey (\k a b -> splat k b a) k a l
  go l@(Br l_p l_m l_l l_r) r@(Br r_p r_m r_l r_r)

    -- the prefixes are identical, we'll union symmetrically
    | l_m == r_m && l_p == r_p =
      Br l_p l_m (go l_l r_l) (go l_r r_r)

    -- the left mask is longer and the right prefix is a subsequence of the left
    -- thus, the right tree is more specific and should be uniond with some
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

-- | Construct an `IntMap` from an associative array from integer keys to values
fromAssocArray :: forall a . Array (Tuple Int a) -> IntMap a
fromAssocArray = foldl (\m (Tuple k v) -> insert k v m) empty

-- | Construct an `IntMap` from an associative array from integer keys to values
fromAssocArrayWith :: forall a . (a -> a -> a) -> Array (Tuple Int a) -> IntMap a
fromAssocArrayWith f = foldl (\m (Tuple k v) -> insertWith f k v m) empty

-- | Construct an `IntMap` from an associative array from integer keys to values
fromAssocArrayWithKey :: forall a . (Int -> a -> a -> a) -> Array (Tuple Int a) -> IntMap a
fromAssocArrayWithKey f = foldl (\m (Tuple k v) -> insertWithKey f k v m) empty

-- | Convert an `IntMap` to an equivalent associative array.
toAssocArray :: forall a . IntMap a -> Array (Tuple Int a)
toAssocArray = foldMapWithKey (\k v -> pure (Tuple k v))

-- | Gather all of the indicies stored in an `IntMap`
indices :: forall a . IntMap a -> Array Int
indices = foldMapWithKey (\k _ -> pure k)

-- | Gather all of the values stored in an `IntMap`
values :: forall a . IntMap a -> Array a
values = foldMap pure

-- | A version of `foldMap` which provides key values during the mapping.
foldMapWithKey :: forall a m . (Monoid m) => (Int -> a -> m) -> IntMap a -> m
foldMapWithKey f = go where
  go Empty = mempty
  go (Lf k x) = f k x
  go (Br _ _ l r) = go l <> go r

-- | A version of `foldl` which provides key values during the mapping.
foldlWithKey :: forall a b. (Int -> b -> a -> b) -> b -> IntMap a -> b
foldlWithKey f = go where
  go z Empty = z
  go z (Lf k a) = f k z a
  go z (Br _ _ l r) = go (go z l) r

-- | A version of `foldr` which provides key values during the mapping.
foldrWithKey :: forall a b. (Int -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f = go where
  go z Empty = z
  go z (Lf k a) = f k a z
  go z (Br _ _ l r) = go (go z r) l

-- | Checks whether an `IntMap` contains any values at all.
null :: forall a . IntMap a -> Boolean
null Empty = true
null _ = false

-- | Count the number of values in the `IntMap`
size :: forall a . IntMap a -> Int
size = foldl (\c _ -> 1 + c) 0

traverseWithKey :: forall a t b . (Applicative t) => (Int -> a -> t b) -> IntMap a -> t (IntMap b)
traverseWithKey inj = go where
  go Empty = pure Empty
  go (Lf k x) = Lf k <$> inj k x
  go (Br p m l r) = Br p m <$> go l <*> go r

-- Private functions
-- ----------------------------------------------------------------------------

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
