## Module Data.IntMap

An efficient implementation of purely functional maps from integer keys to

#### `IntMap`

``` purescript
data IntMap a
```

`IntMap a` is the type of finite maps from integers to values at type `a`.

##### Instances
``` purescript
(Semigroup a) => Semigroup (IntMap a)
(Semigroup a) => Monoid (IntMap a)
Functor IntMap
Foldable IntMap
Traversable IntMap
(Eq a) => Eq (IntMap a)
(Show a) => Show (IntMap a)
```

#### `empty`

``` purescript
empty :: forall a. IntMap a
```

The empty `IntMap`

#### `singleton`

``` purescript
singleton :: forall a. Int -> a -> IntMap a
```

An `IntMap` of a single value.

#### `member`

``` purescript
member :: forall a. Int -> IntMap a -> Boolean
```

Is a given key in the map?

#### `lookup`

``` purescript
lookup :: forall a. Int -> IntMap a -> Maybe a
```

If a value is available in an `IntMap` at a given tree then `lookup`
will return it. Otherwise, `Nothing`.

#### `lookupDefault`

``` purescript
lookupDefault :: forall a. Int -> a -> IntMap a -> a
```

Like `lookup` but returning a default value if not available in the `IntMap`

#### `insert`

``` purescript
insert :: forall a. Int -> a -> IntMap a -> IntMap a
```

Update an `IntMap` by ensuring that a given value exists at a given
key such that for any `IntMap` `m` and integer `k`,

  lookup k (insert k a) = Just a


#### `insertWith`

``` purescript
insertWith :: forall a. (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
```

Like `insert` but if the value already exists in the `IntMap` then it is
combined with the new one using a splatting function. The first argument is
the previous value if it exists and the second the new one.

    lookup k (insertWith s k a (insert k b m)) = Just (s b a)


#### `insertWithKey`

``` purescript
insertWithKey :: forall a. (Int -> a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
```

Like `insertWith` but the splatting function also has access to the
map key where the conflict arose.

#### `delete`

``` purescript
delete :: forall a. Int -> IntMap a -> IntMap a
```

/O(min(n,W))/. Delete a key and its value from map. When the key is not
a member of the map, the original map is returned.

#### `adjust`

``` purescript
adjust :: forall a. (a -> a) -> Int -> IntMap a -> IntMap a
```

/O(min(n,W))/. Adjust a value at a specific key. When the key is not
a member of the map, the original map is returned.

#### `adjustWithKey`

``` purescript
adjustWithKey :: forall a. (Int -> a -> a) -> Int -> IntMap a -> IntMap a
```

/O(min(n,W))/. Adjust a value at a specific key. When the key is not
a member of the map, the original map is returned.

#### `update`

``` purescript
update :: forall a. (a -> Maybe a) -> Int -> IntMap a -> IntMap a
```

/O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.

#### `updateWithKey`

``` purescript
updateWithKey :: forall a. (Int -> a -> Maybe a) -> Int -> IntMap a -> IntMap a
```

/O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.

#### `alter`

``` purescript
alter :: forall a. (Maybe a -> Maybe a) -> Int -> IntMap a -> IntMap a
```

/O(min(n,W))/. The expresion (@'alter' f k m@) alters the value @x@
at key @k@, or absence thereof.
'alter' can be used to insert, delete, or update the value under given
key in the 'IntMap'.
The following property holds:
@'lookup' k ('alter' f k m) = f ('lookup' k m)@.

#### `difference`

``` purescript
difference :: forall a b. IntMap a -> IntMap b -> IntMap a
```

/O(n+m)/. Difference between two maps (based on keys).

#### `differenceWith`

``` purescript
differenceWith :: forall a b. (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
```

/O(n+m)/. Difference with a combining function.

#### `differenceWithKey`

``` purescript
differenceWithKey :: forall a b. (Int -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
```

/O(n+m)/. Difference with a combining function. When two equal keys
are encountered, the combining function is applied to the key and
both values. If it returns 'Nothing', the elements is discarded.
If it returns (@'Just' y@), the element is updated with a new value @y@.

#### `unionWith`

``` purescript
unionWith :: forall a. (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
```

Unions two `IntMap`s together using a splatting function. If
a key is present in both constituent lists then the resulting
list will be the splat of the values from each constituent. If the key
was available in only one constituent then it is available unmodified
in the result.

#### `unionLeft`

``` purescript
unionLeft :: forall a. IntMap a -> IntMap a -> IntMap a
```

Like `unionWith` but where values from the left constituent always override
values from the right.

#### `unionRight`

``` purescript
unionRight :: forall a. IntMap a -> IntMap a -> IntMap a
```

Like `unionWith` but where values from the right constituent always override
values from the left.

#### `unionWithKey`

``` purescript
unionWithKey :: forall a. (Int -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
```

Like `unionWith` but where the splatting function has access to all of the
keys where conflicts arise.

#### `mergeWithKey`

``` purescript
mergeWithKey :: forall a b c. (Int -> a -> b -> Maybe c) -> (IntMap a -> IntMap c) -> (IntMap b -> IntMap c) -> IntMap a -> IntMap b -> IntMap c
```

#### `mapWithKey`

``` purescript
mapWithKey :: forall a b. (Int -> a -> b) -> IntMap a -> IntMap b
```

Transform all of the values in the map.

#### `fromAssocArray`

``` purescript
fromAssocArray :: forall a. Array (Tuple Int a) -> IntMap a
```

Construct an `IntMap` from an associative array from integer keys to values

#### `fromAssocArrayWith`

``` purescript
fromAssocArrayWith :: forall a. (a -> a -> a) -> Array (Tuple Int a) -> IntMap a
```

Construct an `IntMap` from an associative array from integer keys to values

#### `fromAssocArrayWithKey`

``` purescript
fromAssocArrayWithKey :: forall a. (Int -> a -> a -> a) -> Array (Tuple Int a) -> IntMap a
```

Construct an `IntMap` from an associative array from integer keys to values

#### `toAssocArray`

``` purescript
toAssocArray :: forall a. IntMap a -> Array (Tuple Int a)
```

Convert an `IntMap` to an equivalent associative array.

#### `indices`

``` purescript
indices :: forall a. IntMap a -> Array Int
```

Gather all of the indicies stored in an `IntMap`

#### `values`

``` purescript
values :: forall a. IntMap a -> Array a
```

Gather all of the values stored in an `IntMap`

#### `foldMapWithKey`

``` purescript
foldMapWithKey :: forall a m. Monoid m => (Int -> a -> m) -> IntMap a -> m
```

A version of `foldMap` which provides key values during the mapping.

#### `foldlWithKey`

``` purescript
foldlWithKey :: forall a b. (Int -> b -> a -> b) -> b -> IntMap a -> b
```

A version of `foldl` which provides key values during the mapping.

#### `foldrWithKey`

``` purescript
foldrWithKey :: forall a b. (Int -> a -> b -> b) -> b -> IntMap a -> b
```

A version of `foldr` which provides key values during the mapping.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> IntMap a -> IntMap a
```

/O(n)/. Filter all values satisfying the predicate.

#### `filterWithKey`

``` purescript
filterWithKey :: forall a. (Int -> a -> Boolean) -> IntMap a -> IntMap a
```

/O(n)/. Filter all keys\/values satysfying the predicate.

#### `null`

``` purescript
null :: forall a. IntMap a -> Boolean
```

Checks whether an `IntMap` contains any values at all.

#### `size`

``` purescript
size :: forall a. IntMap a -> Int
```

Count the number of values in the `IntMap`

#### `traverseWithKey`

``` purescript
traverseWithKey :: forall a t b. Applicative t => (Int -> a -> t b) -> IntMap a -> t (IntMap b)
```


