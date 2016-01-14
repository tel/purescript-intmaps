## Module Data.IntMap

#### `IntMap`

``` purescript
data IntMap a
```

##### Instances
``` purescript
(Semigroup a) => Semigroup (IntMap a)
(Semigroup a) => Monoid (IntMap a)
Functor IntMap
Foldable IntMap
(Eq a) => Eq (IntMap a)
```

#### `empty`

``` purescript
empty :: forall a. IntMap a
```

#### `singleton`

``` purescript
singleton :: forall a. Int -> a -> IntMap a
```

An `IntMap` of a single value.

#### `lookup`

``` purescript
lookup :: forall a. Int -> IntMap a -> Maybe a
```

If a value is available in an `IntMap` at a given tree then `lookup`
will return it. Otherwise, `Nothing`.

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

#### `mergeWith`

``` purescript
mergeWith :: forall a. (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
```

Merges two `IntMap`s together using a splatting function. If 
a key is present in both constituent lists then the resulting 
list will be the splat of the values from each constituent. If the key
was available in only one constituent then it is available unmodified 
in the result.

#### `mergeLeft`

``` purescript
mergeLeft :: forall a. IntMap a -> IntMap a -> IntMap a
```

Like `mergeWith` but where values from the left constituent always override
values from the right.

#### `mergeRight`

``` purescript
mergeRight :: forall a. IntMap a -> IntMap a -> IntMap a
```

Like `mergeWith` but where values from the right constituent always override
values from the left.

#### `mergeWithKey`

``` purescript
mergeWithKey :: forall a. (Int -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
```

Like `mergeWith` but where the splatting function has access to all of the
keys where conflicts arise.

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
foldMapWithKey :: forall a m. (Monoid m) => (Int -> a -> m) -> IntMap a -> m
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


