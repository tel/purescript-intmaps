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

#### `insert`

``` purescript
insert :: forall a. Int -> a -> IntMap a -> IntMap a
```

#### `insertWith`

``` purescript
insertWith :: forall a. (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
```

#### `insertWithKey`

``` purescript
insertWithKey :: forall a. (Int -> a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
```

#### `mergeWith`

``` purescript
mergeWith :: forall a. (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
```

#### `mergeLeft`

``` purescript
mergeLeft :: forall a. IntMap a -> IntMap a -> IntMap a
```

#### `mergeRight`

``` purescript
mergeRight :: forall a. IntMap a -> IntMap a -> IntMap a
```

#### `mergeWithKey`

``` purescript
mergeWithKey :: forall a. (Int -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
```

#### `mapWithKey`

``` purescript
mapWithKey :: forall a b. (Int -> a -> b) -> IntMap a -> IntMap b
```

Transform all of the values in the map.


