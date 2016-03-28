## Module Data.IntMap.Internal

#### `Prefix`

``` purescript
newtype Prefix
  = Prefix Int
```

##### Instances
``` purescript
Eq Prefix
```

#### `Mask`

``` purescript
newtype Mask
  = Mask Int
```

##### Instances
``` purescript
Eq Mask
Ord Mask
```

#### `maskLonger`

``` purescript
maskLonger :: Mask -> Mask -> Boolean
```

#### `prefixAsKey`

``` purescript
prefixAsKey :: Prefix -> Int
```

#### `branchLeft`

``` purescript
branchLeft :: Mask -> Int -> Boolean
```

#### `matchPrefix`

``` purescript
matchPrefix :: Prefix -> Mask -> Int -> Boolean
```

#### `mask`

``` purescript
mask :: Mask -> Int -> Prefix
```

#### `_mask`

``` purescript
_mask :: Int -> Int -> Int
```

#### `highestBit`

``` purescript
highestBit :: Int -> Int -> Int
```

#### `branchingBit'`

``` purescript
branchingBit' :: Int -> Mask -> Int -> Mask -> Mask
```

#### `branchingBit`

``` purescript
branchingBit :: Int -> Int -> Mask
```

#### `branchMask`

``` purescript
branchMask :: Int -> Int -> Mask
```

#### `highestBitMask`

``` purescript
highestBitMask :: Int -> Int
```

#### `dec2bin`

``` purescript
dec2bin :: Int -> String
```

#### `bin2dec`

``` purescript
bin2dec :: String -> Int
```

#### `pow`

``` purescript
pow :: Fn2 Int Int Int
```

#### `max`

``` purescript
max :: Fn2 Int Int Int
```


