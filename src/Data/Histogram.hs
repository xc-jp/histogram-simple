{-# LANGUAGE CPP #-}

-- |
-- Simple 'Data.Map'-based histogram.
-- A histogram counts occurrences of things, i.e. 'Histogram k' represents a mapping @k -> Int@.
-- Since it is backed by a 'Map' from 'Data.Map', it requires @k@ to have an @Ord@ instance.
module Data.Histogram
  ( Histogram,
    toMap,
    increment,
    decrement,
    Data.Histogram.lookup,
    (!),
    add,
    set,
    reset,
    zero,
    nonzero,
    size,
    empty,
    keys,
    mapKeys,
    singleton,
    singletonCount,
    split,
    splitLookup,
    isSubsetOf,
    isSubsetOfBy,
    disjoint,
    fromList,
    fromCountList,
    flatMap,
    toList,
    fromMap,
    unsafeFromMap,
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- | A simple 'Data.Map'-based histogram that counts occurrences of @k@.
newtype Histogram k = Histogram
  { -- | Convert to a histogram to a map of counts of all nonzero values
    toMap :: M.Map k Int
  }
  deriving (Eq, Show)

instance Ord k => Semigroup (Histogram k) where
  Histogram m1 <> Histogram m2 = Histogram $ M.unionWith (+) m1 m2

instance Ord k => Monoid (Histogram k) where
  mempty = Histogram mempty
  mappend = (<>)

{-# INLINE clip #-}
clip :: Int -> Maybe Int
clip n
  | n > 0 = Just n
  | otherwise = Nothing

-- | Construct a histogram from a map, removing all elements smaller than 1
fromMap :: M.Map k Int -> Histogram k
fromMap = Histogram . M.mapMaybe clip

-- | Construct a histogram directly from a map, without checking if every element is above 1
unsafeFromMap :: M.Map k Int -> Histogram k
unsafeFromMap = Histogram

-- | Increase a key's count by one
increment :: Ord k => k -> Histogram k -> Histogram k
increment k (Histogram m) = Histogram $ M.insertWith (+) k 1 m

-- | Decrease a key's count by one
decrement :: Ord k => k -> Histogram k -> Histogram k
decrement k (Histogram m) = Histogram $ M.update f' k m
  where
    f' n = clip (n - 1)

-- | Increase a key's count by an arbitrary number.
--   Can also be used to decrease by passing a negative value.
--   If the count falls below zero, it's set to 0.
add :: Ord k => Int -> k -> Histogram k -> Histogram k
add n k (Histogram m) = Histogram $ M.alter f k m
  where
    f nOld = clip $ fromMaybe 0 nOld + n

-- | Set a key's count to an exact value.
--   Nonpositive numbers clip to 0.
set :: Ord k => Int -> k -> Histogram k -> Histogram k
set n k (Histogram m) = Histogram $ (if n > 0 then flip M.insert n else M.delete) k m

-- | Set a key's count to 0.
reset :: Ord k => k -> Histogram k -> Histogram k
reset k (Histogram m) = Histogram $ M.delete k m

-- | Check whether a key has a count of at least 1.
nonzero :: Ord k => k -> Histogram k -> Bool
nonzero k (Histogram m) = M.member k m

-- | Check whether a key has a count of 0
zero :: Ord k => k -> Histogram k -> Bool
zero k = not . nonzero k

-- | Get the total number of elements in the map, i.e. the sum of all bins.
size :: Histogram k -> Int
size = sum . M.elems . toMap

-- | Check whether a histogram is empty
empty :: Histogram k -> Bool
empty = null . toMap

-- | Get a list of all non-zero keys.
keys :: Histogram k -> [k]
keys = M.keys . toMap

-- | Applies a function to every key.
--   If two keys in the original map to the same value, their counts are combined.
mapKeys :: Ord k2 => (k1 -> k2) -> Histogram k1 -> Histogram k2
mapKeys f (Histogram m) = Histogram $ M.mapKeysWith (+) f m

-- | A histogram containing one key with a count of 1.
singleton :: k -> Histogram k
singleton k = Histogram $ M.singleton k 1

singletonCount :: Ord k => k -> Int -> Histogram k
singletonCount k n
  | n > 1 = Histogram $ M.singleton k n
  | otherwise = mempty

-- | @isSubsetOfBy f h1 h2@ returns 'True' if every key in @h1@ compares to 'True' to its corresponding key in @h2@ by @f@.
isSubsetOfBy :: Ord k => (Int -> Int -> Bool) -> Histogram k -> Histogram k -> Bool
isSubsetOfBy f (Histogram h1) (Histogram h2) = M.isSubmapOfBy f h1 h2

-- | @isSubsetOf h1 h2@ returns 'True' if no key has a greater count in @h1@ than in @h2@.
isSubsetOf :: Ord k => Histogram k -> Histogram k -> Bool
isSubsetOf = isSubsetOfBy (<=)

-- | Construct a histogram by counting occurrences in a list of keys.
fromList :: Ord k => [k] -> Histogram k
fromList = foldr increment mempty

fromCountList :: Ord k => [(k,Int)] -> Histogram k
fromCountList = foldMap (uncurry singletonCount)

flatMap :: Ord k' => (k -> Int -> Histogram k') -> Histogram k -> Histogram k'
flatMap f = foldMap (uncurry f) . toList

toList :: Histogram k -> [(k, Int)]
toList = M.toList . toMap

lookup :: Ord k => k -> Histogram k -> Int
lookup k (Histogram m) = fromMaybe 0 (m M.!? k)

-- | /O(n)/. The expression (@'split' k hist@) is a pair @(h1,h2)@
-- where all keys in @h1@ are lower than @k@ and all keys in
-- @h2@ larger than @k@. Any key equal to @k@ is found in neither @h1@ nor @h2@.
split :: Ord k => k -> Histogram k -> (Histogram k, Histogram k)
split k (Histogram m) = let (lt, gt) = M.split k m in (Histogram lt, Histogram gt)

splitLookup :: Ord k => k -> Histogram k -> (Histogram k, Int, Histogram k)
splitLookup k (Histogram m) = let (lt, c, gt) = M.splitLookup k m in (Histogram lt, fromMaybe 0 c, Histogram gt)

(!) :: Ord k => Histogram k -> k -> Int
(!) = flip Data.Histogram.lookup

-- | @'disjoint' k1 k2@ returns @True@ when there is no key that is nonzero in both @k1@ and @k2@.
disjoint :: Ord k => Histogram k -> Histogram k -> Bool

#if MIN_VERSION_containers (0,6,2)
disjoint (Histogram m1) (Histogram m2) = M.disjoint m1 m2
#else
disjoint (Histogram m1) (Histogram m2) = M.null (M.intersection m1 m2)
#endif
