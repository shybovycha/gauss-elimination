{-- |
  Suboptiomal yet trivial Map implementation.
  All operations have O(n) complexity.
-}

module Map (
  Map,
  emptyMap,
  containsKey,
  keys,
  -- entries,
  values,
  get,
  put
) where

data Map key value = Map [(key, value)] deriving Show

-- | Create an empty map
emptyMap :: Map k v
emptyMap = Map []

-- | Checks whether a key has a value in the map
containsKey :: Eq k => Map k v -> k -> Bool
containsKey (Map entries) key = length matchingEntries == 1
  where
    matchingEntries = filter ((== key) . fst) entries

-- | Add a key => value mapping to the map
put :: Eq k => Map k v -> k -> v -> Map k v
put (Map entries) key value = Map newEntries
  where
    newEntries = (key, value) : (filter (not . (== key) . fst) entries)

-- | Get a value by key
get :: Eq k => Map k v -> k -> Maybe v
get (Map entries) key
  | length matchingElements == 1 = Just ((snd . head) matchingElements)
  | otherwise = Nothing
  where
    matchingElements = filter ((== key) . fst) entries

-- | Get a list of all the keys in the map
keys :: Map k v -> [k]
keys (Map entries) = map fst entries

-- entries :: Map k v -> [(k, v)]
-- entries (Map tuples) = tuples

values :: Map k v -> [v]
values (Map entries) = map snd entries
