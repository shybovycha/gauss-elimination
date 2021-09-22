module Set where

data Set a = Set [a] deriving Show

emptySet :: Set [a]
emptySet = Set []

containsElement :: Eq v => Set v -> v -> Bool
containsElement (Set entries) key = length matchingEntries == 1
  where
    matchingEntries = filter (== key) entries

put :: Eq v => Set v -> v -> Set v
put (Set entries) v = if containsElement (Set entries) v then Set entries else Set (v : entries)

elements :: Eq v => Set v -> [v]
elements (Set entries) = entries
