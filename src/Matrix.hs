module Matrix where

import Fraction

newtype Row = Row { toList :: [Fraction] } deriving (Eq, Ord)

type Matrix = [Row]

fromList :: [Fraction] -> Row
fromList = Row

instance Show Row where
  show (Row xs) = show xs

{-|
  Element-wise addition of two rows:

  >>> addRows (Row [1 % 1, 2 % 1]) (Row [3 % 1, 4 % 1])
  [4,6]

  >>> addRows (Row [1 % 2, 1 % 3]) (Row [1 % 2, 2 % 3])
  [1,1]

  >>> addRows (Row []) (Row [])
  []
-}
addRows :: Row -> Row -> Row
addRows (Row r1) (Row r2) = Row (zipWith (+) r1 r2)

{-|
  Multiply a row by a scalar:

  >>> multiplyRow (Row [1 % 1, 3 % 1, 5 % 1]) (2 % 1)
  [2,6,10]

  >>> multiplyRow (Row [4 % 1, 6 % 1]) (1 % 2)
  [2,3]

  >>> multiplyRow (Row [1 % 1, 2 % 1]) (0 % 1)
  [0,0]
-}
multiplyRow :: Row -> Fraction -> Row
multiplyRow (Row r) s = Row (map (* s) r)

quicksort :: (Ord a) => [a] -> (a -> a -> Int) -> [a]
quicksort [] _ = []
quicksort (x : xs) cmp = (quicksort lesser cmp) ++ [x] ++ (quicksort greater cmp)
  where
    lesser = [i | i <- xs, (cmp x i) < 0]
    greater = [i | i <- xs, (cmp x i) >= 0]

leadingZeros :: Row -> Int
leadingZeros = length . takeWhile (== 0) . toList

compareRows :: Row -> Row -> Int
compareRows r1 r2 = leadingZeros r2 - leadingZeros r1