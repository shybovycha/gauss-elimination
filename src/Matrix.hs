module Matrix where

import Fraction

newtype Coefficients = Coefficients { coeffList :: [Fraction] } deriving (Eq, Ord, Show)

data Row = Row { coefficients :: Coefficients, freeMember :: Fraction } deriving (Eq, Ord)

type Matrix = [Row]

toList :: Row -> [Fraction]
toList (Row (Coefficients cs) f) = cs ++ [f]

fromList :: [Fraction] -> Row
fromList [] = Row (Coefficients []) 0
fromList xs = Row (Coefficients (init xs)) (last xs)

instance Show Row where
  show r = show (toList r)

{-|
  Element-wise addition of two rows:

  >>> addRows (fromList [1 % 1, 2 % 1]) (fromList [3 % 1, 4 % 1])
  [4,6]

  >>> addRows (fromList [1 % 2, 1 % 3]) (fromList [1 % 2, 2 % 3])
  [1,1]

  >>> addRows (fromList []) (fromList [])
  [0]
-}
addRows :: Row -> Row -> Row
addRows (Row (Coefficients c1) f1) (Row (Coefficients c2) f2) =
  Row (Coefficients (zipWith (+) c1 c2)) (f1 + f2)

{-|
  Multiply a row by a scalar:

  >>> multiplyRow (fromList [1 % 1, 3 % 1, 5 % 1]) (2 % 1)
  [2,6,10]

  >>> multiplyRow (fromList [4 % 1, 6 % 1]) (1 % 2)
  [2,3]

  >>> multiplyRow (fromList [1 % 1, 2 % 1]) (0 % 1)
  [0,0]
-}
multiplyRow :: Row -> Fraction -> Row
multiplyRow (Row (Coefficients cs) f) s = Row (Coefficients (map (* s) cs)) (f * s)

quicksort :: (Ord a) => [a] -> (a -> a -> Int) -> [a]
quicksort [] _ = []
quicksort (x : xs) cmp = (quicksort lesser cmp) ++ [x] ++ (quicksort greater cmp)
  where
    lesser = [i | i <- xs, (cmp x i) < 0]
    greater = [i | i <- xs, (cmp x i) >= 0]

leadingZeros :: Row -> Int
leadingZeros = length . takeWhile (== 0) . coeffList . coefficients

dropLeadingZeros :: Row -> [Fraction]
dropLeadingZeros = dropWhile (== 0) . coeffList . coefficients

compareRows :: Row -> Row -> Int
compareRows r1 r2 = leadingZeros r2 - leadingZeros r1