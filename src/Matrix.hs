module Matrix where

import Fraction

type Row = [Fraction]

type Matrix = [Row]


{-|
  Element-wise addition of two rows:

  >>> addRows [1 % 1, 2 % 1] [3 % 1, 4 % 1]
  [4,6]

  >>> addRows [1 % 2, 1 % 3] [1 % 2, 2 % 3]
  [1,1]

  >>> addRows [] []
  []
-}
addRows :: Row -> Row -> Row
addRows = zipWith (+)

{-|
  Multiply a row by a scalar:

  >>> multiplyRow [1 % 1, 3 % 1, 5 % 1] (2 % 1)
  [2,6,10]

  >>> multiplyRow [4 % 1, 6 % 1] (1 % 2)
  [2,3]

  >>> multiplyRow [1 % 1, 2 % 1] (0 % 1)
  [0,0]
-}
multiplyRow :: Row -> Fraction -> Row
multiplyRow r s = map (* s) r