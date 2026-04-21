module Fraction where

import qualified Data.Ratio as Ratio

data Frac a = a :% a

type Fraction = Frac Integer

{-|
  Create a fraction:

  >>> 1 % 2
  1/2

  >>> 2 % 3
  2/3

  >>> 4 % 2
  2

  >>> 9 % 3
  3
-}
(%) :: Integral a => a -> a -> Frac a
n % d = trim n d

{-|
  Division by zero is defined as infinity:

  >>> 1 % 0
  infinity
-}
infinity :: (Integral a) => (Frac a)
infinity = 1 :% 0

{-|
  Simplifies a fraction down to its simplest form::

  >>> 2 % 4
  1/2

  >>> 6 % 9
  2/3

  >>> 100 % 10
  10

  >>> 3 % 3
  1
-}
trim :: (Integral a) => a -> a -> Frac a
trim _ 0 = infinity
trim x y = ((x * signum y) `quot` g) :% (abs y `quot` g)
  where
    g = gcd x y

{-|
  Get the numerator of a fraction:

  >>> numerator (1 % 2)
  1

  >>> numerator (2 % 4)
  1

  >>> numerator (6 % 9)
  2

  >>> numerator (100 % 10)
  10
-}
numerator :: (Num a) => Frac a -> a
numerator (x :% _) = x

{-|
  Get the denominator of a fraction:

  >>> denominator (1 % 2)
  2

  Note: the fraction constructor performs truncation automatically:

  >>> denominator (2 % 4)
  2

  >>> denominator (6 % 9)
  3
-}
denominator :: (Num a) => Frac a -> a
denominator (_ :% y) = y

instance (Integral a) => Num (Frac a) where
  {-|
    Add two fractions:

    >>> (1 % 2) + (1 % 3)
    5/6

    >>> (1 % 2) + (1 % 2)
    1/1

    >>> (1 % 1) + (1 % 2)
    3/2

    >>> (1 % 1) + (1 % 1)
    2/1

    >>> (1 % 1) + (1 % 0)
    infinity

    >>> (1 % 0) + (1 % 1)
    infinity
  -}
  (x :% y) + (x' :% y') = trim ((x * y') + (x' * y)) (y * y')

  {-|
    Subtract two fractions:

    >>> (1 % 2) - (1 % 3)
    1/6

    >>> (1 % 2) - (1 % 2)
    0/1
  -}
  (x :% y) - (x' :% y') = trim ((x * y') - (x' * y)) (y * y')

  {-|
    Multiply two fractions:

    >>> (1 % 2) * (1 % 3)
    1/6

    >>> (1 % 2) * (1 % 2)
    1/4

    >>> (1 % 1) * (1 % 2)
    1/2

    >>> (1 % 1) * (1 % 1)
    1/1

  >>> (1 % 1) * (1 % 0)
    infinity

    >>> (1 % 0) * (1 % 1)
    infinity
  -}
  (x :% y) * (x' :% y') = trim (x * x') (y * y')

  {-|
    Negate a fraction:

    >>> negate (1 % 2)
    -1/2

    >>> negate (1 % 1)
    -1/1
  -}
  negate (x :% y) = (- x) :% y

  {-|
    Absolute value of a fraction:

    >>> abs (1 % 2)
    1/2

    >>> abs (1 % 1)
    1/1
  -}
  abs (x :% y) = abs x :% y

  {-|
    Signum of a fraction:

    >>> signum (1 % 2)
    1

    >>> signum (1 % 1)
    1

    >>> signum (1 % 0)
    1

    >>> signum (1 % -1)
    -1

    >>> signum (1 % 0)
    0

    >>> signum (0 % 2)
    0

    >>> signum (-1 % 0)
    -1
  -}
  signum (x :% _)
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

  {-|
    Convert an integer to a fraction:

    >>> fromInteger 1
    1/1

    >>> fromInteger 2
    2/1
  -}
  fromInteger x = fromInteger x :% 1

instance (Integral a) => Fractional (Frac a) where
  {-|
    Divide two fractions:

    >>> (1 % 2) / (1 % 3)
    3/2

    >>> (1 % 2) / (1 % 2)
    1/1
  -}
  (x :% y) / (x' :% y') = trim (x * y') (y * x')

  {-|
    Convert a rational number to a fraction, mostly to shut the compiler up
  -}
  fromRational r = trim (fromInteger (Ratio.numerator r)) (fromInteger (Ratio.denominator r))

instance (Integral a, Show a) => Show (Frac a) where
  {-|
    Show a fraction:

    >>> show (1 % 2)
    1/2

    >>> show (1 % 1)
    1/1

    >>> show (1 % 0)
    infinity

    >>> show (1 % -1)
    -1/1

    >>> show (0 % 2)
    0/1

    >>> show (-1 % 0)
    infinity
  -}
  show (a :% b)
    | x == 0 = "0"
    | y == 0 = "infinity"
    | y == 1 = show x
    | otherwise = show x ++ "/" ++ show y
    where
      x = numerator f
      y = denominator f
      f = trim a b

instance (Integral a) => Eq (Frac a) where
  {-|
    Check if two fractions are equal:

    >>> (1 % 2) == (1 % 2)
    True

    >>> (1 % 2) == (1 % 3)
    False
  -}
  (x :% y) == (x' :% y') = (x * y') == (x' * y)

instance (Integral a) => Ord (Frac a) where
  {-|
    Compare two fractions:

    >>> (1 % 2) <= (1 % 3)
    False

    >>> (1 % 2) <= (1 % 2)
    True
  -}
  (x :% y) <= (x' :% y') = (x * y') <= (x' * y)
