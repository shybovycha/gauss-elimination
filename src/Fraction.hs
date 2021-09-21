module Fraction where

data Frac a = a :% a

type Fraction = Frac Integer

(%) :: Integral a => a -> a -> Frac a
n % d = (trim n d)

infinity :: (Integral a) => (Frac a)
infinity = 1 :% 0

trim :: (Integral a) => a -> a -> Frac a
trim _ 0 = infinity
trim x y = ((x * signum y) `quot` g) :% (abs y `quot` g)
  where
    g = gcd x y

numerator :: (Num a) => Frac a -> a
numerator (x :% _) = x

denominator :: (Num a) => Frac a -> a
denominator (_ :% y) = y

instance (Integral a) => Num (Frac a) where
  (x :% y) + (x' :% y') = trim ((x * y') + (x' * y)) (y * y')
  (x :% y) - (x' :% y') = trim ((x * y') - (x' * y)) (y * y')
  (x :% y) * (x' :% y') = trim (x * x') (y * y')

  negate (x :% y) = (- x) :% y
  abs (x :% y) = abs x :% y
  signum (x :% y)
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

  fromInteger x = fromInteger x :% 1

instance (Integral a) => Fractional (Frac a) where
  (x :% y) / (x' :% y') = trim (x * y') (y * x')

instance (Integral a, Show a) => Show (Frac a) where
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
  (x :% y) == (x' :% y') = (x * g) == (x' * g)
    where
      g = gcd y y'

instance (Integral a) => Ord (Frac a) where
  (x :% y) <= (x' :% y') = (x * g) <= (x' * g)
    where
      g = gcd y y'
