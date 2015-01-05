module Fraction where

data Frac a = a :/ a -- deriving (Eq, Ord, Show)

-- type Fraction = Frac Integer

infinity :: (Integral a) => (Frac a)
infinity = 1 :/ 0

trim :: (Integral a) => a -> a -> Frac a
trim _ 0 = infinity -- error "Division by zero within fraction"
trim x y = (x `quot` g) :/ (y `quot` g)
	where g = gcd x y

x % y = trim (x * signum y) (abs y)

instance (Integral a) => Num (Frac a) where
	(x :/ y) + (x' :/ y') = trim ((x * y') + (x' * y)) (y * y')
	(x :/ y) - (x' :/ y') = trim ((x * y') - (x' * y)) (y * y')
	(x :/ y) * (x' :/ y') = trim (x * x') (y * y')

	negate (x :/ y) = (-x) :/ y
	abs (x :/ y) = abs x :/ y
	signum (x :/ y)
		| x > 0 = 1
		| x < 0 = -1
		| otherwise = 0

	fromInteger x = fromInteger x :/ 1

instance (Integral a) => Fractional (Frac a) where
	(x :/ y) / (x' :/ y') = trim (x * y') (y * x')

instance (Integral a, Show a) => Show (Frac a) where
	show (x :/ y)
		| y == 0 = "infinity"
		| y == 1 = show x
		| otherwise = show x ++ "/" ++ show y

instance (Integral a) => Eq (Frac a) where
	(x :/ y) == (x' :/ y') = (x * g) == (x' * g)
		where g = gcd y y'

instance (Integral a) => Ord (Frac a) where
	(x :/ y) <= (x' :/ y') = (x * g) <= (x' * g)
		where g = gcd y y'