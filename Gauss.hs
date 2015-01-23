module Gauss where

import Fraction

type Row = [ Frac Integer ]
type Matrix = [ Row ]

-- 1. Sort rows by count of leading zeros
-- 2. Make zero in each row at its index position and add it to others making zero in that position from top to bottom
-- 3. Do the same from bottom to the top

gaussConvertMatrix :: [ [ Integer ] ] -> Matrix
gaussConvertMatrix = map (map (% 1))

quicksort :: (Ord a) => [a] -> (a -> a -> Int) -> [a]
quicksort [] _ = []
quicksort (x:xs) cmp = quicksort lesser cmp ++ [x] ++ quicksort greater cmp
	where
		lesser = filter (\i -> cmp x i < 0) xs
		greater = filter (\i -> cmp x i >= 0) xs

leadingZeros :: Row -> Int
leadingZeros = length . takeWhile (== 0)

gaussCompareRows :: Row -> Row -> Int
gaussCompareRows r1 r2 = leadingZeros r2 - leadingZeros r1

gaussSortMatrix :: Matrix -> Matrix
gaussSortMatrix = flip quicksort gaussCompareRows

-- check if matrix is inconsistent - it will have all zeroes except last column in at least one row
inconsistentMatrix = any $ all (== 0) . reverse . drop 1 . reverse

-- here, guaranteed that r1 has less leading zeros than r2
gaussMakeZero :: Row -> Row -> Row
gaussMakeZero r1 r2 = map (\pair -> (fst pair * factor) + snd pair) (zip r1 r2)
	where
		index = leadingZeros r1
		r1_elt = r1 !! index
		r2_elt = r2 !! index
		factor = -r2_elt / r1_elt

gaussReduce :: Matrix -> Matrix
gaussReduce [] = []
gaussReduce (r1:rs) = r1 : gaussReduce (map (gaussMakeZero r1) rs)

gaussFixCoefficients :: Matrix -> Matrix
gaussFixCoefficients [] = []
gaussFixCoefficients (r:rs) = map (/ factor) r : gaussFixCoefficients rs
	where
		index = leadingZeros r
		factor = r !! index

gaussShowVars :: Row -> [String] -> String
gaussShowVars r var_names
	| not (null other_coefficients) = var_str ++ other_vars_str
	| otherwise = var_str
	where
		index = leadingZeros r
		koefficient = r !! index
		value = last r
		raw_row = reverse (drop 1 (reverse r))
		elements_count = length raw_row
		other_coefficients = filter (\pair -> fst pair /= 0 && snd pair /= index) (zip raw_row [0..elements_count])
		subtract_coefficient k = if k < 0 then " + " ++ show (-k) else " - " ++ show k
		other_vars_str = concatMap (\pair -> subtract_coefficient (fst pair) ++ " * " ++ (var_names !! snd pair)) other_coefficients
		var_str = (var_names !! index) ++ " = " ++ show (value / koefficient)

gaussExtractResults :: Matrix -> [String] -> String
gaussExtractResults [] _ = []
gaussExtractResults (r:rs) var_names = gaussShowVars r var_names ++ "\n" ++ gaussExtractResults rs var_names

gaussSolveMatrix :: Matrix -> Matrix
gaussSolveMatrix mat = gaussFixCoefficients (reverse (gaussReduce (reverse (gaussReduce mat))))

gaussSolveList :: [[Integer]] -> Matrix
gaussSolveList mat = gaussSolveMatrix (gaussConvertMatrix mat)

gaussSolve :: [[Integer]] -> [String] -> String
gaussSolve mat
	| inconsistentMatrix res = error "Matrix is inconsistent"
	| otherwise = gaussExtractResults res
	where
		res = gaussSolveList mat