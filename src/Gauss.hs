module Gauss where

import Fraction
import Map
import Set

type Row = [Fraction]

type Matrix = [Row]

data Solution = Simple Matrix | Infinite Matrix | Inconsistent

instance Show Solution where
  show (Simple mat) = "Simple solution: " ++ show mat
  show (Infinite mat) = "Infinite solutions: " ++ show mat
  show Inconsistent = "Inconsistent system"

{-|
Overall algorithm:

1. Sort rows by count of leading zeros
2. Make zero in each row at its index position and add it to others making zero in that position from top to bottom
3. Do the same from bottom to the top
-}

quicksort :: (Ord a) => [a] -> (a -> a -> Int) -> [a]
quicksort [] _ = []
quicksort (x : xs) cmp = (quicksort lesser cmp) ++ [x] ++ (quicksort greater cmp)
  where
    lesser = [i | i <- xs, (cmp x i) < 0]
    greater = [i | i <- xs, (cmp x i) >= 0]

leadingZeros :: Row -> Int
leadingZeros = length . takeWhile (== 0)

gaussCompareRows :: Row -> Row -> Int
gaussCompareRows r1 r2 = leadingZeros r2 - leadingZeros r1

gaussSortMatrix :: Matrix -> Matrix
gaussSortMatrix = flip quicksort gaussCompareRows

-- gaussConvertMatrix :: [[Fraction]] -> Matrix
-- gaussConvertMatrix = map (map fromInteger)

-- here, guaranteed that r1 has less leading zeros than r2
gaussMakeZero :: Row -> Row -> Row
gaussMakeZero r1 r2 = case dropWhile ((== 0) . fst) (zip r1 r2) of
  [] -> r2
  ((r1_head, r2_head) : _) ->
    let factor = -r2_head / r1_head
    in zipWith (\a b -> a * factor + b) r1 r2

-- apply the "zeroing head" operation to all the rows except the first one.
-- do this recursively for every row
gaussReduce :: Matrix -> Matrix
gaussReduce = gaussReduce' . gaussSortMatrix
  where
    gaussReduce' [] = []
    gaussReduce' (r1 : rs) = r1 : gaussReduce' (gaussSortMatrix (map (gaussMakeZero r1) rs))

-- similar to gaussReduce, but does not sort the matrix by leading zeros to eliminate all the factors except <current row variable>
gaussReduceBack :: Matrix -> Matrix
gaussReduceBack = reverse . gaussReduceBack' . reverse
  where
    gaussReduceBack' [] = []
    gaussReduceBack' (r1 : rs) = r1 : gaussReduceBack' (map (gaussMakeZero r1) rs)

gaussFixCoefficients :: Matrix -> Matrix
gaussFixCoefficients = map normalize
  where
    normalize r = case dropWhile (== 0) r of
      [] -> r
      (pivot : _) -> map (/ pivot) r

{-|
  converts the matrix row reduced by the Gauss algorithm down to few members to string representation of a result.
  technically it does not _show_ the results, it also calculates them.

  if a row contains just one number, it is the free member and it will be the resulting variable.
  if a row contains exactly two numbers, the resulting variable is the free member (last number) over the last coefficient (the first number).
  if a row contains more numbers, then a simple conversion will be made:

  >>> showVariableValues [3, 4, 5] ["x1", "x2"]
  "x1 = 5/3 - 4 * x2"

  same as:

  3x1 + 4x2 = 5
  3x1 = 5 - 4x2
  x1 = (5 - 4x2) / 3
-}
showVariableValues :: Row -> [String] -> String
showVariableValues r var_names
  | not (null other_coefficients) = var_str ++ other_vars_str
  | otherwise = var_str
  where
    index = leadingZeros r
    coefficient = r !! index
    value = last r
    raw_row = init r -- row coefficients without the free member
    elements_count = length raw_row
    other_coefficients = filter (\(k, k_idx) -> k /= 0 && k_idx /= index) (zip raw_row [0 .. elements_count])
    subtract_coefficient k = if k < 0 then " + " ++ show (- k) else " - " ++ show k
    other_vars_str = concatMap (\(k, k_idx) -> subtract_coefficient k ++ " * " ++ (var_names !! k_idx)) other_coefficients
    var_str = (var_names !! index) ++ " = " ++ show (value / coefficient)

gaussExtractResults :: Matrix -> [String] -> String
gaussExtractResults rows var_names = foldl (\acc row -> showVariableValues row var_names ++ "\n" ++ acc) "" rows

isZeroRow :: Row -> Bool
isZeroRow = all (== 0)

isInconsistentRow :: Row -> Bool
isInconsistentRow []  = False
isInconsistentRow row = all (== 0) (init row) && last row /= 0

gaussSolveMatrix :: Matrix -> Solution
gaussSolveMatrix mat
  | any isInconsistentRow m3 = Inconsistent
  | length pivots < numVars = Infinite (gaussFixCoefficients pivots)
  | otherwise = Simple (gaussFixCoefficients pivots)
  where
    m1 = gaussReduce mat
    m2 = gaussReduceBack m1
    m3 = reverse m2
    pivots = filter (not . isZeroRow) m2
    numVars = case mat of
      (r : _) -> length r - 1
      [] -> 0

extractAndWrapResults :: Solution -> [String] -> String
extractAndWrapResults Inconsistent _ = "System is inconsistent"
extractAndWrapResults (Simple res) var_names = gaussExtractResults res var_names
extractAndWrapResults (Infinite res) var_names = "System has infinite solutions. One of them is\n" ++ gaussExtractResults res var_names

{-|
  Solve a system of linear equations:

  >>> gaussSolve [[2, 3, 8], [1, -1, 1]] ["x", "y"]
  "x = 11/5\ny = 6/5\n"

  >>> gaussSolve [[1, 1, 1, 6], [2, -1, 1, 3], [1, 2, -1, 2]] ["x", "y", "z"]
  "x = 1\ny = 2\nz = 3\n"

  >>> gaussSolve [[3, 2, -1, 1], [2, -2, 4, -2], [-1, 1 % 2, -1, 0]] ["x", "y", "z"]
  "x = 1\ny = -2\nz = -2\n"

  >>> gaussSolve [ [3, 2, -1, 1] , [2, -2, 4, -2] , [-1, 1 % 2, -1, 0] ] ["x", "y", "z"]
  "x = 1\ny = -2\nz = -2\n"

  >>> gaussSolve [[1, 1, 2], [2, 2, 5]] ["x", "y"]
  "System is inconsistent"

  >>> gaussSolve [ [1, 1, 1, 6] , [2, 2, 2, 12] , [3, 3, 3, 18] ] ["x", "y", "z"]
  "System has infinite solutions. One of them is\nx = 6 - 1 * y - 1 * z\n"
-}
gaussSolve :: [[Fraction]] -> [String] -> String
gaussSolve = extractAndWrapResults . gaussSolveMatrix

extractVariableNames :: [([(Fraction, String)], Fraction)] -> [String]
extractVariableNames = Set.elements . foldl (\acc (equation, _) -> foldl (\acc1 (_, var) -> Set.put acc1 var) acc equation) emptySet

extractFreeMembers :: [([(Fraction, String)], Fraction)] -> [Fraction]
extractFreeMembers = map snd

mapVariablesToFactors :: [([(Fraction, String)], Fraction)] -> [Map String Fraction]
mapVariablesToFactors = map (\(equation, _) -> foldl (\acc (factor, var) -> Map.put acc var factor) emptyMap equation)

convertEquationToMatrix :: [([(Fraction, String)], Fraction)] -> ([[Fraction]], [String])
convertEquationToMatrix equations = (matrixView, variableNames)
  where
    variableNames = extractVariableNames equations
    mapView = mapVariablesToFactors equations
    freeMembers = extractFreeMembers equations
    mapWithFreeView = zip mapView freeMembers
    matrixView = map (\(equationMap, free) -> (map (\var -> maybe 0 id (Map.get equationMap var)) variableNames) ++ [free]) mapWithFreeView
