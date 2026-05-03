module Gauss where

import Fraction
import Map
import Matrix
import Set

newtype Variable = Variable { name :: String } deriving (Eq, Ord)

instance Show Variable where
  show (Variable s) = show s

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

gaussSortMatrix :: Matrix -> Matrix
gaussSortMatrix = flip quicksort compareRows

-- gaussConvertMatrix :: [[Fraction]] -> Matrix
-- gaussConvertMatrix = map (map fromInteger)

-- here, guaranteed that r1 has less leading zeros than r2
gaussMakeZero :: Row -> Row -> Row
gaussMakeZero r1 r2 = case dropWhile ((== 0) . fst) (zip (coeffList (coefficients r1)) (coeffList (coefficients r2))) of
  [] -> r2
  ((r1_head, r2_head) : _) ->
    let factor = -r2_head / r1_head
    in addRows (multiplyRow r1 factor) r2

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
    normalize r = case dropWhile (== 0) (coeffList (coefficients r)) of
      [] -> r
      (pivot : _) -> Row (Coefficients (map (/ pivot) (coeffList (coefficients r)))) (freeMember r / pivot)

{-|
  converts the matrix row reduced by the Gauss algorithm down to few members to string representation of a result.
  technically it does not _show_ the results, it also calculates them.

  if a row contains just one number, it is the free member and it will be the resulting variable.
  if a row contains exactly two numbers, the resulting variable is the free member (last number) over the last coefficient (the first number).
  if a row contains more numbers, then a simple conversion will be made:

  >>> showVariableValues (fromList [3, 4, 5]) [Variable "x1", Variable "x2"]
  "x1 = 5/3 - 4 * x2"

  same as:

  3x1 + 4x2 = 5
  3x1 = 5 - 4x2
  x1 = (5 - 4x2) / 3
-}
showVariableValues :: Row -> [Variable] -> String
showVariableValues r var_names
  | not (null other_coefficients) = var_str ++ other_vars_str
  | otherwise = var_str
  where
    cs = coeffList (coefficients r)
    index = leadingZeros r
    coefficient = cs !! index
    value = freeMember r
    elements_count = length cs
    other_coefficients = filter (\(k, k_idx) -> k /= 0 && k_idx /= index) (zip cs [0 .. elements_count])
    subtract_coefficient k = if k < 0 then " + " ++ show (- k) else " - " ++ show k
    other_vars_str = concatMap (\(k, k_idx) -> subtract_coefficient k ++ " * " ++ name (var_names !! k_idx)) other_coefficients
    var_str = name (var_names !! index) ++ " = " ++ show (value / coefficient)

gaussExtractResults :: Matrix -> [Variable] -> String
gaussExtractResults rows var_names = foldl (\acc row -> acc ++ showVariableValues row var_names ++ "\n") "" rows

isZeroRow :: Row -> Bool
isZeroRow (Row (Coefficients cs) f) = all (== 0) cs && f == 0

isInconsistentRow :: Row -> Bool
isInconsistentRow (Row (Coefficients cs) f) = all (== 0) cs && f /= 0

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
      (r : _) -> length (coeffList (coefficients r))
      [] -> 0

extractAndWrapResults :: Solution -> [Variable] -> String
extractAndWrapResults Inconsistent _ = "System is inconsistent"
extractAndWrapResults (Simple res) var_names = gaussExtractResults res var_names
extractAndWrapResults (Infinite res) var_names = "System has infinite solutions. One of them is\n" ++ gaussExtractResults res var_names

{-|
  Solve a system of linear equations:

  >>> gaussSolve [fromList [2, 3, 8], fromList [1, -1, 1]] [Variable "x", Variable "y"]
  "x = 11/5\ny = 6/5\n"

  >>> gaussSolve [fromList [1, 1, 1, 6], fromList [2, -1, 1, 3], fromList [1, 2, -1, 2]] [Variable "x", Variable "y", Variable "z"]
  "x = 1\ny = 2\nz = 3\n"

  >>> gaussSolve [fromList [3, 2, -1, 1], fromList [2, -2, 4, -2], fromList [-1, 1 % 2, -1, 0]] [Variable "x", Variable "y", Variable "z"]
  "x = 1\ny = -2\nz = -2\n"

  >>> gaussSolve [ fromList [3, 2, -1, 1] , fromList [2, -2, 4, -2] , fromList [-1, 1 % 2, -1, 0] ] [Variable "x", Variable "y", Variable "z"]
  "x = 1\ny = -2\nz = -2\n"

  >>> gaussSolve [fromList [1, 1, 2], fromList [2, 2, 5]] [Variable "x", Variable "y"]
  "System is inconsistent"

  >>> gaussSolve [ fromList [1, 1, 1, 6] , fromList [2, 2, 2, 12] , fromList [3, 3, 3, 18] ] [Variable "x", Variable "y", Variable "z"]
  "System has infinite solutions. One of them is\nx = 6 - 1 * y - 1 * z\n"
-}
gaussSolve :: Matrix -> [Variable] -> String
gaussSolve = extractAndWrapResults . gaussSolveMatrix

extractVariableNames :: [([(Fraction, String)], Fraction)] -> [String]
extractVariableNames = Set.elements . foldl (\acc (equation, _) -> foldl (\acc1 (_, var) -> Set.put acc1 var) acc equation) emptySet

extractFreeMembers :: [([(Fraction, String)], Fraction)] -> [Fraction]
extractFreeMembers = map snd

mapVariablesToFactors :: [([(Fraction, String)], Fraction)] -> [Map String Fraction]
mapVariablesToFactors = map (\(equation, _) -> foldl (\acc (factor, var) -> Map.put acc var factor) emptyMap equation)

convertEquationToMatrix :: [([(Fraction, String)], Fraction)] -> (Matrix, [Variable])
convertEquationToMatrix equations = (matrixView, map Variable variableNames)
  where
    variableNames = extractVariableNames equations
    mapView = mapVariablesToFactors equations
    freeMembers = extractFreeMembers equations
    mapWithFreeView = zip mapView freeMembers
    matrixView = map (\(equationMap, free) -> Row (Coefficients (map (\var -> maybe 0 id (Map.get equationMap var)) variableNames)) free) mapWithFreeView
