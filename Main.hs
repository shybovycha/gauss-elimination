-- import System.IO
import Fraction
import Gauss
import EquationReader

-- readCoefficient :: String -> String -> Fraction
-- readCoefficient (c:cs) prev = if 

-- readRow :: String -> Row
-- readRow s = 

-- readMatrix :: Matrix
-- readMatrix = do
-- 	ls <- hGetContents (hDuplicate stdin)
-- 	map readRow ls

-- main = putStrLn (gaussExtractResults (gaussSolveList [[3,2,3,4,0], [2,5,7,1,-2], [3,4,1,9,5]]))
main = putStrLn $ show $ first (parseRow "-1977 x1 + 14 x2 + y = -3" [] []) -- putStrLn (gaussExtractResults (gaussSolveList [[3,2,3,4,0], [2,5,7,1,-2], [3,4,1,9,5]]))