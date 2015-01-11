import System.IO
import Fraction
import Gauss
import EquationParser

-- main = putStrLn (gaussExtractResults (gaussSolveList [[3,2,3,4,0], [2,5,7,1,-2], [3,4,1,9,5]]))
-- main = putStrLn $ show $ first (parseRow "-1977 x1 + 14 x2 + y = -3" [] [])
-- putStrLn (gaussExtractResults (gaussSolveList [[3,2,3,4,0], [2,5,7,1,-2], [3,4,1,9,5]]))

split :: (Char -> Bool) -> String -> [String]
split _ [] = []
split check s = word : (split check rs)
	where
		word = takeWhile (\e -> not (check e)) s
		wo_word = dropWhile (\e -> not (check e)) s
		rs = dropWhile check wo_word

getStdin = helper ""
	where
		helper str = do
			a <- getLine
			if length a <= 1 then return str else helper (str ++ "\n" ++ a)

parseStdin :: String -> ([[Integer]], [String])
parseStdin txt = (coefficients, var_names)
	where
		ls = filter ((> 1) . length) $ split (== '\n') txt
		ls_parsed = map parseLine ls
		coefficients = map fst ls_parsed
		var_names = concat $ map snd ls_parsed

main = do
	txt <- getStdin
	putStrLn $ (uncurry gaussSolve) (parseStdin txt)
