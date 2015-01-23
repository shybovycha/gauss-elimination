import System.IO
import Fraction
import Gauss
import EquationParser

-- main = putStrLn (gaussExtractResults (gaussSolveList [[3,2,3,4,0], [2,5,7,1,-2], [3,4,1,9,5]]))
-- main = putStrLn $ show $ first (parseRow "-1977 x1 + 14 x2 + y = -3" [] [])
-- putStrLn (gaussExtractResults (gaussSolveList [[3,2,3,4,0], [2,5,7,1,-2], [3,4,1,9,5]]))

deepEqual lists = all (== h) ls
	where
		h = head lists
		ls = drop 1 lists

split :: (Char -> Bool) -> String -> [String]
split _ [] = []
split check s = word : split check rs
	where
		word = takeWhile (not . check) s
		wo_word = dropWhile (not . check) s
		rs = dropWhile check wo_word

getStdin = helper ""
	where
		helper str = do
			a <- getLine
			if length a <= 1 then return str else helper (str ++ "\n" ++ a)

parseStdin :: String -> ([[Integer]], [String])
parseStdin txt = if inequal_var_names then error error_msg else (coefficients, var_names)
	where
		ls = filter ((> 1) . length) $ split (== '\n') txt
		ls_parsed = map parseLine ls
		coefficients = map fst ls_parsed
		all_var_names = map snd ls_parsed
		var_names = head $ map snd ls_parsed
		inequal_var_names = not $ deepEqual all_var_names
		error_msg = "Invalid variables in equations. Maybe, you should provide some missed variables with coefficient 0? (" ++ show all_var_names ++ ")"

main = do
	txt <- getStdin
	putStrLn $ uncurry gaussSolve (parseStdin txt)
