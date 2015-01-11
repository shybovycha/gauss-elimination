module EquationParser where

import Fraction

deepEqual lists = foldl1 (&&) $ map (== h) ls
	where
		h = head lists
		ls = drop 1 lists

sign :: Integer -> Integer
sign x
	| x < 0 = -1
	| otherwise = 1

isEltInList :: (Eq a) => a -> [a] -> Bool
isEltInList e list = ((> 0) . length) $ filter (== e) list

isNum :: Char -> Bool
isNum ch = isEltInList ch ['0'..'9']

isAlpha :: Char -> Bool
isAlpha ch = isEltInList ch (['a'..'z'] ++ ['_'])

isAlphaNum :: Char -> Bool
isAlphaNum ch = (isAlpha ch) || (isNum ch)

isSpace :: Char -> Bool
isSpace ch = isEltInList ch [' ', '\t']

atoi :: Char -> Integer
atoi ch = snd $ head $ filter ((== ch) . fst) (zip ['0'..'9'] [0..9])

-- ------------------------------------------------------------------------

parseRow :: String -> Integer -> [Integer] -> [String] -> ([Integer], [String])
parseRow [] state coefficients var_names
	| state == 7 = ((reverse coefficients), (reverse var_names))
	| otherwise = error ("Invalid equation (state: " ++ (show state) ++ "; coefficients: " ++ (show coefficients) ++ "; var_names: " ++ (show var_names) ++ ")")

parseRow (c:cs) state coefficients var_names
	| (state == 1) && (c == '-') = parseRow cs 2 ((-1) : coefficients) var_names
	| (state == 1) && (isNum c) = parseRow cs 3 (c_int : coefficients) var_names
	| (state == 1) && (isAlpha c) = parseRow cs 4 coefficients ([c] : var_names)
	| (state == 2) && (isNum c) = parseRow cs 3 (repl_k : (drop 1 coefficients)) var_names
	| (state == 2) && (isAlpha c) = parseRow cs 4 coefficients ([c] : var_names)
	| (state == 3) && (c == '=') = parseRow cs 5 coefficients var_names
	| (state == 3) && (isAlpha c) = parseRow cs 4 coefficients ([c] : var_names)
	| (state == 3) && (isNum c) = parseRow cs 3 (new_k : (drop 1 coefficients)) var_names
	| (state == 4) && (c == '-') = parseRow cs 2 ((-1) : coefficients) var_names
	| (state == 4) && (c == '+') = parseRow cs 2 (1 : coefficients) var_names
	| (state == 4) && (isAlphaNum c) = parseRow cs 4 coefficients (new_v : (drop 1 var_names))
	| (state == 4) && (c == '=') = parseRow cs 5 coefficients var_names
	| (state == 5) && (c == '-') = parseRow cs 6 ((-1) : coefficients) var_names
	| (state == 5) && (isNum c) = parseRow cs 7 (c_int : coefficients) var_names
	| (state == 6) && (isNum c) = parseRow cs 7 (repl_k : (drop 1 coefficients)) var_names
	| (state == 7) && (isNum c) = parseRow cs 7 (new_k : (drop 1 coefficients)) var_names
	| otherwise = parseRow cs state coefficients var_names
	where
		k = abs $ head coefficients
		k_sign = sign (head coefficients)
		c_int = atoi c
		new_k = k_sign * ((k * 10) + c_int)
		repl_k = k_sign * c_int
		v = if (length var_names) > 0 then head var_names else []
		new_v = v ++ [c]

parseLine :: String -> ([Integer], [String])
parseLine s = parseRow s 1 [] []