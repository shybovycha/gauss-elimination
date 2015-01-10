module EquationReader where

import Fraction

type Row = [ Integer ]
type Matrix = [ Row ]

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

first (a, b, c) = a
second (a, b, c) = b
third (a, b, c) = c

-- ------------------------------------------------------------------------

-- nodes ## 1 .. 2
-- readCoefficient :: String -> Integer -> (Integer, String)
-- readCoefficient [] k = (k, [])
-- readCoefficient (c:cs) k
-- 	| (c == '-') && (k == 0) = (-k2, s1)
-- 	| isNum c = readCoefficient cs k1
-- 	| otherwise = (k, c:cs)
-- 	where
-- 		k1 = (k * 10) + (atoi c)
-- 		(k2, s1) = readCoefficient cs k

-- readCoefficients :: String -> [Integer] -> [Integer]
-- readCoefficients [] values = values
-- readCoefficients (c:cs) values
-- 	| isEltInList c (['+', '-'] ++ ['0'..'9']) = readCoefficients s (values ++ [ k ])
-- 	| otherwise = readCoefficients cs values
-- 	where
-- 		(k, s) = readCoefficient (c:cs) 0

-- node #1
-- readNegativeCoefficient :: String -> Integer -> (Integer, String)
-- readNegativeCoefficient [] k = (k, [])
-- readNegativeCoefficient src k = (-k', src')
-- 	where
-- 		(k', src') = readCoefficient src k

-- node #3
-- readVarName :: String -> String -> (String, String)
-- readVarName [] var_name = (var_name, [])
-- readVarName (c:cs) var_name
-- 	| (isNum c) && (name_len == 0) = (var_name, c : cs)
-- 	| (isAlpha c) && (name_len == 0) = readVarName cs (var_name ++ [c])
-- 	| (isAlphaNum c) && (name_len > 0) = readVarName cs (var_name ++ [c])
-- 	| otherwise = (var_name, c : cs)
-- 	where
-- 		name_len = length var_name

-- parser
{-
parseRow :: String -> [Integer] -> [String] -> Integer -> ([Integer], [String], String)
parseRow [] coefficients var_names state
	| state /= 6 = error "Wrong equation"
	| otherwise = (coefficients, var_names, [])

parseRow (c:cs) coefficients var_names state
	| (c == '-') && (state == 1) = parseRow s1 (k : coefficients) var_names
	| c == '+' = parseRow cs coefficients var_names
	| c == '=' = parseRow s3 (r : coefficients) var_names
	| isNum c  = parseRow s1 (k : coefficients) var_names
	| c == '*' && isCoefficientState = parseRow s2 coefficients (x : var_names)
	| isSpace c = parseRow cs coefficients var_names
	| otherwise = (coefficients, var_names, cs ++ "::" ++ [c])
	where
		s = c : cs
		(k, s1) = readCoefficient s 0
		(x, s2) = readVarName s []
		(r, s3) = readCoefficient cs 0
		isCoefficientState = ((length coefficients) == (length var_names) + 1)
-}

parseRow :: String -> Integer -> [Integer] -> [String] -> ([Integer], [String])
parseRow [] state coefficients var_names
	| state == 6 = ((reverse coefficients), (reverse var_names))
	| otherwise = error ("Invalid equation (state: " ++ (show state) ++ "; coefficients: " ++ (show coefficients) ++ "; var_names: " ++ (show var_names) ++ ")")

parseRow (c:cs) state coefficients var_names
	| (state == 1) && (c == '-') = parseRow cs 2 ((-1) : coefficients) var_names
	| (state == 1) && (isNum c) = parseRow cs 2 (new_k : (drop 1 coefficients)) var_names
	| (state == 1) && (isAlpha c) = parseRow cs 3 coefficients ([c] : var_names)
	| (state == 2) && (isNum c) = parseRow cs 2 (new_k : (drop 1 coefficients)) var_names
	| (state == 2) && (c == '*') = parseRow cs 2 coefficients var_names
	| (state == 2) && (c == '=') = parseRow cs 4 coefficients var_names
	| (state == 2) && (isAlpha c) = parseRow cs 3 coefficients (new_v : (drop 1 var_names))
	| (state == 3) && (c == '-') = parseRow cs 2 ((-1) : coefficients) var_names
	| (state == 3) && (c == '+') = parseRow cs 2 (1 : coefficients) var_names
	| (state == 3) && (isAlphaNum c) = parseRow cs 3 coefficients (new_v : (drop 1 var_names))
	| (state == 3) && (c == '=') = parseRow cs 4 coefficients var_names
	| (state == 4) && (c == '-') = parseRow cs 5 ((-1) : coefficients) var_names
	| (state == 4) && (isNum c) = parseRow cs 6 (new_k : (drop 1 coefficients)) var_names
	| (state == 5) && (isNum c) = parseRow cs 6 (new_k : (drop 1 coefficients)) var_names
	| (state == 6) && (isNum c) = parseRow cs 6 (new_k : (drop 1 coefficients)) var_names
	| otherwise = parseRow cs state coefficients var_names
	where
		k = abs $ head coefficients
		k_sign = sign k
		c_int = atoi c
		new_k = if k == 1 then c_int else ((k * k_sign * 10) + c_int)
		v = if (length var_names) > 0 then head var_names else []
		new_v = v ++ [c]