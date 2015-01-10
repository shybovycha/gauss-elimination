module EquationReader where

import Fraction

type Row = [ Integer ]
type Matrix = [ Row ]

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
readCoefficient :: String -> Integer -> (Integer, String)
readCoefficient [] k = (k, [])
readCoefficient (c:cs) k
	| (c == '-') && (k == 0) = (-k2, s1)
	| (c == '-') && (k > 0) = (k, c:cs)
	| isNum c = readCoefficient cs k1
	| otherwise = (k, c : cs)
	where
		k1 = (k * 10) + (atoi c)
		(k2, s1) = readCoefficient cs k

-- node #1
-- readNegativeCoefficient :: String -> Integer -> (Integer, String)
-- readNegativeCoefficient [] k = (k, [])
-- readNegativeCoefficient src k = (-k', src')
-- 	where
-- 		(k', src') = readCoefficient src k

-- node #3
readVarName :: String -> String -> (String, String)
readVarName [] var_name = (var_name, [])
readVarName (c:cs) var_name
	| (isNum c) && (name_len == 0) = (var_name, c : cs)
	| (isAlpha c) && (name_len == 0) = readVarName cs (var_name ++ [c])
	| (isAlphaNum c) && (name_len > 0) = readVarName cs (var_name ++ [c])
	| otherwise = (var_name, c : cs)
	where
		name_len = length var_name

-- parser
parseRow :: String -> [Integer] -> [String] -> ([Integer], [String], String)
parseRow [] coefficients var_names = (coefficients, var_names, [])
parseRow (c:cs) coefficients var_names
	| c == '-' = parseRow s1 (k : coefficients) var_names
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