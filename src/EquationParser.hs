module EquationParser where

import Fraction

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
isAlphaNum ch = isAlpha ch || isNum ch

isSpace :: Char -> Bool
isSpace ch = isEltInList ch " \t"

atoi :: Char -> Integer
atoi ch = snd $ head $ filter ((== ch) . fst) (zip ['0'..'9'] [0..9])

-- ------------------------------------------------------------------------

-- LineStart (start of the line) = 1, ReadElement (read either a positive or negative number or symbol name) = 2, ReadCoefficient (read number) = 3, ReadSymbol (symbol name) = 4, ReadRightSide (after equal sign) = 5, ReadNegativeFreeMember (number) = 6, ReadPositiveFreeMember (number) = 7
data ParserState = LineStart | ReadElement | ReadCoefficient | ReadSymbol | ReadRightSide | ReadNegativeFreeMember | ReadPositiveFreeMember deriving (Show, Eq)

parseRow :: String -> ParserState -> [Integer] -> [String] -> ([Integer], [String])
parseRow [] state coefficients var_names
  | state == ReadPositiveFreeMember = (reverse coefficients, reverse var_names)
  | otherwise = error ("Invalid equation (state: " ++ show state ++ "; coefficients: " ++ show coefficients ++ "; var_names: " ++ show var_names ++ ")")

parseRow (c:cs) state coefficients var_names
  | state == LineStart && (c == '-') = parseRow cs ReadElement ((-1) : coefficients) var_names
  | state == LineStart && isNum c = parseRow cs ReadCoefficient (c_int : coefficients) var_names
  | state == LineStart && isAlpha c = parseRow cs ReadSymbol (1 : coefficients) ([c] : var_names)
  | state == ReadElement && isNum c = parseRow cs ReadCoefficient (repl_k : drop 1 coefficients) var_names
  | state == ReadElement && isAlpha c = parseRow cs ReadSymbol coefficients ([c] : var_names)
  | state == ReadCoefficient && (c == '=') = parseRow cs ReadRightSide coefficients var_names
  | state == ReadCoefficient && isAlpha c = parseRow cs ReadSymbol coefficients ([c] : var_names)
  | state == ReadCoefficient && isNum c = parseRow cs ReadCoefficient (new_k : drop 1 coefficients) var_names
  | state == ReadSymbol && (c == '-') = parseRow cs ReadElement ((-1) : coefficients) var_names
  | state == ReadSymbol && (c == '+') = parseRow cs ReadElement (1 : coefficients) var_names
  | state == ReadSymbol && isAlphaNum c = parseRow cs ReadSymbol coefficients (new_v : drop 1 var_names)
  | state == ReadSymbol && (c == '=') = parseRow cs ReadRightSide coefficients var_names
  | state == ReadRightSide && (c == '-') = parseRow cs ReadNegativeFreeMember ((-1) : coefficients) var_names
  | state == ReadRightSide && isNum c = parseRow cs ReadPositiveFreeMember (c_int : coefficients) var_names
  | state == ReadNegativeFreeMember && isNum c = parseRow cs ReadPositiveFreeMember (repl_k : drop 1 coefficients) var_names
  | state == ReadPositiveFreeMember && isNum c = parseRow cs ReadPositiveFreeMember (new_k : drop 1 coefficients) var_names
  | otherwise = parseRow cs state coefficients var_names
  where
    k = abs $ head coefficients
    k_sign = sign (head coefficients)
    c_int = atoi c
    new_k = k_sign * ((k * 10) + c_int)
    repl_k = k_sign * c_int
    v = if not (null var_names) then head var_names else []
    new_v = v ++ [c]

parseLine :: String -> ([Integer], [String])
parseLine s = parseRow s LineStart [] []