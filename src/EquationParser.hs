module EquationParser where

import Fraction

-- | Get the sign of the number (n < 0 = -1 or n >= 0 = 1)
sign :: Integer -> Integer
sign x
  | x < 0 = -1
  | otherwise = 1

-- | Check if an element is in a list
isEltInList :: (Eq a) => a -> [a] -> Bool
isEltInList e list = ((> 0) . length) $ filter (== e) list

-- | Check if a char is a number ('0'..'9')
isNum :: Char -> Bool
isNum ch = isEltInList ch ['0' .. '9']

-- | Check if a char is a letter ('a'..'z' or '_')
isAlpha :: Char -> Bool
isAlpha ch = isEltInList ch (['a' .. 'z'] ++ ['_'])

-- | Check if a char is either a number or a letter
isAlphaNum :: Char -> Bool
isAlphaNum ch = isAlpha ch || isNum ch

-- | Check if a char is a whitespace
isSpace :: Char -> Bool
isSpace ch = isEltInList ch " \t\n\r"

-- | Parse number from a character
atoi :: Char -> Integer
atoi ch = snd $ head $ filter ((== ch) . fst) (zip ['0' .. '9'] [0 .. 9])

-- ------------------------------------------------------------------------

-- | This is an enumeration of all possible parser states (as in state machine):
data ParserState
  = -- | start of the line
    LineStart
  | -- | read either a positive or negative number or symbol name
    ReadElement
  | -- | read number
    ReadCoefficient
  | -- | symbol name
    ReadSymbol
  | -- | after equal sign
    ReadRightSide
  | -- | number
    ReadNegativeFreeMember
  | -- | number
    ReadPositiveFreeMember
  deriving (Show, Eq)

-- | Parse equation string, internal helper method
parseRow ::
  -- | the line to parse
  String ->
  -- | current parser state
  ParserState ->
  -- | list of coefficient accumulated so far
  [Integer] ->
  -- | list of variable names so far, should be aligned with coefficients
  [String] ->
  -- | a tuple of coefficients and variable names
  ([Integer], [String])
parseRow [] state coefficients var_names
  | state == ReadPositiveFreeMember = (reverse coefficients, reverse var_names)
  | otherwise = error ("Invalid equation (state: " ++ show state ++ "; coefficients: " ++ show coefficients ++ "; var_names: " ++ show var_names ++ ")")

parseRow (c : cs) state coefficients var_names
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

parseEquationLine :: String -> ([Integer], [String])
parseEquationLine s = parseRow s LineStart [] []
