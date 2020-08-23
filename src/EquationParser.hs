{--|
  Parser for the user input. Parses single lines into equations and lists of lines into equation systems.
--}

module EquationParser where

import Control.Monad (foldM)

import Fraction

-- | Get the sign of the number
--
-- >>> sign (-7)
-- -1
-- >>> sign 0
-- 1
-- >>> sign 42
-- 1
--
sign :: Integer -> Integer
sign x
  | x < 0 = -1
  | otherwise = 1

-- | Check if an element is a member of a given list
-- >>> member 12 [-4, 7, 13]
-- False
-- >>> member 2 [1, 2, 3]
-- True
--
member :: (Eq a) => a -> [a] -> Bool
member e list = ((> 0) . length) $ filter (== e) list

-- | Check if a char is a number ('0'..'9')
-- >>> isNum 'a'
-- False
-- >>> isNum '4'
-- True
--
isNum :: Char -> Bool
isNum ch = member ch ['0' .. '9']

-- | Check if a char is a letter ('a'..'z' or '_')
isAlpha :: Char -> Bool
isAlpha ch = member ch (['a' .. 'z'] ++ ['_'])

-- | Check if a char is either a number or a letter
isAlphaNum :: Char -> Bool
isAlphaNum ch = isAlpha ch || isNum ch

-- | Check if a char is a whitespace
isSpace :: Char -> Bool
isSpace ch = member ch " \t\n\r"

-- | Parse number from a character
atoi :: Char -> Maybe Integer
atoi ch
  | isNum ch = Just (fst . head $ filter ((== ch) . snd) (zip [0..9] ['0'..'9']))
  | otherwise = Nothing

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
  Either String ([Integer], [String])

parseRow [] ReadPositiveFreeMember coefficients var_names = Right (reverse coefficients, reverse var_names)
parseRow [] state coefficients var_names = Left ("Invalid equation (state: " ++ show state ++ "; coefficients: " ++ show coefficients ++ "; var_names: " ++ show var_names ++ ")")

parseRow (c : cs) LineStart coefficients var_names
  | (c == '-') = parseRow cs ReadElement ((-1) : coefficients) var_names
  | isNum c = do
    c_int <- maybe
      (Left ("Invalid coefficient " ++ [c]))
      (Right)
      (atoi c)
    parseRow cs ReadCoefficient (c_int : coefficients) var_names
  | isAlpha c = parseRow cs ReadSymbol (1 : coefficients) ([c] : var_names)
  | otherwise = Left ("Invalid equation (state: LineStart; coefficients: " ++ show coefficients ++ "; var_names: " ++ show var_names ++ "; current char: `" ++ (show c) ++ "`)")

parseRow (c : cs) ReadElement (k : ks) var_names
  | isNum c = do
    repl_k <- maybe
      (Left ("Invalid coefficient " ++ [c]))
      (\c_int -> Right (c_int * (sign k)))
      (atoi c)
    parseRow cs ReadCoefficient (repl_k : ks) var_names
  | isAlpha c = parseRow cs ReadSymbol (k : ks) ([c] : var_names)
  | otherwise = Left ("Invalid equation (state: ReadElement; coefficients: " ++ show (k : ks) ++ "; var_names: " ++ show var_names ++ "; current char: `" ++ (show c) ++ "`)")

parseRow (c : cs) ReadCoefficient (k : ks) var_names
  | (c == '=') = parseRow cs ReadRightSide (k : ks) var_names
  | isAlpha c = parseRow cs ReadSymbol (k : ks) ([c] : var_names)
  | isNum c = do
    new_k <- maybe
      (Left ("Invalid coefficient " ++ [c]))
      (\c_int -> Right ((k * 10) + c_int))
      (atoi c)
    parseRow cs ReadCoefficient (new_k : ks) var_names
  | otherwise = Left ("Invalid equation (state: ReadCoefficient; coefficients: " ++ show (k : ks) ++ "; var_names: " ++ show var_names ++ "; current char: `" ++ (show c) ++ "`)")

parseRow (c : cs) ReadSymbol coefficients var_names
  | (c == '-') = parseRow cs ReadElement ((-1) : coefficients) var_names
  | (c == '+') = parseRow cs ReadElement (1 : coefficients) var_names
  | (c == '=') = parseRow cs ReadRightSide coefficients var_names
  | isAlphaNum c && (null var_names) = parseRow cs ReadSymbol coefficients [[c]]
  | isAlphaNum c = parseRow cs ReadSymbol coefficients (((head var_names) ++ [c]) : drop 1 var_names)
  | otherwise = Left ("Invalid equation (state: ReadSymbol; coefficients: " ++ show coefficients ++ "; var_names: " ++ show var_names ++ "; current char: `" ++ (show c) ++ "`)")

parseRow (c : cs) ReadRightSide coefficients var_names
  | (c == '-') = parseRow cs ReadNegativeFreeMember ((-1) : coefficients) var_names
  | isNum c = do
    c_int <- maybe (Left ("Invalid coefficient " ++ [c])) (Right) (atoi c)
    parseRow cs ReadPositiveFreeMember (c_int : coefficients) var_names
  | otherwise = Left ("Invalid equation (state: ReadRightSide; coefficients: " ++ show coefficients ++ "; var_names: " ++ show var_names ++ "; current char: `" ++ (show c) ++ "`)")

parseRow (c : cs) ReadNegativeFreeMember (k : ks) var_names
  | isNum c = do
    repl_k <- maybe
      (Left ("Invalid coefficient " ++ [c]))
      (\c_int -> Right (c_int * (sign k)))
      (atoi c)
    parseRow cs ReadPositiveFreeMember (repl_k : ks) var_names
  | otherwise = Left ("Invalid equation (state: ReadNegativeFreeMember; coefficients: " ++ show (k : ks) ++ "; var_names: " ++ show var_names ++ "; current char: `" ++ (show c) ++ "`)")

parseRow (c : cs) ReadPositiveFreeMember (k : ks) var_names
  | isNum c = do
    new_k <- maybe
      (Left ("Invalid coefficient " ++ [c]))
      (\c_int -> Right ((k * 10) + ((sign k) * c_int)))
      (atoi c)
    parseRow cs ReadPositiveFreeMember (new_k : ks) var_names
  | otherwise = Left ("Invalid equation (state: ReadPositiveFreeMember; coefficients: " ++ show (k : ks) ++ "; var_names: " ++ show var_names ++ "; current char: `" ++ (show c) ++ "`)")

parseRow (c : cs) state coefficients var_names = parseRow cs state coefficients var_names

-- | Parse a string into equation system.
-- This first removes all spaces from the input line (so we don't have to bother further down the line)
-- and runs a state machine with the initial state.
parseEquationLine :: String -> Either String ([Integer], [String])
parseEquationLine line = parseRow (filter (not . isSpace) line) LineStart [] []

-- | Remove duplicates from a list
-- This utilizes the `foldl` with `any` underneath.
-- Thus this is not a fastest implementation, but since I did not want to use any third-party modules, ¯\_(ツ)_/¯
-- A __much__ faster implementation would be basically just constructing a Set out of a list and then spreading it back to a list.
unique :: Eq a => [a] -> [a]
unique elts = foldl (\acc elt -> if any (== elt) acc then acc else (elt : acc)) [] elts

-- | Parse a single string into equation and, if successful, fill out the accumulated values.
-- Currently the whole conversion is nothing but running a `parseEquationLine`
-- and mapping its result onto a tuple of coefficients and variable names.
-- This is actually the bottleneck, which implies a constraint on user input - all variables must follow
-- the same order and no missed variables are allowed.
parseEquationLines' :: ([[Integer]], [String]) -> String -> Either String ([[Integer]], [String])
parseEquationLines' (coefficients, varNames) line =
  fmap
    (\(ks, vars) -> ((ks : coefficients), unique (vars ++ varNames)))
    (parseEquationLine line)

-- | Parses a list of strings into a system of equations
-- This runs a functor on a list of lines with initial state.
-- It is essentially just a nifty `foldl` on `Either`'s.
parseEquationLines :: [String] -> Either String ([[Integer]], [String])
parseEquationLines lines = foldM parseEquationLines' ([], []) lines
