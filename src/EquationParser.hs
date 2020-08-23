{--|
  Parser for the user input. Parses single lines into equations and lists of lines into equation systems.
--}

module EquationParser where

import Control.Monad (foldM)
import Control.Arrow (left)

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

data EquationParams coefficients variables = Equation coefficients variables deriving Show

data ParserError message state =
  GenericParserError message state |
  MissingStateTransitionError message state |
  InvalidEndStateError message state |
  InternalParserError message state
  deriving Show

-- | This is an enumeration of all possible parser states (as in state machine):
data ParserState equationParams
  = -- | start of the line
    LineStart equationParams
  | -- | read either a positive or negative number or symbol name
    ReadElement equationParams
  | -- | read number
    ReadCoefficient equationParams
  | -- | symbol name
    ReadSymbol equationParams
  | -- | after equal sign
    ReadRightSide equationParams
  | -- | number
    ReadNegativeFreeMember equationParams
  | -- | number
    ReadPositiveFreeMember equationParams
  deriving (Show, Eq)

-- | Parse equation string. Returns an `Either` of a `ParserError` with context or `EquationParams`
parseRow ::
  -- | string to parse
  String ->
  -- | current parser state
  (ParserState (EquationParams [Integer] [String])) ->
  -- | a tuple of coefficients and variable names
  Either (ParserError String (ParserState (EquationParams [Integer] [String]))) (EquationParams [Integer] [String])

parseDigit :: Char -> Either String Integer
parseDigit c = maybe
  (Left ("Can not parse digit " ++ [c]))
  (Right)
  (atoi c)

parseRow [] (ReadPositiveFreeMember equation) = Right equation
parseRow [] state = Left (InvalidEndStateError "Parser reached the end of a line with invalid state" state)

parseRow (c : cs) (LineStart (Equation coefficients var_names))
  | (c == '-') = parseRow cs (ReadElement (Equation ((-1) : coefficients) var_names))
  | isNum c = do
    c_int <- left (\msg -> InternalParserError msg (LineStart (Equation coefficients var_names))) (parseDigit c)
    parseRow cs (ReadCoefficient (Equation (c_int : coefficients) var_names))
  | isAlpha c = parseRow cs (ReadSymbol (Equation (1 : coefficients) ([c] : var_names)))

parseRow (c : cs) (ReadElement (Equation (k : ks) var_names))
  | isNum c = do
    c_int <- left (\msg -> InternalParserError msg (ReadElement (Equation (k : ks) var_names))) (parseDigit c)
    parseRow cs (ReadCoefficient (Equation ((c_int * (sign k)) : ks) var_names))
  | isAlpha c = parseRow cs (ReadSymbol (Equation (k : ks) ([c] : var_names)))

parseRow (c : cs) (ReadCoefficient (Equation (k : ks) var_names))
  | (c == '=') = parseRow cs (ReadRightSide (Equation (k : ks) var_names))
  | isAlpha c = parseRow cs (ReadSymbol (Equation (k : ks) ([c] : var_names)))
  | isNum c = do
    c_int <- left (\msg -> InternalParserError msg (ReadCoefficient (Equation (k : ks) var_names))) (parseDigit c)
    parseRow cs (ReadCoefficient (Equation (((k * 10) + c_int) : ks) var_names))

parseRow (c : cs) (ReadSymbol (Equation coefficients var_names))
  | (c == '-') = parseRow cs (ReadElement (Equation ((-1) : coefficients) var_names))
  | (c == '+') = parseRow cs (ReadElement (Equation (1 : coefficients) var_names))
  | (c == '=') = parseRow cs (ReadRightSide (Equation coefficients var_names))
  | isAlphaNum c && (null var_names) = parseRow cs (ReadSymbol (Equation coefficients [[c]]))
  | isAlphaNum c = parseRow cs (ReadSymbol (Equation coefficients (((head var_names) ++ [c]) : drop 1 var_names)))

parseRow (c : cs) (ReadRightSide (Equation coefficients var_names))
  | (c == '-') = parseRow cs (ReadNegativeFreeMember (Equation ((-1) : coefficients) var_names))
  | isNum c = do
    c_int <- left (\msg -> InternalParserError msg (ReadRightSide (Equation coefficients var_names))) (parseDigit c)
    parseRow cs (ReadPositiveFreeMember (Equation (c_int : coefficients) var_names))

parseRow (c : cs) (ReadNegativeFreeMember (Equation (k : ks) var_names))
  | isNum c = do
    c_int <- left (\msg -> InternalParserError msg (ReadNegativeFreeMember (Equation (k : ks) var_names))) (parseDigit c)
    parseRow cs (ReadPositiveFreeMember (Equation ((c_int * (sign k)) : ks) var_names))

parseRow (c : cs) (ReadPositiveFreeMember (Equation (k : ks) var_names))
  | isNum c = do
    c_int <- left (\msg -> InternalParserError msg (ReadPositiveFreeMember (Equation (k : ks) var_names))) (parseDigit c)
    parseRow cs (ReadPositiveFreeMember (Equation (((k * 10) + ((sign k) * c_int)) : ks) var_names))

parseRow (c : cs) state = Left (MissingStateTransitionError ("Can not determine parser state transition for character " ++ [c]) state)

extractParserError :: (Show t1, Show t2) => ParserError String (ParserState (EquationParams t1 t2)) -> String
extractParserError err = show err

extractParserResults :: EquationParams a b -> (a, b)
extractParserResults (Equation coefficients variables) = (coefficients, variables)

initialParserState :: ParserState (EquationParams [a1] [a2])
initialParserState = (LineStart (Equation [] []))

-- | Parse a string into equation system.
-- This first removes all spaces from the input line (so we don't have to bother further down the line)
-- and runs a state machine with the initial state.
parseEquationLine :: String -> Either String ([Integer], [String])
parseEquationLine line = either
  (Left . extractParserError)
  (Right . extractParserResults)
  (parseRow (filter (not . isSpace) line) initialParserState)

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
