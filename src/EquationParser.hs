{--|
  Parser for the user input. Parses single lines into equations and lists of lines into equation systems.
--}

module EquationParser where

import Control.Monad (foldM, sequence)

import Fraction
import Map

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

parseDigit :: Char -> state -> Either (ParserError [Char] state) Integer
parseDigit c state = maybe
  (Left (InternalParserError ("Can not parse digit " ++ [c]) state))
  (Right)
  (atoi c)

-- | Parse equation string. Returns an `Either` of a `ParserError` with context or `EquationParams`
parseRow ::
  -- | string to parse
  String ->
  -- | current parser state
  (ParserState (EquationParams [Integer] [String])) ->
  -- | a tuple of coefficients and variable names
  Either (ParserError String (ParserState (EquationParams [Integer] [String]))) (EquationParams [Integer] [String])

parseRow [] (ReadPositiveFreeMember equation) = Right equation
parseRow [] state = Left (InvalidEndStateError "Parser reached the end of a line with invalid state" state)

parseRow (c : cs) (LineStart (Equation coefficients var_names))
  | (c == '-') = parseRow cs (ReadElement (Equation ((-1) : coefficients) var_names))
  | isNum c = do
    c_int <- parseDigit c (LineStart (Equation coefficients var_names))
    parseRow cs (ReadCoefficient (Equation (c_int : coefficients) var_names))
  | isAlpha c = parseRow cs (ReadSymbol (Equation (1 : coefficients) ([c] : var_names)))

parseRow (c : cs) (ReadElement (Equation (k : ks) var_names))
  | isNum c = do
    c_int <- parseDigit c (ReadElement (Equation (k : ks) var_names))
    parseRow cs (ReadCoefficient (Equation ((c_int * (sign k)) : ks) var_names))
  | isAlpha c = parseRow cs (ReadSymbol (Equation (k : ks) ([c] : var_names)))

parseRow (c : cs) (ReadCoefficient (Equation (k : ks) var_names))
  | (c == '=') = parseRow cs (ReadRightSide (Equation (k : ks) var_names))
  | isAlpha c = parseRow cs (ReadSymbol (Equation (k : ks) ([c] : var_names)))
  | isNum c = do
    c_int <- parseDigit c (ReadCoefficient (Equation (k : ks) var_names))
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
    c_int <- parseDigit c (ReadRightSide (Equation coefficients var_names))
    parseRow cs (ReadPositiveFreeMember (Equation (c_int : coefficients) var_names))

parseRow (c : cs) (ReadNegativeFreeMember (Equation (k : ks) var_names))
  | isNum c = do
    c_int <- parseDigit c (ReadNegativeFreeMember (Equation (k : ks) var_names))
    parseRow cs (ReadPositiveFreeMember (Equation ((c_int * (sign k)) : ks) var_names))

parseRow (c : cs) (ReadPositiveFreeMember (Equation (k : ks) var_names))
  | isNum c = do
    c_int <- parseDigit c (ReadPositiveFreeMember (Equation (k : ks) var_names))
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
  (parseRow lineWithNoSpaces initialParserState)
  where
    lineWithNoSpaces = filter (not . isSpace) line

-- | Returns a mapping from a variable name to its coefficient. Skips free equation member.
getVariablesMapping :: ([Integer], [String]) -> Map String Integer
getVariablesMapping (coefficients, variables) = foldl (\acc (variable, coefficient) -> put acc variable coefficient) emptyMap (zip variables (drop 1 coefficients))

-- | Smashes all the keys of all the maps provided into one list and removes duplicates.
-- This effectively gives a list of all the variables found in the equation system.
getVariableNames :: [Map String Integer] -> [String]
getVariableNames mappings = foldl (\acc mapping -> unique (acc ++ (keys mapping))) [] mappings

-- | Takes the list of existing variables and fills the mapping with missing ones. Defaults missing variables' coefficients to 0.
fillVariablesMapping :: [String] -> Map String Integer -> Map String Integer
fillVariablesMapping variables mapping = foldl (\acc variable -> if not (containsKey acc variable) then put acc variable 0 else acc) mapping variables

-- | This is somewhat complex function - it does quite a few manipulations in order to get things right for the solver:
--
-- 1. parse the equations into a tuple of coefficients and variable names
-- 2. get all variable names registered in the system
-- 3. add the missing variables with 0 coefficient to every equation that misses them
-- 4. put every coefficient in the same order as variables in the `variables` list
-- 5. get the free member of each equation and append it *to the end* of each list of coefficients
-- 6. convert everything back into a tuple of coefficients and variable names
--
-- This way all coefficients and variable names will be aligned and the gaps will be filled so that
-- the whole system is ready to be passed to the solver.
parseSystem :: [String] -> Either String ([[Integer]], [String])
parseSystem lines = do
  equationsParams <- sequence $ map parseEquationLine lines
  let partialMappings = map getVariablesMapping equationsParams
  let variables = getVariableNames partialMappings
  let fullMappings = map (fillVariablesMapping variables) partialMappings
  let freeMembers = map ((take 1) . fst) equationsParams -- takes the free member for each equation, which is always the first element of coefficients element of a tuple
  let orderedCoefficientsMaybe = sequence $ map (\mapping -> sequence $ map (get mapping) variables) fullMappings
  orderedCoefficients <- maybe (Left "Coefficients do not match") (Right) orderedCoefficientsMaybe
  let coefficients = map (\(freeMember, rest) -> rest ++ freeMember) (zip freeMembers orderedCoefficients)
  return (coefficients, variables)

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
