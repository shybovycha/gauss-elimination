import Control.Monad (foldM)
import Data.Char (isSpace)
import EquationParser (parseEquationLine)
import Gauss (gaussSolve)

-- | Removes blank characters (spaces) from the beginning and the end of the string
strip ::
  -- | Input string
  String ->
  -- | String with space characters stripped
  String
strip = f . f
  where
    f = reverse . (dropWhile isSpace)

-- | Reads non-blank lines from STDIN and puts them into a list
readStdinLinesUlessBlank ::
  -- | Non-blank lines read so far
  IO [String] ->
  -- | Non-blank lines read
  IO [String]
readStdinLinesUlessBlank accumulatedLinesIO = do
  line <- fmap strip getLine
  if length line < 1
    then accumulatedLinesIO
    else readStdinLinesUlessBlank (fmap (++ [line]) accumulatedLinesIO)

-- | Read non-blank lines from STDIN
getInput :: IO [String]
getInput = readStdinLinesUlessBlank (return [])

-- | Remove duplicates from a list
unique :: Eq a => [a] -> [a]
unique elts = foldl (\acc elt -> if any (== elt) acc then acc else (elt : acc)) [] elts

-- | Parses a list of strings into a system of equations
parseInput :: [String] -> Either String ([[Integer]], [String])
parseInput lines = foldM parseInput' ([], []) lines

-- | Parse a string into equation and, if successful, fill out the accumulated values
parseInput' :: ([[Integer]], [String]) -> String -> Either String ([[Integer]], [String])
parseInput' (coefficients, varNames) line =
  fmap
    (\(ks, vars) -> ((ks : coefficients), unique (vars ++ varNames)))
    (parseEquationLine line)

solveSystem :: ([[Integer]], [String]) -> IO String
solveSystem (coefficients, varNames) = return (gaussSolve coefficients varNames)

printHelp :: IO ()
printHelp = do
  putStrLn "This app solves systems of linear equations."
  putStrLn "Please enter a system - equation per line, in form of:"
  putStrLn "\tK_iV_i + K_i+1Vi+1 = K_n"
  putStrLn "where K_i, K_i+1 and K_n are numbers and V_i and V_i+1 are variable names (a-z0-9)."
  putStrLn "End the input by entering an empty line."

main :: IO ()
main = do
  printHelp
  lines <- getInput
  solution <- either (return) (solveSystem) (parseInput lines)
  putStrLn $ solution
