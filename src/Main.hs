import Data.Char (isSpace)
import EquationParser (parseEquationLines)
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

solveSystem :: ([[Integer]], [String]) -> IO String
solveSystem input = return (uncurry gaussSolve $ input)

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
  solution <- either (return) (solveSystem) (parseEquationLines lines)
  putStrLn $ solution
