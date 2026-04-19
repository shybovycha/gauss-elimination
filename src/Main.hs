import Data.Char (isSpace)
import EquationParser (parseEquationSystem)
import Fraction
import Gauss (gaussSolve, convertEquationToMatrix)

-- | Removes blank characters (spaces) from the beginning and the end of the string
strip ::
  -- | Input string
  String ->
  -- | String with space characters stripped
  String
strip = f . f
  where
    f = reverse . (dropWhile isSpace)

-- | Read non-blank lines from STDIN
getInput :: IO [String]
getInput = takeWhile (not . null) . map strip . lines <$> getContents

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
  input <- getInput
  putStrLn $ case parseEquationSystem input of
    Nothing -> "Could not parse input"
    Just system -> uncurry gaussSolve (convertEquationToMatrix system)
