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

-- ------------------------------------------------------------------------

deepEqual :: Eq a => [a] -> Bool
deepEqual (h : ls) = all (== h) ls

split :: (Char -> Bool) -> String -> [String]
split _ [] = []
split check s = word : split check rs
  where
    word = takeWhile (not . check) s
    wo_word = dropWhile (not . check) s
    rs = dropWhile check wo_word

parseStdin :: [String] -> ([[Integer]], [String])
parseStdin lines = if inequal_var_names then error error_msg else (coefficients, var_names)
  where
    ls = filter ((> 1) . length) lines
    ls_parsed = map parseEquationLine ls
    coefficients = map fst ls_parsed
    all_var_names = map snd ls_parsed
    var_names = head $ map snd ls_parsed
    inequal_var_names = not $ deepEqual all_var_names
    error_msg = "Invalid variables in equations. Maybe, you should provide some missed variables with coefficient 0? (" ++ show all_var_names ++ ")"

main :: IO ()
main = do
  putStrLn "This app solves systems of linear equations."
  putStrLn "Please enter a system - equation per line, in form of:"
  putStrLn "\tK_iV_i + K_i+1Vi+1 = K_n"
  putStrLn "where K_i, K_i+1 and K_n are numbers and V_i and V_i+1 are variable names (a-z0-9)."
  putStrLn "End the input by entering an empty line."
  lines <- getInput
  putStrLn $ uncurry gaussSolve (parseStdin lines)
