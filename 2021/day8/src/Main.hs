import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

notEmpty :: String -> Bool
notEmpty a = not (null a)

getOutputValues :: String -> [String]
getOutputValues line = filter notEmpty (splitOn " " (last (splitOn "|" line)))

representsDigit :: Int -> String -> Bool
representsDigit d s
  | d == 1 = length s == 2
  | d == 4 = length s == 4
  | d == 7 = length s == 3
  | d == 8 = length s == 7
  | otherwise = False

countValuesOfDigit :: [String] -> Int -> Int
countValuesOfDigit outputValues digit = length (filter (representsDigit digit) outputValues)

task1 :: [String] -> IO ()
task1 outputValues = do
  let result = sum (map (countValuesOfDigit outputValues) [1, 4, 7, 8])

  printf "Task 1: result=%d\n" result

main = do
  content <- readFile inputFile
  let chars = lines content
  let outputValues = concatMap getOutputValues chars

  task1 outputValues

