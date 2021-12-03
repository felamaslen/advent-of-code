import Text.Printf

inputFile = "./input.txt"

stringToInt :: String -> Int
stringToInt s = read s :: Int

countIncrease :: Bool -> Int
countIncrease True = 1
countIncrease False = 0

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [value] = 0
countIncreases values = countIncrease ((values !! 1) > head values) + countIncreases (drop 1 values)

sumWindow :: Int -> [Int] -> Int
sumWindow n values
  | length values >= n = sum (take n values)
  | otherwise = 0

countWindowedIncreases :: Int -> [Int] -> Int
countWindowedIncreases n values
  | length values >= n =
    countIncrease (sumWindow n (drop 1 values) > sumWindow n values) + countWindowedIncreases n (drop 1 values)
  | otherwise = 0

task1 :: [Int] -> IO ()
task1 values = do
  let numIncreases = countIncreases values
  printf "Task 1: numIncreases=%d\n" numIncreases

task2 :: [Int] -> IO ()
task2 values = do
  let numWindowedIncreases = countWindowedIncreases 3 values
  printf "Task 2: numWindowedIncreases=%d\n" numWindowedIncreases

main = do
  content <- readFile inputFile
  let chars = lines content
  let values = map stringToInt chars

  task1 values
  task2 values
