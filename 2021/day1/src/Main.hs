import Text.Printf

stringToInt :: String -> Int
stringToInt s = read s :: Int

countIncrease :: Bool -> Int
countIncrease True = 1
countIncrease False = 0

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [value] = 0
countIncreases values = countIncrease ((values !! 1) > head values) + countIncreases (drop 1 values)

main = do
  content <- readFile "./input.txt"
  let chars = lines content
  let values = map stringToInt chars

  let numIncreases = countIncreases values

  printf "Task 1: numIncreases=%d\n" numIncreases
