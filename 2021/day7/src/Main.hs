import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

toInt :: String -> Int
toInt s = read s :: Int

getLinearMoveCost :: [Int] -> Int -> Int
getLinearMoveCost crabs pos = foldr (\ c -> (+) (abs (c - pos))) 0 crabs

linearSum :: Int -> Int
linearSum n = round (m / 2 * (m + 1)) where m = fromIntegral n :: Float

getExpensiveMoveCost :: [Int] -> Int -> Int
getExpensiveMoveCost crabs pos = foldr (\ c -> (+) (linearSum (abs (c - pos)))) 0 crabs

getCheapestPossiblePositionLoop :: ([Int] -> Int -> Int) -> [Int] -> (Int, Int) -> Int -> (Int, Int)
getCheapestPossiblePositionLoop getMoveCost crabs (prevPos, prevCost) pos
  | pos < 0 = (prevPos, prevCost)
  | getMoveCost crabs pos < prevCost =
    getCheapestPossiblePositionLoop getMoveCost crabs (pos, getMoveCost crabs pos) (pos - 1)
  | otherwise = getCheapestPossiblePositionLoop getMoveCost crabs (prevPos, prevCost) (pos - 1)

getCheapestPossiblePosition :: ([Int] -> Int -> Int) -> [Int] -> (Int, Int)
getCheapestPossiblePosition getMoveCost crabs = getCheapestPossiblePositionLoop getMoveCost crabs
  (getMaxPosition crabs, getMoveCost crabs (getMaxPosition crabs))
  (getMaxPosition crabs)

getMaxPosition :: [Int] -> Int
getMaxPosition = foldr max 0

getLinearCheapestPossiblePosition = getCheapestPossiblePosition getLinearMoveCost
getExpensiveCheapestPossiblePosition = getCheapestPossiblePosition getExpensiveMoveCost

task1 :: [Int] -> IO ()
task1 crabs = do
  let (cheapestPos, cheapestCost) = getLinearCheapestPossiblePosition crabs
  printf "Task 1: position=%d, cost=%d\n" cheapestPos cheapestCost

task2 :: [Int] -> IO ()
task2 crabs = do
  let (cheapestPos, cheapestCost) = getExpensiveCheapestPossiblePosition crabs
  printf "Task 2: position=%d, cost=%d\n" cheapestPos cheapestCost

main = do
  content <- readFile inputFile
  let chars = lines content
  let crabs = map toInt (splitOn "," (head chars))

  task1 crabs
  task2 crabs
