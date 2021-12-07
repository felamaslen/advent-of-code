import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

toInt :: String -> Int
toInt s = read s :: Int

getMoveCost :: [Int] -> Int -> Int
getMoveCost crabs pos = foldr (\ c -> (+) (abs (c - pos))) 0 crabs

getCheapestPossiblePositionLoop :: [Int] -> (Int, Int) -> Int -> (Int, Int)
getCheapestPossiblePositionLoop crabs (prevPos, prevCost) pos
  | pos < 0 = (prevPos, prevCost)
  | getMoveCost crabs pos < prevCost =
    getCheapestPossiblePositionLoop crabs (pos, getMoveCost crabs pos) (pos - 1)
  | otherwise = getCheapestPossiblePositionLoop crabs (prevPos, prevCost) (pos - 1)

getMaxPosition :: [Int] -> Int
getMaxPosition = foldr max 0

getCheapestPossiblePosition :: [Int] -> (Int, Int)
getCheapestPossiblePosition crabs = getCheapestPossiblePositionLoop crabs
  (getMaxPosition crabs, getMoveCost crabs (getMaxPosition crabs))
  (getMaxPosition crabs)

task1 :: [Int] -> IO ()
task1 crabs = do
  let (cheapestPos, cheapestCost) = getCheapestPossiblePosition crabs
  printf "Task 1: position=%d, cost=%d\n" cheapestPos cheapestCost

main = do
  content <- readFile inputFile
  let chars = lines content
  let crabs = map toInt (splitOn "," (head chars))

  task1 crabs
