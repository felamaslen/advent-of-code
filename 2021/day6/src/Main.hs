import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

toInt :: String -> Int
toInt s = read s :: Int

maxFish = 8

initNumOfEachN :: Int -> [Int]
initNumOfEachN n
  | n == 0 = []
  | otherwise = 0 : initNumOfEachN (n - 1)

getNumOfEachNLoop :: [Int] -> [Int] -> [Int]
getNumOfEachNLoop [] red = red
getNumOfEachNLoop (f:fish) red
  | f == 0 = getNumOfEachNLoop fish (head red + 1 : drop 1 red)
  | otherwise = getNumOfEachNLoop fish (take f red ++ [(red !! f) + 1] ++ drop (f + 1) red)

getNumOfEachN :: [Int] -> [Int]
getNumOfEachN fish = getNumOfEachNLoop fish (initNumOfEachN (maxFish + 1))

growFish :: [Int] -> [Int]
growFish numOfEachN = [
    numOfEachN !! 1,
    numOfEachN !! 2,
    numOfEachN !! 3,
    numOfEachN !! 4,
    numOfEachN !! 5,
    numOfEachN !! 6,
    numOfEachN !! 7 + head numOfEachN,
    numOfEachN !! 8,
    head numOfEachN
  ]

growLoop :: Int -> [Int] -> [Int]
growLoop daysLeft numOfEachN
  | daysLeft == 0 = numOfEachN
  | otherwise = growLoop (daysLeft - 1) (growFish numOfEachN)

countFishLoop :: [Int] -> Int -> Int
countFishLoop numOfEachN index
  | index == 0 = head numOfEachN
  | otherwise = numOfEachN !! index + countFishLoop numOfEachN (index - 1)

countFish :: [Int] -> Int
countFish numOfEachN = countFishLoop numOfEachN maxFish

growFishForNDays :: [Int] -> Int -> Int
growFishForNDays fish numDays = countFish (growLoop numDays (getNumOfEachN fish))

task1 :: [Int] -> IO ()
task1 fish = do
  let finalNumFish = growFishForNDays fish 80
  printf "Task 1: finalNumFish=%d\n" finalNumFish

task2 :: [Int] -> IO ()
task2 fish = do
  let finalNumFish = growFishForNDays fish 256
  printf "Task 2: finalNumFish=%d\n" finalNumFish

main = do
  content <- readFile inputFile
  let chars = lines content
  let initialFish = map toInt (splitOn "," (head chars))

  task1 initialFish
  task2 initialFish
