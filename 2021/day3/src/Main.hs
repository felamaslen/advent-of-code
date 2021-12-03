import Data.Bits
import Text.Printf

inputFile = "./input.txt"

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

mapBinaryCharLine :: String -> Int
mapBinaryCharLine [] = 0
mapBinaryCharLine (x:xs) = (read [x] :: Int) * (2 ^ length xs) + mapBinaryCharLine xs

numOneNthBits :: [Int] -> Int -> Int
numOneNthBits xs index = foldr
  (\ x -> (+) (boolToInt (testBit x index))) 0 xs

one = 1
zero = 0
neither = -1

flipMostCommon :: Int -> Int
flipMostCommon v
  | v == one = zero
  | v == zero = one
  | otherwise = v

mostCommon :: Int -> Int -> Int
mostCommon numValues numOnes
  | numOnes > numValues - numOnes = one
  | numOnes < numValues - numOnes = zero
  | otherwise = neither

mostCommonNthBit :: [Int] -> Int -> Int
mostCommonNthBit values index = mostCommon
  (length values)
  (numOneNthBits values index)

leastCommonNthBit :: [Int] -> Int -> Int
leastCommonNthBit values index = flipMostCommon (mostCommonNthBit values index)

setOrClearBit :: Int -> Int -> Int -> Int
setOrClearBit value i oneOrZero
  | oneOrZero == 0 = clearBit value i
  | oneOrZero == 1 = setBit value i
  | otherwise = value

constructNthBitLoop :: ([Int] -> Int -> Int) -> [Int] -> Int -> Int -> Int
constructNthBitLoop getNthBit values index result
  | index == 0 = setOrClearBit result 0 (getNthBit values 0)
  | index > 0 = setOrClearBit (
    constructNthBitLoop getNthBit values (index - 1) result
  ) index (getNthBit values index)

constructNthBitValue :: ([Int] -> Int -> Int) -> [Int] -> Int -> Int
constructNthBitValue getNthBit values numBits = constructNthBitLoop getNthBit values (numBits - 1) 0

constructMostCommon = constructNthBitValue mostCommonNthBit
constructLeastCommon = constructNthBitValue leastCommonNthBit

filterByNthBit :: [Int] -> Int -> Int -> [Int]
filterByNthBit [] index nthBit = []
filterByNthBit (x:xs) index nthBit
  | boolToInt (testBit x index) == nthBit = x : filterByNthBit xs index nthBit
  | otherwise = filterByNthBit xs index nthBit

negateBin :: Int -> Int
negateBin 0 = 1
negateBin 1 = 0

filterByNthBitValue :: ([Int] -> Int -> Int) -> Int -> [Int] -> Int -> [Int]
filterByNthBitValue getNthBit testedValue values index
  | getNthBit values index == testedValue = filterByNthBit values index testedValue
  | otherwise = filterByNthBit values index (negateBin testedValue)

eliminateByNthBitValue :: ([Int] -> Int -> Int) -> Int -> [Int] -> Int -> Int
eliminateByNthBitValue getNthBit testedValue values numBits
  | length values == 1 = head values
  | otherwise = eliminateByNthBitValue getNthBit testedValue (filterByNthBitValue getNthBit testedValue values (numBits - 1)) (numBits - 1)

eliminateByMostCommon = eliminateByNthBitValue mostCommonNthBit 0
eliminateByLeastCommon = eliminateByNthBitValue leastCommonNthBit 1

task1 :: [Int] -> Int -> IO ()
task1 values numBits = do
  let gamma = constructMostCommon values numBits
  let epsilon = constructLeastCommon values numBits

  let powerConsumption = gamma * epsilon

  printf "Task 1: gamma=%d, epsilon=%d, powerConsumption=%d\n" gamma epsilon powerConsumption

task2 :: [Int] -> Int -> IO ()
task2 values numBits = do
  let oxygenGeneratorRating = eliminateByMostCommon values numBits
  let co2ScrubberRating = eliminateByLeastCommon values numBits

  let lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating

  printf "Task 2: oxygenGeneratorRating=%d, co2ScrubberRating=%d, lifeSupportRating=%d\n" oxygenGeneratorRating co2ScrubberRating lifeSupportRating

main = do
  content <- readFile inputFile
  let chars = lines content
  let numBits = length (head chars)
  let values = map mapBinaryCharLine chars

  task1 values numBits
  task2 values numBits
