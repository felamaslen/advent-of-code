import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

toInt :: Char -> Int
toInt s = read [s] :: Int

getHeightMap :: [String] -> [[Int]]
getHeightMap = map (map toInt)

reduceLocalMinimaAtTop :: [[Int]] -> Int -> [Int] -> [Int]
reduceLocalMinimaAtTop heightMap x red
  | x == 0
    && head (heightMap !! 1) > head (head heightMap)
    && head heightMap !! 1 > head (head heightMap) =
      reduceLocalMinimaAtTop heightMap (x+1) (head heightMap !! x : red)

  | x > 0 && x < length (head heightMap) - 1
    && head heightMap !! (x-1) > head heightMap !! x
    && head heightMap !! (x+1) > head heightMap !! x
    && heightMap !! 1 !! x > head heightMap !! x =
      reduceLocalMinimaAtTop heightMap (x+1) (head heightMap !! x : red)

  | x == (length (head heightMap) - 1)
    && head heightMap !! (x-1) > head heightMap !! x
    && (heightMap !! 1) !! x > head heightMap !! x =
      reduceLocalMinimaAtTop heightMap (x+1) (head heightMap !! x : red)

  | x > length (head heightMap) - 1 = red
  | otherwise = reduceLocalMinimaAtTop heightMap (x+1) red

reduceLocalMinimaAtBottom :: [[Int]] -> Int -> [Int] -> [Int]
reduceLocalMinimaAtBottom heightMap x red
  | x == 0
    && head (last (init heightMap)) > head (last heightMap)
    && last heightMap !! 1 > head (last heightMap) =
      reduceLocalMinimaAtBottom heightMap (x+1) ((last heightMap !! x) : red)

  | x > 0 && x < length (head heightMap) - 1
    && last heightMap !! (x-1) > last heightMap !! x
    && last heightMap !! (x+1) > last heightMap !! x
    && last (init heightMap) !! x > last heightMap !! x =
      reduceLocalMinimaAtBottom heightMap (x+1) ((last heightMap !! x) : red)

  | x == (length (head heightMap) - 1)
    && last heightMap !! (x-1) > last heightMap !! x
    && last (init heightMap) !! x > last heightMap !! x =
      reduceLocalMinimaAtBottom heightMap (x+1) ((last heightMap !! x) : red)

  | x > length (head heightMap) - 1 = red
  | otherwise = reduceLocalMinimaAtBottom heightMap (x+1) red

reduceLocalMinimaAtMiddle :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
reduceLocalMinimaAtMiddle heightMap (x,y) red
  | x == 0
    && head (heightMap !! (y-1)) > head (heightMap !! y)
    && head (heightMap !! (y+1)) > head (heightMap !! y)
    && (heightMap !! y) !! 1 > head (heightMap !! y) =
      reduceLocalMinimaAtMiddle heightMap (x+1,y) (head (heightMap !! y) : red)

  | x > 0 && x < length (head heightMap) - 1
    && (heightMap !! (y-1)) !! x > (heightMap !! y) !! x
    && (heightMap !! (y+1)) !! x > (heightMap !! y) !! x
    && (heightMap !! y) !! (x-1) > (heightMap !! y) !! x
    && (heightMap !! y) !! (x+1) > (heightMap !! y) !! x =
      reduceLocalMinimaAtMiddle heightMap (x+1,y) ((heightMap !! y) !! x : red)

  | x == (length (head heightMap) - 1)
    && (heightMap !! (y-1)) !! x > (heightMap !! y) !! x
    && (heightMap !! (y+1)) !! x > (heightMap !! y) !! x
    && (heightMap !! y) !! (x-1) > (heightMap !! y) !! x =
      reduceLocalMinimaAtMiddle heightMap (x+1,y) ((heightMap !! y) !! x : red)

  | x > length (head heightMap) - 1 = red
  | otherwise = reduceLocalMinimaAtMiddle heightMap (x+1,y) red

reduceLocalMinima :: [[Int]] -> Int -> [Int] -> [Int]
reduceLocalMinima heightMap y red
  | y == 0 = reduceLocalMinima heightMap (y+1) (reduceLocalMinimaAtTop heightMap 0 red)
  | y < (length heightMap - 1) = reduceLocalMinima heightMap (y+1) (reduceLocalMinimaAtMiddle heightMap (0,y) red)
  | otherwise = reduceLocalMinimaAtBottom heightMap 0 red

getLocalMinima :: [[Int]] -> [Int]
getLocalMinima heightMap = reduceLocalMinima heightMap 0 []

getRiskLevel :: Int -> Int
getRiskLevel height = height + 1

sumRiskLevels :: [[Int]] -> Int
sumRiskLevels heightMap = sum (map getRiskLevel (getLocalMinima heightMap))

task1 :: [[Int]] -> IO ()
task1 heightMap = do
  let sumOfRiskLevels = sumRiskLevels heightMap
  printf "Task 1: sumOfRiskLevels=%d\n" sumOfRiskLevels

main = do
  content <- readFile inputFile
  let chars = lines content
  let heightMap = getHeightMap chars

  task1 heightMap
