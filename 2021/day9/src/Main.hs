import Data.List.Split
import Text.Printf
import Data.List.Extra (sortBy)

inputFile = "./input.txt"

toInt :: Char -> Int
toInt s = read [s] :: Int

getHeightMap :: [String] -> [[Int]]
getHeightMap = map (map toInt)

type Point = (Int, Int)

reduceLocalMinimaAtTop :: [[Int]] -> Int -> [Point] -> [Point]
reduceLocalMinimaAtTop heightMap x red
  | x == 0
    && head (heightMap !! 1) > head (head heightMap)
    && head heightMap !! 1 > head (head heightMap) =
      reduceLocalMinimaAtTop heightMap (x+1) ((px,py) : red)

  | x > 0 && x < length (head heightMap) - 1
    && head heightMap !! (x-1) > head heightMap !! x
    && head heightMap !! (x+1) > head heightMap !! x
    && heightMap !! 1 !! x > head heightMap !! x =
      reduceLocalMinimaAtTop heightMap (x+1) ((px,py) : red)

  | x == (length (head heightMap) - 1)
    && head heightMap !! (x-1) > head heightMap !! x
    && (heightMap !! 1) !! x > head heightMap !! x =
      reduceLocalMinimaAtTop heightMap (x+1) ((px,py) : red)

  | x > length (head heightMap) - 1 = red
  | otherwise = reduceLocalMinimaAtTop heightMap (x+1) red

  where (px,py) = (x, 0)

reduceLocalMinimaAtBottom :: [[Int]] -> Int -> [Point] -> [Point]
reduceLocalMinimaAtBottom heightMap x red
  | x == 0
    && head (last (init heightMap)) > head (last heightMap)
    && last heightMap !! 1 > head (last heightMap) =
      reduceLocalMinimaAtBottom heightMap (x+1) ((px,py) : red)

  | x > 0 && x < length (head heightMap) - 1
    && last heightMap !! (x-1) > last heightMap !! x
    && last heightMap !! (x+1) > last heightMap !! x
    && last (init heightMap) !! x > last heightMap !! x =
      reduceLocalMinimaAtBottom heightMap (x+1) ((px,py) : red)

  | x == (length (head heightMap) - 1)
    && last heightMap !! (x-1) > last heightMap !! x
    && last (init heightMap) !! x > last heightMap !! x =
      reduceLocalMinimaAtBottom heightMap (x+1) ((px,py) : red)

  | x > length (head heightMap) - 1 = red
  | otherwise = reduceLocalMinimaAtBottom heightMap (x+1) red

  where (px,py) = (x, length heightMap - 1)

reduceLocalMinimaAtMiddle :: [[Int]] -> (Int, Int) -> [Point] -> [Point]
reduceLocalMinimaAtMiddle heightMap (x,y) red
  | x == 0
    && head (heightMap !! (y-1)) > head (heightMap !! y)
    && head (heightMap !! (y+1)) > head (heightMap !! y)
    && (heightMap !! y) !! 1 > head (heightMap !! y) =
      reduceLocalMinimaAtMiddle heightMap (x+1,y) ((x,y) : red)

  | x > 0 && x < length (head heightMap) - 1
    && (heightMap !! (y-1)) !! x > (heightMap !! y) !! x
    && (heightMap !! (y+1)) !! x > (heightMap !! y) !! x
    && (heightMap !! y) !! (x-1) > (heightMap !! y) !! x
    && (heightMap !! y) !! (x+1) > (heightMap !! y) !! x =
      reduceLocalMinimaAtMiddle heightMap (x+1,y) ((x,y) : red)

  | x == (length (head heightMap) - 1)
    && (heightMap !! (y-1)) !! x > (heightMap !! y) !! x
    && (heightMap !! (y+1)) !! x > (heightMap !! y) !! x
    && (heightMap !! y) !! (x-1) > (heightMap !! y) !! x =
      reduceLocalMinimaAtMiddle heightMap (x+1,y) ((x,y) : red)

  | x > length (head heightMap) - 1 = red
  | otherwise = reduceLocalMinimaAtMiddle heightMap (x+1,y) red

reduceLocalMinima :: [[Int]] -> Int -> [Point] -> [Point]
reduceLocalMinima heightMap y red
  | y == 0 = reduceLocalMinima heightMap (y+1) (reduceLocalMinimaAtTop heightMap 0 red)
  | y < (length heightMap - 1) = reduceLocalMinima heightMap (y+1) (reduceLocalMinimaAtMiddle heightMap (0,y) red)
  | otherwise = reduceLocalMinimaAtBottom heightMap 0 red

getLocalMinima :: [[Int]] -> [Point]
getLocalMinima heightMap = reduceLocalMinima heightMap 0 []

getValueAtPoint :: [[Int]] -> Point -> Int
getValueAtPoint heightMap (x,y) = (heightMap !! y) !! x

getRiskLevel :: [[Int]] -> Point -> Int
getRiskLevel heightMap point = getValueAtPoint heightMap point + 1

sumRiskLevels :: [[Int]] -> Int
sumRiskLevels heightMap = sum (map (getRiskLevel heightMap) (getLocalMinima heightMap))

isPointEqual :: Point -> Point -> Bool
isPointEqual (x1,y1) (x2,y2) = x1 == x2 && y1 == y2

isStepUpPoint :: [[Int]] -> Point -> Point -> Bool
isStepUpPoint heightMap from point = pointValue < 9
  && not (isPointEqual from point)
  && pointValue >= fromValue
  where
    pointValue = getValueAtPoint heightMap point
    fromValue = getValueAtPoint heightMap from

getPointsAroundPoint :: [[Int]] -> Point -> [Point]
getPointsAroundPoint heightMap (x,y)
  | isTop && isLeft = [(x+1,y), (x,y+1)]
  | isTop && isRight = [(x-1,y), (x,y+1)]
  | isBottom && isLeft = [(x+1,y), (x,y-1)]
  | isBottom && isRight = [(x-1,y), (x,y-1)]
  | isTop = [(x-1,y), (x,y+1), (x+1,y)]
  | isBottom = [(x-1,y), (x,y-1), (x+1,y)]
  | isLeft = [(x,y-1), (x+1,y), (x,y+1)]
  | isRight = [(x,y-1), (x-1,y), (x,y+1)]
  | otherwise = [(x-1,y), (x,y-1), (x,y+1), (x+1,y)]
  where
    isTop = y == 0
    isBottom = y == length heightMap - 1
    isLeft = x == 0
    isRight = x == length (head heightMap) - 1

getSurroundingStepUpPoints :: [[Int]] -> Point -> [Point]
getSurroundingStepUpPoints heightMap (mx,my) =
  filter (isStepUpPoint heightMap (mx,my))
    (getPointsAroundPoint heightMap (mx,my))

pointsContainPoint :: [Point] -> Point -> Bool
pointsContainPoint [] _ = False
pointsContainPoint ((px,py):ps) (x,y)
  | px == x && py == y = True
  | otherwise = pointsContainPoint ps (x,y)

filterUnique :: [Point] -> [Point]
filterUnique [] = []
filterUnique (x:xs)
  | pointsContainPoint xs x = filterUnique xs
  | otherwise = x : filterUnique xs

notIn :: [Point] -> [Point] -> [Point]
notIn parent [] = []
notIn parent (c:children)
  | not (any (isPointEqual c) parent) = c : notIn parent children
  | otherwise = notIn parent children

getBasinLoop :: [[Int]] -> [Point] -> Point -> [Point]
getBasinLoop heightMap red (mx,my) =
  nextRed ++ concatMap (getBasinLoop heightMap nextRed) (notIn red surroundingPoints)
  where
    surroundingPoints = getSurroundingStepUpPoints heightMap (mx,my)
    nextRed = (mx,my) : surroundingPoints

getBasin :: [[Int]] -> Point -> [Point]
getBasin heightMap localMinimum = filterUnique (getBasinLoop heightMap [localMinimum] localMinimum)

getAllBasins :: [[Int]] -> [[Point]]
getAllBasins heightMap = map (getBasin heightMap) (getLocalMinima heightMap)

getThreeLargestBasins :: [[Int]] -> [[Point]]
getThreeLargestBasins heightMap = take 3
  (sortBy (\ a b -> compare (length b) (length a)) (getAllBasins heightMap))

productLength :: [[Point]] -> Int
productLength = foldr ((*) . length) 1

getThreeLargestBasinsProduct :: [[Int]] -> Int
getThreeLargestBasinsProduct heightMap = productLength (getThreeLargestBasins heightMap)

task1 :: [[Int]] -> IO ()
task1 heightMap = do
  let sumOfRiskLevels = sumRiskLevels heightMap
  printf "Task 1: sumOfRiskLevels=%d\n" sumOfRiskLevels

task2 :: [[Int]] -> IO ()
task2 heightMap = do
  let productOfBasinSizes = getThreeLargestBasinsProduct heightMap
  printf "Task 2: productOfBasinSizes=%d\n" productOfBasinSizes

main = do
  content <- readFile inputFile
  let chars = lines content
  let heightMap = getHeightMap chars

  task1 heightMap
  task2 heightMap
