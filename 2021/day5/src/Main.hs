import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

type Line = ((Int, Int), (Int, Int))

toInt :: String -> Int
toInt s = read s :: Int

processCoord :: String -> [Int]
processCoord coord = map toInt (splitOn "," coord)

coordToTuple :: [Int] -> (Int, Int)
coordToTuple coord = (head coord, coord !! 1)

getCoord :: String -> Int -> (Int, Int)
getCoord line index = coordToTuple (processCoord (splitOn " -> " line !! index))

processLine :: String -> Line
processLine line = (getCoord line 0, getCoord line 1)

getLines :: [String] -> [Line]
getLines = map processLine

isLineVertical :: Line -> Bool
isLineVertical ((x1,y1), (x2,y2)) = x1 == x2

isLineHorizontal :: Line -> Bool
isLineHorizontal ((x1,y1), (x2,y2)) = y1 == y2

isLineStraight :: Line -> Bool
isLineStraight line = isLineVertical line || isLineHorizontal line

filterStraightLines :: [Line] -> [Line]
filterStraightLines [] = []
filterStraightLines (x:xs)
  | isLineStraight x = x : filterStraightLines xs
  | otherwise = filterStraightLines xs

getVerticalLinePoints :: Int -> Int -> Int -> [(Int, Int)]
getVerticalLinePoints x y1 y2
  | y1 > y2 = []
  | y1 == y2 = [(x, y1)]
  | otherwise = (x, y1) : getVerticalLinePoints x (y1 + 1) y2

getHorizontalLinePoints :: Int -> Int -> Int -> [(Int, Int)]
getHorizontalLinePoints y x1 x2
  | x1 > x2 = []
  | x1 == x2 = [(x1, y)]
  | otherwise = (x1, y) : getHorizontalLinePoints y (x1 + 1) x2

getPoints :: Line -> [(Int, Int)]
getPoints ((x1,y1), (x2,y2))
  | x1 == x2 = getVerticalLinePoints x1 (min y1 y2) (max y1 y2)
  | y1 == y2 = getHorizontalLinePoints y1 (min x1 x2) (max x1 x2)
  | otherwise = error "getPoints called with non-horizontal line"

getCollisionsBetweenVerticalLines :: Line -> Line -> [(Int, Int)]
getCollisionsBetweenVerticalLines ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2))
  | ax1 == bx1 = getVerticalLinePoints
    ax1
    (max (min ay1 ay2) (min by1 by2))
    (min (max ay1 ay2) (max by1 by2))
  | otherwise = []

getCollisionsBetweenHorizontalLines :: Line -> Line -> [(Int, Int)]
getCollisionsBetweenHorizontalLines ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2))
  | ay1 == by1 = getHorizontalLinePoints
    ay1
    (max (min ax1 ax2) (min bx1 bx2))
    (min (max ax1 ax2) (max bx1 bx2))
  | otherwise = []

-- First line is vertical, second is horizontal
getCollisionBetweenCrossedLines :: Line -> Line -> [(Int, Int)]
getCollisionBetweenCrossedLines ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2))
  | ax1 >= min bx1 bx2 && ax1 <= max bx1 bx2 && by1 >= min ay1 ay2 && by1 <= max ay1 ay2 =
    [(ax1, by1)]
  | otherwise = []

getCollisionsBetweenLines :: Line -> Line -> [(Int, Int)]
getCollisionsBetweenLines main compare
  | isLineVertical main && isLineVertical compare = getCollisionsBetweenVerticalLines main compare
  | isLineHorizontal main && isLineHorizontal compare = getCollisionsBetweenHorizontalLines main compare
  | isLineVertical main && isLineHorizontal compare = getCollisionBetweenCrossedLines main compare
  | isLineHorizontal main && isLineVertical compare = getCollisionBetweenCrossedLines compare main
  | otherwise = []

aContainsB :: [(Int, Int)] -> (Int, Int) -> Bool
aContainsB [] (bx, by) = False
aContainsB ((ax, ay):as) (bx, by)
  | ax == bx && ay == by = True
  | otherwise = aContainsB as (bx, by)

mergeUnique :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mergeUnique a [] = a
mergeUnique a (b:bs)
  | aContainsB a b = mergeUnique a bs
  | otherwise = b : mergeUnique a bs

reduceLinesCollisionsWithCompare :: Line -> Line -> [(Int, Int)] -> [(Int, Int)]
reduceLinesCollisionsWithCompare main compare reduction =
  mergeUnique reduction (getCollisionsBetweenLines main compare)

reduceLinesCollisionsWithRest :: Line -> [Line] -> [(Int, Int)] -> [(Int, Int)]
reduceLinesCollisionsWithRest mainLine compareLines reduction
  | null compareLines = reduction
  | otherwise = reduceLinesCollisionsWithRest mainLine (drop 1 compareLines)
    (reduceLinesCollisionsWithCompare mainLine (head compareLines) reduction)

reduceCollisionsLoop :: [Line] -> [(Int, Int)] -> [(Int, Int)]
reduceCollisionsLoop [] reduction = reduction
reduceCollisionsLoop (l:ls) reduction = reduceCollisionsLoop ls (reduceLinesCollisionsWithRest l ls reduction)

countCollisions :: [Line] -> Int
countCollisions straightLines = length (reduceCollisionsLoop straightLines [])

task1 :: [Line] -> IO ()
task1 lines = do
  let straightLines = filterStraightLines lines
  let collisions = countCollisions straightLines

  printf "Task 1: collisions=%d\n" collisions

main = do
  content <- readFile inputFile
  let chars = lines content

  let lines = getLines chars

  task1 lines
  -- task2 order boards

