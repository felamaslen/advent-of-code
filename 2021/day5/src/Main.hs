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

isLineDiagonal :: Line -> Bool
isLineDiagonal ((x1,y1), (x2,y2)) = abs (x1 - x2) == abs (y1 - y2)

filterDiagonalLines :: [Line] -> [Line]
filterDiagonalLines [] = []
filterDiagonalLines (x:xs)
  | isLineDiagonal x = x : filterDiagonalLines xs
  | otherwise = filterDiagonalLines xs

filterStraightAndDiagonalLines :: [Line] -> [Line]
filterStraightAndDiagonalLines allLines = filterStraightLines allLines ++ filterDiagonalLines allLines

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

diagDiff :: Int -> Int -> Int
diagDiff start end
  | start < end = 1
  | otherwise = -1

getDiagonalLinePointsLoop :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
getDiagonalLinePointsLoop sx sy ex ey dx dy
  | sx == ex || sy == ey = [(sx, sy)]
  | otherwise = (sx,sy) : getDiagonalLinePointsLoop (sx + dx) (sy + dy) ex ey dx dy

getDiagonalLinePoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
getDiagonalLinePoints sx sy ex ey = getDiagonalLinePointsLoop sx sy ex ey
  (diagDiff sx ex) (diagDiff sy ey)

getMaxX :: [Line] -> Int
getMaxX [] = 0
getMaxX (((lx1,ly1), (lx2,ly2)):ls) = max (max lx1 lx2) (getMaxX ls)

getMaxY :: [Line] -> Int
getMaxY [] = 0
getMaxY (((lx1,ly1), (lx2,ly2)):ls) = max (max ly1 ly2) (getMaxY ls)

initBoxRow :: Int -> [Int]
initBoxRow 0 = []
initBoxRow width = (0 :: Int) : initBoxRow (width - 1)

initBox :: Int -> Int -> [[Int]]
initBox width 0 = []
initBox width height = initBoxRow width : initBox width (height - 1)

getBox :: [Line] -> [[Int]]
getBox filteredLines = initBox (getMaxX filteredLines + 1) (getMaxY filteredLines + 1)

incrementBoxRowAtCell :: [Int] -> Int -> [Int]
incrementBoxRowAtCell row index
  | index < 0 || index >= length row = error "incrementBoxAtCell called with invalid x-index"
  | index == 0 = head row + 1 : drop 1 row
  | otherwise = take index row ++ [(row !! index) + 1] ++ drop (index + 1) row

incrementBoxAtCell :: [[Int]] -> Int -> Int -> [[Int]]
incrementBoxAtCell box x y
  | y < 0 || y >= length box = error "incrementBoxAtCell called with invalid y-index"
  | y == 0 = incrementBoxRowAtCell (head box) x : drop 1 box
  | otherwise = take y box ++ [incrementBoxRowAtCell (box !! y) x] ++ drop (y + 1) box

incrementBoxAtPoints :: [[Int]] -> [(Int, Int)] -> [[Int]]
incrementBoxAtPoints box [] = box
incrementBoxAtPoints box ((x,y):ps) = incrementBoxAtPoints (incrementBoxAtCell box x y) ps

addVerticalLineToBox :: [[Int]] -> Line -> [[Int]]
addVerticalLineToBox box ((x1,y1), (x2,y2))
  | y1 == y2 = incrementBoxAtCell box x1 y1
  | otherwise = addVerticalLineToBox (incrementBoxAtCell box x1 (min y1 y2)) ((x1,min y1 y2 + 1), (x2,max y1 y2))

addHorizontalLineToBox :: [[Int]] -> Line -> [[Int]]
addHorizontalLineToBox box ((x1,y1), (x2,y2))
  | x1 == x2 = incrementBoxAtCell box x1 y1
  | otherwise = addHorizontalLineToBox (incrementBoxAtCell box (min x1 x2) y1) ((min x1 x2 + 1,y1), (max x1 x2,y1))

addDiagonalLineToBox :: [[Int]] -> Line -> [[Int]]
addDiagonalLineToBox box ((x1,y1), (x2,y2)) = incrementBoxAtPoints box (getDiagonalLinePoints x1 y1 x2 y2)

addLineToBox :: [[Int]] -> Line -> [[Int]]
addLineToBox box line
  | isLineVertical line = addVerticalLineToBox box line
  | isLineHorizontal line = addHorizontalLineToBox box line
  | isLineDiagonal line = addDiagonalLineToBox box line
  | otherwise = box

fillBox :: [[Int]] -> [Line] -> [[Int]]
fillBox = foldl addLineToBox

countCollisionsInRow :: [Int] -> Int
countCollisionsInRow [] = 0
countCollisionsInRow (c:cs)
  | c > 1 = 1 + countCollisionsInRow cs
  | otherwise = countCollisionsInRow cs

countCollisionsInBox :: [[Int]] -> Int
countCollisionsInBox = foldr ((+) . countCollisionsInRow) 0

countCollisions :: [Line] -> Int
countCollisions filteredLines = countCollisionsInBox (fillBox (getBox filteredLines) filteredLines)

task1 :: [Line] -> IO ()
task1 lines = do
  let straightLines = filterStraightLines lines
  let collisions = countCollisions straightLines

  printf "Task 1: collisions=%d\n" collisions

task2 :: [Line] -> IO ()
task2 lines = do
  let straightAndDiagonalLines = filterStraightAndDiagonalLines lines
  let collisions = countCollisions straightAndDiagonalLines

  printf "Task 2: collisions=%d\n" collisions

main = do
  content <- readFile inputFile
  let chars = lines content

  let lines = getLines chars

  task1 lines
  task2 lines
