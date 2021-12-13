import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

toInt :: String -> Int
toInt s = read s :: Int

type Board = [[Bool]]
type Fold = (Int, Int)

toPoint :: String -> (Int, Int)
toPoint chars = (x,y) where [x,y] = map toInt (splitOn "," chars)

isFold :: String -> Bool
isFold line
  | null w = False
  | otherwise = head w == "fold"
  where w = words line

toFold :: String -> Fold
toFold chars
  | direction == "x" = (mag, 0)
  | otherwise = (0, mag)
  where
    [direction, magStr] = splitOn "=" (last (words chars))
    mag = toInt magStr

createBoardRow :: Int -> Int -> [Bool] -> [Bool]
createBoardRow maxX x row
  | x > maxX = row
  | otherwise = createBoardRow maxX (x+1) (False : row)

createBoard :: Int -> Int -> Int -> Board -> Board
createBoard maxX maxY y board
  | y > maxY = board
  | otherwise = createBoard maxX maxY (y+1) (createBoardRow maxX 0 [] : board)

applyPointToBoard :: Board -> (Int, Int) -> Board
applyPointToBoard board (x,y) =
  take y board
  ++ [take x (board !! y) ++ [True] ++ drop (x+1) (board !! y)]
  ++ drop (y+1) board

applyToBoard :: Board -> [(Int, Int)] -> Board
applyToBoard = foldl applyPointToBoard

getInitialBoard :: [String] -> (Board, [Fold])
getInitialBoard chars = (board, folds)
  where
    numFolds = length (filter isFold chars)
    folds = map toFold (drop (length chars - numFolds) chars)
    points = map toPoint (take (length chars - numFolds - 1) chars)
    maxX = maximum (map fst points)
    maxY = maximum (map snd points)
    emptyBoard = createBoard maxX maxY 0 []
    board = applyToBoard emptyBoard points

combineZippedRows :: [([Bool], [Bool])] -> Board
combineZippedRows [] = []
combineZippedRows ((leftRow, rightRow):rest) =
  zipWith (||) leftRow rightRow : combineZippedRows rest

combineHalves :: Board -> Board -> Board
combineHalves left right = combineZippedRows (zip left right)

foldBoardVertical :: Board -> Int -> Board
foldBoardVertical board pos = combineHalves left right
  where
    left = map (take pos) board
    right = map (reverse . drop pos) board

foldBoardHorizontal :: Board -> Int -> Board
foldBoardHorizontal board pos = combineHalves top bottom
  where
    top = take pos board
    bottom = reverse (drop pos board)

foldBoard :: Board -> Fold -> Board
foldBoard board (x,y)
  | x > 0 = foldBoardVertical board x
  | otherwise = foldBoardHorizontal board y

countDots :: Board -> Int
countDots = foldr ((+) . length . filter (True ==)) 0

foldOnceAndCountDots :: Board -> [Fold] -> Int
foldOnceAndCountDots board folds = countDots (foldBoard board (head folds))

printBoardRow :: [Bool] -> IO ()
printBoardRow [] = do printf "\n"
printBoardRow (c:cs)
  | c = do
    printf "#"
    printBoardRow cs
  | otherwise = do
    printf "."
    printBoardRow cs

printBoard :: Board -> IO ()
printBoard [] = do printf "\n"
printBoard (r:rs) = do
  printBoardRow r
  printBoard rs

task1 :: Board -> [Fold] -> IO ()
task1 board folds = do
  let numDots = foldOnceAndCountDots board folds
  printf "Task 1: numDots=%d\n" numDots

main = do
  content <- readFile inputFile
  let chars = lines content
  let (board, folds) = getInitialBoard chars

  task1 board folds
