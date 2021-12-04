import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

toInt :: String -> Int
toInt s = read s :: Int

getOrder :: String -> [Int]
getOrder firstLine = map toInt (splitOn "," firstLine)

type Board = [[(Int, Bool)]]

initCell :: Int -> (Int, Bool)
initCell value = (value, False)

getBoardDefinition :: [String] -> Board
getBoardDefinition = map (map (initCell . toInt) . words)

getBoards :: [String] -> [Board]
getBoards chars
  | null chars = []
  | head chars == "" = getBoards (drop 1 chars)
  | length chars < 5 = []
  | otherwise = getBoardDefinition (take 5 chars) : getBoards (drop 5 chars)

isRowMarked :: [(Int, Bool)] -> Bool
isRowMarked = foldr ((&&) . snd) True

isColumnMarkedLoop :: Board -> Int -> Int -> Bool
isColumnMarkedLoop board col row
  | row > length board - 1 = True
  | otherwise = snd ((board !! row) !! col) && isColumnMarkedLoop board col (row + 1)

isColumnMarked :: Board -> Int -> Bool
isColumnMarked board index = isColumnMarkedLoop board index 0

hasBoardFullRowMarked :: Board -> Bool
hasBoardFullRowMarked = foldr ((||) . isRowMarked) False

hasBoardFullColumnMarkedLoop :: Board -> Int -> Bool
hasBoardFullColumnMarkedLoop board index
  | index > length (head board) - 1 = False
  | otherwise = isColumnMarked board index || hasBoardFullColumnMarkedLoop board (index + 1)

hasBoardFullColumnMarked :: Board -> Bool
hasBoardFullColumnMarked board = hasBoardFullColumnMarkedLoop board 0

hasIndividualBoardWon :: Board -> Bool
hasIndividualBoardWon board = hasBoardFullRowMarked board || hasBoardFullColumnMarked board

hasBoardWon :: [Board] -> Bool
hasBoardWon boards
  | null boards = False
  | otherwise = hasIndividualBoardWon (head boards) || hasBoardWon (drop 1 boards)

getWonBoard :: [Board] -> Board
getWonBoard boards
  | null boards = error "No boards have won"
  | hasIndividualBoardWon (head boards) = head boards
  | otherwise = getWonBoard (drop 1 boards)

deleteAtIndex :: [t] -> Int -> [t]
deleteAtIndex [] index = []
deleteAtIndex items index = take index items ++ drop (index + 1) items

excludeWonBoardsLoop :: [Board] -> Int -> [Board]
excludeWonBoardsLoop boards index
  | null boards = boards
  | index >= length boards = boards
  | hasIndividualBoardWon (boards !! index) = excludeWonBoardsLoop (deleteAtIndex boards index) index
  | otherwise = excludeWonBoardsLoop boards (index + 1)

excludeWonBoards :: [Board] -> [Board]
excludeWonBoards boards = excludeWonBoardsLoop boards 0

playMoveOnCell :: Int -> (Int, Bool) -> (Int, Bool)
playMoveOnCell move cell
  | fst cell == move = (fst cell, True)
  | otherwise = cell

playMoveOnRow :: Int -> [(Int, Bool)] -> [(Int, Bool)]
playMoveOnRow move = map (playMoveOnCell move)

playMoveOnBoard :: Int -> Board -> Board
playMoveOnBoard move = map (playMoveOnRow move)

playMove :: [Int] -> [Board] -> [Board]
playMove order boards
  | null order = error "No moves left to play and no winning board"
  | otherwise = map (playMoveOnBoard (head order)) boards

getWinningBoardAndMove :: [Int] -> [Board] -> (Board, Int)
getWinningBoardAndMove order boards
  | hasBoardWon (playMove order boards) = (getWonBoard (playMove order boards), head order)
  | otherwise = getWinningBoardAndMove (drop 1 order) (playMove order boards)

sumUnmarkedCell :: (Int, Bool) -> Int
sumUnmarkedCell (value, marked)
  | marked = 0
  | otherwise = value

sumUnmarkedRow :: [(Int, Bool)] -> Int
sumUnmarkedRow = foldr ((+) . sumUnmarkedCell) 0

sumUnmarked :: Board -> Int
sumUnmarked = foldr ((+) . sumUnmarkedRow) 0

addScore :: Board -> Int -> (Board, Int)
addScore wonBoard winningMove = (wonBoard, winningMove * sumUnmarked wonBoard)

getLastBoardOrderedByWinningWithScoreLoop :: [Int] -> [Board] -> (Board, Int) -> (Board, Int)
getLastBoardOrderedByWinningWithScoreLoop order boards (winningBoard, winningScore)
  | null boards || null order = (winningBoard, winningScore)
  | hasBoardWon (playMove order boards) = getLastBoardOrderedByWinningWithScoreLoop
    (drop 1 order)
    (excludeWonBoards (playMove order boards))
    (addScore (getWonBoard (playMove order boards)) (head order))
  | otherwise = getLastBoardOrderedByWinningWithScoreLoop
    (drop 1 order)
    (playMove order boards)
    (winningBoard, winningScore)

getLastBoardOrderedByWinningWithScore :: [Int] -> [Board] -> (Board, Int)
getLastBoardOrderedByWinningWithScore order boards = getLastBoardOrderedByWinningWithScoreLoop order boards ([], 0)

task1 :: [Int] -> [Board] -> IO ()
task1 order boards = do
  let (winningBoard, winningMove) = getWinningBoardAndMove order boards
  let unmarkedValue = sumUnmarked winningBoard

  let finalScore = winningMove * unmarkedValue

  printf "Task 1: winningMove=%d, unmarkedValue=%d, finalScore=%d\n" winningMove unmarkedValue finalScore

task2 :: [Int] -> [Board] -> IO ()
task2 order boards = do
  let (winningBoard, winningScore) = getLastBoardOrderedByWinningWithScore order boards

  printf "Task 2: winningScore=%d\n" winningScore

main = do
  content <- readFile inputFile
  let chars = lines content

  let order = getOrder (head chars)
  let boards = getBoards (drop 1 chars)

  task1 order boards
  task2 order boards
