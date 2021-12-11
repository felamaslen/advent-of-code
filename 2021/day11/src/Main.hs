import Text.Printf

inputFile = "./input.txt"

toInt :: Char -> Int
toInt s = read [s] :: Int

type Board = [[Int]]

getStartingBoard :: [String] -> Board
getStartingBoard = map (map toInt)

incrementEnergyLevels :: Board -> Board
incrementEnergyLevels = map (map (+ 1))

setCellValue :: (Int -> Int) -> Board -> (Int, Int) -> Board
setCellValue getNewValue board (x,y) =
  take y board
  ++ [take x (board !! y) ++ [getNewValue ((board !! y) !! x)] ++ drop (x+1) (board !! y)]
  ++ drop (y+1) board

incrementCell = setCellValue (+ 1)
zeroCell = setCellValue (const 0)

incrementCells :: Board -> [(Int, Int)] -> Board
incrementCells board [] = board
incrementCells board ((x,y):points)
  | x < 0 || x >= length (head board) || y < 0 || y >= length board || (board !! y) !! x == 0 =
    incrementCells board points
  | otherwise = incrementCells (incrementCell board (x,y)) points

getSurroundingCells :: (Int, Int) -> [(Int, Int)]
getSurroundingCells (x,y) = [(x-1,y), (x-1,y-1), (x,y-1), (x+1,y-1), (x+1,y), (x+1,y+1), (x,y+1), (x-1,y+1)]

flashOctopus :: Board -> (Int, Int) -> Board
flashOctopus board (x,y) = zeroCell (incrementCells board (getSurroundingCells (x,y))) (x,y)

getReadyToBeFlashedLoop :: Board -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getReadyToBeFlashedLoop board (x,y) red
  | y >= length board = red
  | x >= length (head board) = getReadyToBeFlashedLoop board (0,y+1) red
  | (board !! y) !! x > 9 = getReadyToBeFlashedLoop board (x+1,y) ((x,y) : red)
  | otherwise = getReadyToBeFlashedLoop board (x+1,y) red

getReadyToBeFlashed :: Board -> [(Int, Int)]
getReadyToBeFlashed board = getReadyToBeFlashedLoop board (0,0) []

flashAllOctopuses :: (Board, Int) -> (Board, Int)
flashAllOctopuses (board, numFlashes)
  | null readyToBeFlashed = (board, numFlashes)
  | otherwise = flashAllOctopuses (
    foldl flashOctopus board readyToBeFlashed,
    numFlashes + length readyToBeFlashed
  )
  where readyToBeFlashed = getReadyToBeFlashed board

runBoardThroughStep :: (Board, Int) -> (Board, Int)
runBoardThroughStep (board, numFlashes) = flashAllOctopuses (incrementEnergyLevels board, numFlashes)

runBoardThroughSteps :: Int -> Int -> (Board, Int) -> (Board, Int)
runBoardThroughSteps numSteps step (board, numFlashes)
  | step >= numSteps = (board, numFlashes)
  | otherwise = runBoardThroughSteps numSteps (step+1) (nextBoard, nextNumFlashes)
  where (nextBoard, nextNumFlashes) = runBoardThroughStep (board, numFlashes)

simulateNSteps :: Board -> Int -> (Board, Int)
simulateNSteps board numSteps = runBoardThroughSteps numSteps 0 (board, 0)

reduceFirstAllFlashMoment :: Int -> (Board, Int) -> Int
reduceFirstAllFlashMoment step (board, numFlashes)
  | numFlashes == length board * length (head board) = step
  | otherwise = reduceFirstAllFlashMoment (step+1) (runBoardThroughStep (board, 0))

getFirstAllFlashMoment :: Board -> Int
getFirstAllFlashMoment board = reduceFirstAllFlashMoment 0 (board, 0)

printBoardRow :: [Int] -> IO ()
printBoardRow [] = do printf "\n"
printBoardRow (c:cells) = do
  printf "%d" c
  printBoardRow cells

printBoard :: Board -> IO ()
printBoard [] = do return ()
printBoard (r:rows) = do
  printBoardRow r
  printBoard rows

task1 :: Board -> IO ()
task1 board = do
  let (_, numFlashes) = simulateNSteps board 100
  printf "Task 1: numFlashes=%d\n" numFlashes

task2 :: Board -> IO ()
task2 board = do
  let firstAllFlashStep = getFirstAllFlashMoment board
  printf "Task 2: firstAllFlashStep=%d\n" firstAllFlashStep

main = do
  content <- readFile inputFile
  let chars = lines content
  let board = getStartingBoard chars

  task1 board
  task2 board
