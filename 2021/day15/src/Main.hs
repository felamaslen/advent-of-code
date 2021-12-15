import Data.List
import Data.List.Split
import Data.Map (Map)
import Debug.Trace
import Text.Printf
import GHC.IO (unsafePerformIO)
import Data.Bifunctor.Compat.Repl (Bifunctor(bimap))

inputFile = "./input.txt"

type Board = [[Int]]

printBoardRow :: Int -> Int -> [Int] -> Path -> IO ()
printBoardRow x y [] _ = do printf "\n"
printBoardRow x y (c:row) path
  | any (\ (px,py) -> px == x && py == y) path = do
    printf "\x1B[33m"
    printf "%d" c
    printf "\x1B[0m"
    printBoardRow (x+1) y row path
  | otherwise = do
    printf "%d" c
    printBoardRow (x+1) y row path

printBoardWithPathLoop :: Int -> Board -> Path -> IO ()
printBoardWithPathLoop _ [] _ = do printf "\n"
printBoardWithPathLoop y (b:board) path = do
  printBoardRow 0 y b path
  printBoardWithPathLoop (y+1) board path

printBoardWithPath :: Board -> Path -> IO ()
printBoardWithPath = printBoardWithPathLoop 0

clearPrintedBoard :: Board -> IO ()
clearPrintedBoard [] = do return ()
clearPrintedBoard (b:board) = do
  printf "\33[2K\r"
  printf "\033[A"
  clearPrintedBoard board

traceBoard :: Board -> Path -> a -> a
traceBoard board path expr = unsafePerformIO $ do
  printBoardWithPath board path
  return expr

getBoard :: [String] -> Board
getBoard = map (map (\ c -> read [c] :: Int))

type Point = (Int, Int)
type Path = [Point]

getTotalRisk :: Board -> Path -> Int
getTotalRisk _ [] = 0
getTotalRisk _ [(0,0)] = 0
getTotalRisk board ((x,y):points) = ((board !! y) !! x) + getTotalRisk board points

getNeighbours :: Board -> Point -> [Point]
getNeighbours board (x,y)
  | isLeft && isTop = [(x+1,y),(x,y+1)]
  | isRight && isTop = [(x-1,y),(x,y+1)]
  | isLeft && isBottom = [(x+1,y),(x,y-1)]
  | isRight && isBottom = [(x-1,y),(x,y-1)]
  | isLeft = [(x,y-1),(x+1,y),(x,y+1)]
  | isTop = [(x-1,y),(x,y+1),(x+1,y)]
  | isRight = [(x-1,y),(x,y-1),(x,y+1)]
  | isBottom = [(x-1,y),(x,y-1),(x+1,y)]
  | otherwise = [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]
  where isLeft = x == 0
        isTop = y == 0
        isRight = x == length (head board) - 1
        isBottom = y == length board - 1

getValidNeighbours :: Board -> Point -> Point -> [Point]
getValidNeighbours board (x,y) (prevX, prevY) =
  filter (\ (px,py) -> not (px == prevX && py == prevY)) (getNeighbours board (x,y))

heuristic :: Board -> Point -> Int
heuristic board (x,y) = 1 * ((length (head board) - 1 - x) + (length board - 1 - y))

nodeCostFn :: Board -> Path -> Int
nodeCostFn board path = getTotalRisk board path + heuristic board (head path)

type PathReduction = ((Point, Point), (Int, Int))
type PointReduction = (Point, Int)

filterNeighboursWithLowerGScore :: [PathReduction] -> PathReduction -> Bool
filterNeighboursWithLowerGScore reduction ((point,_),(_,gScore)) =
  prevGScore == -1 || gScore < prevGScore
  where
    prevGScore = case findIndex (\ ((p,_),_) -> p == point) reduction of
      Nothing -> -1
      Just idx -> (snd.snd) (reduction !! idx)

mergePathReduction :: [PathReduction] -> [PathReduction] -> [PathReduction]
mergePathReduction to [] = to
mergePathReduction to (f:from) = case findIndex (\ ((p,_),_) -> p == (fst.fst) f) to of
  Nothing -> mergePathReduction (f:to) from
  Just idx -> mergePathReduction (take idx to ++ [f] ++ drop (idx+1) to) from

mergePointsReduction :: [PointReduction] -> [PathReduction] -> [PointReduction]
mergePointsReduction to [] = to
mergePointsReduction to (((point,_),(f,_)):from) = case findIndex (\ (p,_) -> p == point) to of
  Nothing -> mergePointsReduction ((point,f):to) from
  Just idx -> mergePointsReduction (take idx to ++ [(point,f)] ++ drop (idx+1) to) from

getNextReduction :: Board -> [PathReduction] -> [PointReduction] -> ([PathReduction], [PointReduction])
getNextReduction board reduction points = (nextReduction, nextPoints)
  where
    topNode = fst (head points)
    topNodeIndex = findIndex (\ ((p,_),_) -> p == topNode) reduction
    prevNode = case topNodeIndex of
      Nothing -> topNode
      Just idx -> (snd.fst) (reduction !! idx)
    neighbours = getValidNeighbours board topNode prevNode
    gScore = case topNodeIndex of
      Nothing -> 0
      Just idx -> (snd.snd) (reduction !! idx)
    neighboursWithGScores = map (\ (x,y) -> (
      ((x,y), topNode),
      (gScore + (board !! y) !! x + heuristic board (x,y), gScore + (board !! y) !! x)
      )) neighbours
    filteredNeighbours = filter (filterNeighboursWithLowerGScore reduction) neighboursWithGScores
    nextReduction = mergePathReduction reduction filteredNeighbours
    pointsWithNeighbours = mergePointsReduction (tail points) filteredNeighbours
    nextPoints = sortBy (\ (_,a) (_,b) -> compare a b) pointsWithNeighbours

buildPathFromReductionLoop :: [PathReduction] -> Point -> Path -> Path
buildPathFromReductionLoop _ (0,0) path = path
buildPathFromReductionLoop reduction point path =
  case findIndex (\ ((pTo, pFrom), _) -> pTo == point) reduction of
    Nothing -> path
    Just idx -> buildPathFromReductionLoop reduction ((snd.fst) (reduction !! idx)) ((snd.fst) (reduction !! idx):path)

buildPathFromReduction :: [PathReduction] -> Point -> (Path, Int)
buildPathFromReduction reduction point = (path, score)
  where 
    reductionItem = head (filter (\ ((p,_),_) -> p == point) reduction)
    score = (fst.snd) reductionItem
    path = buildPathFromReductionLoop reduction point [point]

getOptimalPathLoop :: Board -> [PathReduction] -> [PointReduction] -> (Path, Int)
getOptimalPathLoop board reduction points
  | heuristic board topPoint == 0 =
    buildPathFromReduction nextReduction topPoint
  | otherwise = getOptimalPathLoop board nextReduction nextPoints
  where
    (nextReduction, nextPoints) = getNextReduction board reduction points
    topPoint = (fst.head) points

getOptimalPathWithScore :: Board -> (Path, Int)
getOptimalPathWithScore board = getOptimalPathLoop board [((start, start), (startH, 0))] [(start, startH)]
  where start = (0,0)
        startH = heuristic board start

task1 :: Board -> IO ()
task1 board = do
  let (path, score) = getOptimalPathWithScore board
  printf "score=%d\n" score

main = do
  content <- readFile inputFile
  let chars = lines content
  let board = getBoard chars

  task1 board
