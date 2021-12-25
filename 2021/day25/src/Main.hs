import Text.Printf

inputFile = "./input.txt"

off = 0
east = 1
south = 2

newtype SeaFloor = SeaFloor { cells :: [[Int]] }

instance Eq SeaFloor where
  (SeaFloor a) == (SeaFloor b) = length a == length b &&
    (length . head) a == (length . head) b &&
    not (any (\ y -> any (\ x -> (a !! y) !! x /= (b !! y) !! x) [0..(length . head) a - 1]) [0..length a - 1])

getTypeFromChar :: Char -> Int
getTypeFromChar c
  | c == '.' = off
  | c == 'v' = south
  | c == '>' = east
  | otherwise = error ("invalid char "++show c)

getCharFromType :: Int -> Char
getCharFromType t
  | t == off = '.'
  | t == south = 'v'
  | t == east = '>'
  | otherwise = error ("invalid type "++show t)

instance Show SeaFloor where
  show (SeaFloor cells) = concatMap ((++"\n") . map getCharFromType) cells

initSeaFloor :: [String] -> SeaFloor
initSeaFloor = SeaFloor . map (map getTypeFromChar)

runStepForType :: Int -> SeaFloor -> SeaFloor
runStepForType cellType (SeaFloor cells) = SeaFloor nextCells
  where h = length cells
        w = (length . head) cells
        nextCells = getNextCells cells 0
        getNextCells board y
          | y >= h = board
          | otherwise = getNextCells (getNextRow board 0 y) (y+1)
        getNextRow board x y
          | x >= w = board
          | otherwise = getNextRow nextBoard (x+1) y
          where cell = (cells !! y) !! x
                dx = if cell == east then 1 else 0
                dy = if cell == east then 0 else 1
                destX = mod (x + dx) w
                destY = mod (y + dy) h
                isFree = (cells !! destY) !! destX == off
                nextBoard = if cell == cellType && isFree then nextBoard1 else board
                nextBoard0 = take y board ++ [take x (board !! y) ++ [off] ++ drop (x+1) (board !! y)] ++ drop (y+1) board
                nextBoard1 = take destY nextBoard0 ++
                  [take destX (nextBoard0 !! destY) ++ [cell] ++ drop (destX+1) (nextBoard0 !! destY)] ++
                  drop (destY+1) nextBoard0

runStep = runStepForType south . runStepForType east

getNumStepsBeforeClear :: SeaFloor -> Int
getNumStepsBeforeClear seaFloor = loop seaFloor 0
  where loop prev i
          | moved = loop nextSeaFloor (i+1)
          | otherwise = i+1
          where nextSeaFloor = runStep prev
                moved = nextSeaFloor /= prev

task1 = getNumStepsBeforeClear

main = do
  content <- readFile inputFile
  let seaFloor = (initSeaFloor . lines) content

  printf "Task 1: result=%d\n" (task1 seaFloor)
