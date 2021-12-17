import Data.List.Split (splitOn)
import Debug.Trace
import Text.Printf

inputFile = "./input.txt"

toInt :: String -> Int
toInt c = read c :: Int

data TargetArea = TargetArea { minX :: Int
                             , maxX :: Int
                             , minY :: Int
                             , maxY :: Int
                             } deriving (Show)

highestGuessY :: TargetArea -> Int
highestGuessY targetArea = max (abs (minY targetArea)) (abs (maxY targetArea))

highestGuessX :: TargetArea -> Int
highestGuessX targetArea = max (minX targetArea) (maxX targetArea)

getTargetArea :: String -> TargetArea
getTargetArea line = TargetArea minX maxX minY maxY
  where [_, dims] = splitOn ":" line
        [xDims, yDims] = splitOn "," dims
        [_, xRange] = splitOn "=" xDims
        [_, yRange] = splitOn "=" yDims
        [minY, maxY] = map toInt (splitOn ".." yRange)
        [minX, maxX] = map toInt (splitOn ".." xRange)

type Pos = (Int, Int)
type Velocity = Pos

isInTarget :: Pos -> TargetArea -> Bool
isInTarget (x,y) targetArea = x >= minX targetArea && x <= maxX targetArea
  && y >= minY targetArea && y <= maxY targetArea

getNextVectors :: (Pos, Velocity) -> (Pos, Velocity)
getNextVectors ((x,y), (vx,vy)) = ((x+vx,y+vy), (vx+dragX,vy-1))
  where dragX
          | vx > 0 = -1
          | vx < 0 = 1
          | otherwise = 0

shotReachesTargetLoop :: TargetArea -> (Pos, Velocity) -> Bool
shotReachesTargetLoop targetArea (pos, velocity)
  | isInTarget pos targetArea = True
  | fst pos > maxX targetArea ||
    (fst velocity <= 0 && (fst pos < minX targetArea || fst pos > maxX targetArea)) ||
    (snd velocity < 0 && snd pos < minY targetArea)
      = False
  | otherwise = shotReachesTargetLoop targetArea (nextPos, nextVelocity)
  where (nextPos, nextVelocity) = getNextVectors (pos, velocity)

shotReachesTarget :: TargetArea -> Velocity -> Bool
shotReachesTarget targetArea velocity = shotReachesTargetLoop targetArea ((0,0), velocity)

getTopOfShotLoop :: Int -> (Pos, Velocity) -> Int
getTopOfShotLoop previousTop vectors
  | vy <= 0 = max y previousTop
  | otherwise = getTopOfShotLoop previousTop (getNextVectors vectors)
  where ((_,y),(_,vy)) = vectors

getTopOfShot :: Velocity -> Int
getTopOfShot velocity = getTopOfShotLoop 0 ((0,0),velocity)

type Reduction = (Velocity, Int) -- max height

findBestShotReachingTargetWithFixedVY :: TargetArea -> Int -> Int -> Reduction -> Reduction
findBestShotReachingTargetWithFixedVY targetArea vy vx red
  | vx >= highestGuessX targetArea = nextReduction
  | otherwise = findBestShotReachingTargetWithFixedVY targetArea vy (vx+1) nextReduction
  where reachesTarget = shotReachesTarget targetArea (vx,vy)
        topOfShot = if reachesTarget then getTopOfShot (vx,vy) else 0
        nextReduction = if topOfShot > snd red then ((vx,vy),topOfShot) else red

findBestShotReachingTargetLoop :: TargetArea -> Int -> Reduction -> Reduction
findBestShotReachingTargetLoop targetArea vy red
  | vy <= -1 * highestGuessY targetArea = bestShot
  | otherwise = findBestShotReachingTargetLoop targetArea (vy-1) bestShot
  where bestShot = findBestShotReachingTargetWithFixedVY targetArea vy 0 red

findBestShotReachingTarget :: TargetArea -> Reduction
findBestShotReachingTarget targetArea = findBestShotReachingTargetLoop targetArea (highestGuessY targetArea) ((0,0),0)

task1 :: TargetArea -> IO ()
task1 targetArea = do
  let bestShot = findBestShotReachingTarget targetArea
  printf "Task 1: %s\n" (show bestShot)

main = do
  content <- readFile inputFile
  let chars = lines content
  let targetArea = getTargetArea (head chars)

  task1 targetArea
