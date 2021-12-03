import Text.Printf

inputFile = "./input.txt"

forward = "forward"
up = "up"
down = "down"

mapSplitLine :: [String] -> (String, Int)
mapSplitLine [direction, distance] = (direction, read distance :: Int)
mapSplitLine _ = (forward, 0)

mapControlTuples :: String -> (String, Int)
mapControlTuples s = mapSplitLine (words s)

calculate2DMotion :: String -> Int -> (Int, Int) -> (Int, Int)
calculate2DMotion direction distance (horizontal, depth)
  | direction == forward = (horizontal + distance, depth)
  | direction == up = (horizontal, depth - distance)
  | direction == down = (horizontal, depth + distance)
  | otherwise = (horizontal, depth)

reduce2DMotion :: [(String, Int)] -> (Int, Int)
reduce2DMotion = foldr (uncurry calculate2DMotion) (0, 0)

calculateAimedMotion :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
calculateAimedMotion direction distance (horizontal, depth, aim)
  | direction == forward = (horizontal + distance, depth + distance * aim, aim)
  | direction == up = (horizontal, depth, aim - distance)
  | direction == down = (horizontal, depth, aim + distance)
  | otherwise = (horizontal, depth, aim)

reduceAimedMotion :: [(String, Int)] -> (Int, Int, Int)
reduceAimedMotion [] = (0, 0, 0)
reduceAimedMotion values =
  uncurry calculateAimedMotion (last values) (reduceAimedMotion (init values))

task1 :: [(String, Int)] -> IO ()
task1 values = do
  let (horizontal, depth) = reduce2DMotion values
  let position = horizontal * depth
  printf "Task 1: horizontal=%d, depth=%d, position=%d\n" horizontal depth position

task2 :: [(String, Int)] -> IO ()
task2 values = do
  let (horizontal, depth, aim) = reduceAimedMotion values
  let position = horizontal * depth

  printf "Task 2: horizontal=%d, depth=%d, position=%d\n" horizontal depth position

main = do
  content <- readFile inputFile
  let chars = lines content
  let values = map mapControlTuples chars

  task1 values
  task2 values
