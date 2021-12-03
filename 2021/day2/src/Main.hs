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

task1 :: [(String, Int)] -> IO ()
task1 values = do
  let (horizontal, depth) = reduce2DMotion values
  let position = horizontal * depth
  printf "Task 1: horizontal=%d, depth=%d, position=%d\n" horizontal depth position

main = do
  content <- readFile inputFile
  let chars = lines content
  let values = map mapControlTuples chars

  task1 values
