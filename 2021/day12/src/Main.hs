import Data.Char
import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

type Path = (String, String)

startsAt :: String -> Path -> Bool
startsAt p (begin, _) = begin == p

endsAt :: String -> Path -> Bool
endsAt p (_, end) = end == p

startsOrEndsAt :: String -> Path -> Bool
startsOrEndsAt p path = startsAt p path || endsAt p path

isStart = startsAt "start"
isEnd = endsAt "end"

toPath :: String -> Path
toPath line = (head components, components !! 1)
  where components = splitOn "-" line

isSmallCave :: String -> Bool
isSmallCave c = c /= "end" && map toLower c == c

hasVisited :: [String] -> String -> Bool
hasVisited soFar cave = cave `elem` soFar

notRevisitingSmallPath :: [String] -> String -> Bool
notRevisitingSmallPath _ "start" = False
notRevisitingSmallPath soFar option = not (isSmallCave option && hasVisited soFar option)

getOtherEnd :: String -> Path -> String
getOtherEnd from (begin, end)
  | from == begin = end
  | from == end = begin
  | otherwise = error "getOtherEnd called for non-matching path"

getNextOptions :: [Path] -> [String] -> String -> [String]
getNextOptions allPaths soFar from = filter
  (notRevisitingSmallPath soFar)
  (map (getOtherEnd from) (filter (startsOrEndsAt from) allPaths))

addToEnd :: [String] -> String -> [String]
addToEnd soFar option = soFar ++ [option]

differs :: (String, String) -> Bool
differs (a,b) = a /= b

isPathEqual :: [String] -> [String] -> Bool
isPathEqual a b = length a == length b && not (any differs (zip a b))

reduceFullPathsToEnd :: [Path] -> [String] -> [[String]] -> String -> [[String]]
reduceFullPathsToEnd allPaths soFar reduction from =
  nextReduction ++ concatMap (reduceFullPathsToEnd allPaths nextSoFar []) nonCompletedPaths
  where
    nextSoFar = soFar ++ [from]
    nextOptions = getNextOptions allPaths soFar from
    completedPaths = filter ("end" ==) nextOptions
    nextReduction = reduction ++ map (addToEnd nextSoFar) completedPaths
    nonCompletedPaths = filter ("end" /=) nextOptions

findAllPathsFromStartToEnd :: [Path] -> [[String]]
findAllPathsFromStartToEnd allPaths = reduceFullPathsToEnd allPaths [] [] "start"

printPaths :: [[String]] -> IO ()
printPaths [] = do return ()
printPaths (p:paths) = do
  print p
  printPaths paths

task1 :: [Path] -> IO ()
task1 paths = do
  let allPathsToEnd = findAllPathsFromStartToEnd paths
  let result = length allPathsToEnd
  printf "Task 1: numPaths=%d\n" result

main = do
  content <- readFile inputFile
  let chars = lines content
  let paths = map toPath chars

  task1 paths
