import Data.Char
import Data.List
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

getOtherEnd :: String -> Path -> String
getOtherEnd from (begin, end)
  | from == begin = end
  | from == end = begin
  | otherwise = error "getOtherEnd called for non-matching path"

toPath :: String -> Path
toPath line = (head components, components !! 1)
  where components = splitOn "-" line

isSmallCave :: String -> Bool
isSmallCave c = c /= "start" && c /= "end" && map toLower c == c

hasVisited :: [String] -> String -> Bool
hasVisited soFar cave = cave `elem` soFar

notRevisitingSmallPath :: [String] -> String -> Bool
notRevisitingSmallPath _ "start" = False
notRevisitingSmallPath soFar option = not (isSmallCave option && hasVisited soFar option)

groupLoop :: [String] -> [[String]] -> [[String]]
groupLoop [] red = red
groupLoop (a:as) red
  | not (null red) && a == head (head red) = groupLoop as ((a:head red):drop 1 red)
  | otherwise = groupLoop as ([a]:red)

sortAndGroup :: [String] -> [[String]]
sortAndGroup as = groupLoop (sort as) []

hasAtMostOneSmallCaveRevist :: [String] -> Bool
hasAtMostOneSmallCaveRevist caves = maximum lengths < 3 && length (filter (2 ==) lengths) < 2
  where lengths = map length (sortAndGroup (filter isSmallCave caves))

notRevisitingSecondSmallPath :: [String] -> String -> Bool
notRevisitingSecondSmallPath _ "start" = False
notRevisitingSecondSmallPath soFar option
  | not (isSmallCave option) = True
  | otherwise = not (hasVisited soFar option) || hasAtMostOneSmallCaveRevist (option:soFar)

getNextOptions1 :: [Path] -> [String] -> String -> [String]
getNextOptions1 allPaths soFar from = filter
  (notRevisitingSmallPath soFar)
  (map (getOtherEnd from) (filter (startsOrEndsAt from) allPaths))

getNextOptions2 :: [Path] -> [String] -> String -> [String]
getNextOptions2 allPaths soFar from = filter
  (notRevisitingSecondSmallPath (from:soFar))
  (map (getOtherEnd from) (filter (startsOrEndsAt from) allPaths))

addToEnd :: [String] -> String -> [String]
addToEnd soFar option = soFar ++ [option]

type ReduceFullPathsToEnd = [Path] -> [String] -> String -> [[String]]
type GetNextOptions = [Path] -> [String] -> String -> [String]

makeReduceFullPathsToEnd :: GetNextOptions -> ReduceFullPathsToEnd
makeReduceFullPathsToEnd getNextOptions allPaths soFar from =
  reduction ++ concatMap (reducer allPaths nextSoFar) nonCompletedPaths
  where
    reducer = makeReduceFullPathsToEnd getNextOptions
    nextSoFar = soFar ++ [from]
    nextOptions = getNextOptions allPaths soFar from
    nonCompletedPaths = filter ("end" /=) nextOptions
    completedPaths = filter ("end" ==) nextOptions
    reduction = map (addToEnd nextSoFar) completedPaths

reduceFullPathsToEnd1 = makeReduceFullPathsToEnd getNextOptions1
reduceFullPathsToEnd2 = makeReduceFullPathsToEnd getNextOptions2

findAllPathsFromStartToEnd1 :: [Path] -> [[String]]
findAllPathsFromStartToEnd1 allPaths = reduceFullPathsToEnd1 allPaths [] "start"

findAllPathsFromStartToEnd2 :: [Path] -> [[String]]
findAllPathsFromStartToEnd2 allPaths = reduceFullPathsToEnd2 allPaths [] "start"

printPaths :: [[String]] -> IO ()
printPaths [] = do return ()
printPaths (p:paths) = do
  print p
  printPaths paths

task1 :: [Path] -> IO ()
task1 paths = do
  let allPathsToEnd = findAllPathsFromStartToEnd1 paths
  let result = length allPathsToEnd
  printf "Task 1: numPaths=%d\n" result

task2 :: [Path] -> IO ()
task2 paths = do
  let allPathsToEnd = findAllPathsFromStartToEnd2 paths
  let result = length allPathsToEnd
  printf "Task 2: numPaths=%d\n" result

main = do
  content <- readFile inputFile
  let chars = lines content
  let paths = map toPath chars

  task1 paths
  task2 paths
