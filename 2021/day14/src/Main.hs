import Data.List
import Data.List.Split
import Text.Printf

inputFile = "./input.txt"

type Insertion = (String, Char)

getInsertion :: String -> Insertion
getInsertion line = (left, head right) where [left, right] = splitOn " -> " line

getPolymerTemplate :: [String] -> (String, [Insertion])
getPolymerTemplate chars = (template, insertions)
  where
    template = head chars
    insertions = map getInsertion (drop 2 chars)

applyInsertion :: [Insertion] -> Char -> Char -> String
applyInsertion insertions left right
  | length matchingInsertions == 1 = [left, snd (head matchingInsertions)]
  | otherwise = [left]
  where
    matchingInsertions = filter ((right ==) . (last . fst)) (filter ((left ==) . (head . fst)) insertions)

applyInsertions :: [Insertion] -> String -> String
applyInsertions insertions str
  | length str < 2 = str
  | otherwise = applyInsertion insertions (head str) (str !! 1) ++ applyInsertions insertions (drop 1 str)

applyNInsertions :: [Insertion] -> Int -> String -> String
applyNInsertions insertions 0 str = str
applyNInsertions insertions n str = applyNInsertions insertions (n-1) (applyInsertions insertions str)

countElementsLoop :: String -> [(Char, Int)] -> [(Char, Int)]
countElementsLoop [] result = result
countElementsLoop (s:str) result =
  case findIndex ((s ==) . fst) result of
    Nothing -> countElementsLoop str ((s, 1) : result)
    Just idx -> countElementsLoop str (take idx result ++ [(s, snd (result !! idx) + 1)] ++ drop (idx+1) result)

getMostAndLeastCommonElementCounts :: String -> (Int, Int)
getMostAndLeastCommonElementCounts str = (mostCommon, leastCommon)
  where
    counts = countElementsLoop str []
    leastCommon = snd (minimumBy (\ (_,a) (_,b) -> compare a b) counts)
    mostCommon = snd (maximumBy (\ (_,a) (_,b) -> compare a b) counts)

getMostLeastCommonDiff :: String -> Int
getMostLeastCommonDiff str = mostCommon - leastCommon where (mostCommon, leastCommon) = getMostAndLeastCommonElementCounts str

getMostLeastCommonDiffAfterNInsertions :: [Insertion] -> Int -> String -> Int
getMostLeastCommonDiffAfterNInsertions insertions n str = getMostLeastCommonDiff (applyNInsertions insertions n str)

task1 :: String -> [Insertion] -> IO ()
task1 template insertions = do
  let diff = getMostLeastCommonDiffAfterNInsertions insertions 10 template
  printf "Task 1: diff=%d\n" diff

main = do
  content <- readFile inputFile
  let chars = lines content
  let (template, insertions) = getPolymerTemplate chars

  task1 template insertions
