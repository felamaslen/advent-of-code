import Data.List
import Data.List.Split
import Text.Printf
import Control.Monad.State (liftM2)

inputFile = "./input.txt"

type Insertion = ((Char, Char), Char)

getInsertion :: String -> Insertion
getInsertion line = ((head left, last left), head right) where [left, right] = splitOn " -> " line

getPolymerTemplate :: [String] -> (String, [Insertion])
getPolymerTemplate chars = (template, insertions)
  where
    template = head chars
    insertions = map getInsertion (drop 2 chars)

type PairCount = ((Char, Char), Int)

type LetterCount = (Char, Int)

getInitialCountsLoop :: String -> ([PairCount], [LetterCount]) -> ([PairCount], [LetterCount])
getInitialCountsLoop [] counts = counts
getInitialCountsLoop template (pairs, letters) = getInitialCountsLoop nextTemplate (nextPairs, nextLetters)
  where
    left = head template
    right = template !! 1
    nextTemplate = case length template of
                     2 -> []
                     n -> drop 1 template
    nextPairs = case findIndex (liftM2 (&&) ((left ==) . (fst . fst)) ((right ==) . (snd . fst))) pairs of
      Nothing -> ((left, right), 1) : pairs
      Just idx -> take idx pairs ++ [((left, right), snd (pairs !! idx) + 1)] ++ drop (idx+1) pairs
    nextLetters = case findIndex ((right ==) . fst) letters of
      Nothing -> (right, 1) : letters
      Just idx -> take idx letters ++ [(right, snd (letters !! idx) + 1)] ++ drop (idx+1) letters

getInitialCounts :: String -> ([PairCount], [LetterCount])
getInitialCounts [] = ([], [])
getInitialCounts template = getInitialCountsLoop template ([], [(head template, 1)])

mergePairs :: [PairCount] -> [(Char, Char)] -> Int -> [PairCount]
mergePairs to [] _ = to
mergePairs to (f:from) count =
  case findIndex ((f ==) . fst) to of
    Nothing -> mergePairs ((f, count) : to) from count
    Just idx -> mergePairs (take idx to ++ [(f, snd (to !! idx) + count)] ++ drop (idx+1) to) from count

incrementLetterCount :: [LetterCount] -> Maybe Char -> Int -> [LetterCount]
incrementLetterCount letterCount insertedLetter numInsertions =
  case insertedLetter of
    Nothing -> letterCount
    Just letter -> case findIndex ((letter ==) . fst) letterCount of
      Nothing -> (letter, numInsertions) : letterCount
      Just idx -> take idx letterCount ++ [(letter, snd (letterCount !! idx) + numInsertions)] ++ drop (idx+1) letterCount

applyInsertionsLoop :: [Insertion] -> ([PairCount], [LetterCount]) -> [PairCount] -> ([PairCount], [LetterCount])
applyInsertionsLoop _ ([], letterCount) reduction = (reduction, letterCount)
applyInsertionsLoop insertions ((pair, count):pairCounts, letterCount) reduction = applyInsertionsLoop
  insertions
  (pairCounts, nextLetters)
  (mergePairs reduction replacedPairs count)
  where
    matchingInsertionIdx = findIndex ((pair ==) . fst) insertions
    insertedLetter = case matchingInsertionIdx of
      Nothing -> Nothing
      Just idx -> Just (snd (insertions !! idx))
    replacedPairs = case matchingInsertionIdx of
      Nothing -> [pair]
      Just idx -> [(fst pair, snd (insertions !! idx)), (snd (insertions !! idx), snd pair)]
    nextLetters = incrementLetterCount letterCount insertedLetter count

applyInsertions :: [Insertion] -> ([PairCount], [LetterCount]) -> ([PairCount], [LetterCount])
applyInsertions insertions counts = applyInsertionsLoop insertions counts []

applyNInsertions :: Int -> [Insertion] -> ([PairCount], [LetterCount]) -> ([PairCount], [LetterCount])
applyNInsertions 0 _ counts = counts
applyNInsertions n insertions counts = applyNInsertions (n-1) insertions (applyInsertions insertions counts)

getMostAndLeastCommonElementCounts :: [LetterCount] -> (Int, Int)
getMostAndLeastCommonElementCounts counts = (mostCommon, leastCommon)
  where
    leastCommon = snd (minimumBy (\ (_,a) (_,b) -> compare a b) counts)
    mostCommon = snd (maximumBy (\ (_,a) (_,b) -> compare a b) counts)

getMostLeastCommonDiffAfterNInsertions :: String -> [Insertion] -> Int -> Int
getMostLeastCommonDiffAfterNInsertions template insertions n = mostCommon - leastCommon
  where
    (pairs, letters) = getInitialCounts template
    (_, counts) = applyNInsertions n insertions (pairs, letters)
    (mostCommon, leastCommon) = getMostAndLeastCommonElementCounts counts

task1 :: String -> [Insertion] -> IO ()
task1 template insertions = do
  let result = getMostLeastCommonDiffAfterNInsertions template insertions 10
  printf "Task 1: result=%d\n" result

task2 :: String -> [Insertion] -> IO ()
task2 template insertions = do
  let result = getMostLeastCommonDiffAfterNInsertions template insertions 40
  printf "Task 2: result=%d\n" result

main = do
  content <- readFile inputFile
  let chars = lines content
  let (template, insertions) = getPolymerTemplate chars

  task1 template insertions
  task2 template insertions
