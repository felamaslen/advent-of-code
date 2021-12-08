import Data.List.Split
import Text.Printf
import Data.List (elemIndex, sort)

inputFile = "./input.txt"

toInt :: String -> Int
toInt s = read s :: Int

notEmpty :: String -> Bool
notEmpty a = not (null a)

getOutputValues :: String -> [String]
getOutputValues line = filter notEmpty (splitOn " " (last (splitOn "|" line)))

representsDigit :: Int -> String -> Bool
representsDigit d s
  | d == 1 = length s == 2
  | d == 4 = length s == 4
  | d == 7 = length s == 3
  | d == 8 = length s == 7
  | otherwise = False

countValuesOfDigit :: [String] -> Int -> Int
countValuesOfDigit outputValues digit = length (filter (representsDigit digit) outputValues)

type SegmentMap = [Char]

segments = ['a', 'b', 'c', 'd', 'e', 'f', 'g']

isMapped :: Char -> Bool
isMapped m = m /= '_'

applySegment :: Int -> Char -> SegmentMap -> SegmentMap
applySegment from to segmentMap
  | from == 0 = to : drop 1 segmentMap
  | otherwise = take from segmentMap ++ [to] ++ drop (from + 1) segmentMap

isNotIn s c = c `notElem` s

includesLetter :: Char -> String -> Bool
includesLetter c s = c `elem` s

stringDoesNotIncludeLetter :: Char -> Char -> Bool
stringDoesNotIncludeLetter c s = c /= s

hasLength :: Int -> String -> Bool
hasLength l s = length s == l

getDigit :: [String] -> Int -> String
getDigit recorded digit = head (filter (representsDigit digit) recorded)

deduceSegmentA :: [String] -> SegmentMap -> SegmentMap
deduceSegmentA recorded =
  applySegment 0 segmentA
  where
    one = getDigit recorded 1
    seven = getDigit recorded 7
    segmentA = head (filter (isNotIn one) seven)

deduceSegmentB :: [String] -> SegmentMap -> SegmentMap
deduceSegmentB recorded =
  applySegment 1 segmentB
  where
    one = getDigit recorded 1
    four = getDigit recorded 4
    three = head (
        filter (includesLetter (one !! 1)) (
          filter (includesLetter (head one)) (
            filter (hasLength 5) recorded
          )
        )
      )
    segmentB = head (filter (isNotIn three) four)

deduceSegmentD :: [String] -> SegmentMap -> SegmentMap
deduceSegmentD recorded segmentMap =
  applySegment 3 segmentD segmentMap
  where
    one = getDigit recorded 1
    four = getDigit recorded 4
    segmentD = head (
        filter (isNotIn one) (
          filter (stringDoesNotIncludeLetter (segmentMap !! 1)) four
        )
      )

deduceSegmentG :: [String] -> SegmentMap -> SegmentMap
deduceSegmentG recorded segmentMap =
  applySegment 6 segmentG segmentMap
  where
    one = getDigit recorded 1
    three = head (
        filter (includesLetter (one !! 1)) (
          filter (includesLetter (head one)) (
            filter (hasLength 5) recorded
          )
        )
      )
    seven = getDigit recorded 7
    segmentG = head (
        filter (isNotIn seven) (
          filter (stringDoesNotIncludeLetter (segmentMap !! 3)) three
        )
      )

deduceSegmentF :: [String] -> SegmentMap -> SegmentMap
deduceSegmentF recorded segmentMap =
  applySegment 5 segmentF segmentMap
  where
    five = head (
        filter (includesLetter (segmentMap !! 1)) (
          filter (hasLength 5) recorded
        )
      )
    segmentF = head (filter (isNotIn [head segmentMap, segmentMap !! 1, segmentMap !! 3, segmentMap !! 6]) five)

deduceSegmentC :: [String] -> SegmentMap -> SegmentMap
deduceSegmentC recorded segmentMap =
  applySegment 2 segmentC segmentMap
    where
      one = getDigit recorded 1
      segmentC = head (filter (stringDoesNotIncludeLetter (segmentMap !! 5)) one)

deduceSegmentE :: [String] -> SegmentMap -> SegmentMap
deduceSegmentE recorded segmentMap =
  applySegment 4 segmentE segmentMap
  where
    segmentE = head (filter (isNotIn segmentMap) segments)

deduceNextSegment :: [String] -> SegmentMap -> SegmentMap
deduceNextSegment recorded segmentMap
  | isMapped (segmentMap !! 2) = deduceSegmentE recorded segmentMap
  | isMapped (segmentMap !! 5) = deduceSegmentC recorded segmentMap
  | isMapped (segmentMap !! 6) = deduceSegmentF recorded segmentMap
  | isMapped (segmentMap !! 3) = deduceSegmentG recorded segmentMap
  | isMapped (segmentMap !! 1) = deduceSegmentD recorded segmentMap
  | isMapped (head segmentMap) = deduceSegmentB recorded segmentMap
  | otherwise = deduceSegmentA recorded segmentMap

deduceSegmentMap :: [String] -> SegmentMap -> SegmentMap
deduceSegmentMap recorded segmentMap
  | length (filter isMapped segmentMap) == length segmentMap = segmentMap
  | otherwise = deduceSegmentMap recorded (deduceNextSegment recorded segmentMap)

deduceSegments :: [String] -> SegmentMap
deduceSegments recorded = deduceSegmentMap
  recorded
  ['_', '_', '_', '_', '_', '_', '_']

indexOf :: Char -> String -> Int
indexOf a as = case elemIndex a as of
                 Just a -> a
                 Nothing -> -1

mapCharacter :: SegmentMap -> Char -> Char
mapCharacter segmentMap e = segments !! indexOf e segmentMap

mapDigit :: SegmentMap -> String -> String
mapDigit segmentMap digit = sort (map (mapCharacter segmentMap) digit)

getDecodedDigit :: String -> Int
getDecodedDigit s
  | s == "abcefg" = 0
  | s == "cf" = 1
  | s == "acdeg" = 2
  | s == "acdfg" = 3
  | s == "bcdf" = 4
  | s == "abdfg" = 5
  | s == "abdefg" = 6
  | s == "acf" = 7
  | s == "abcdefg" = 8
  | s == "abcdfg" = 9
  | otherwise = error "illegal digit"

decodeDigit :: SegmentMap -> String -> Int
decodeDigit segmentMap digit = getDecodedDigit (mapDigit segmentMap digit)

computeNumberFromDigits :: [Int] -> Int -> Int -> Int
computeNumberFromDigits digits index value
  | index >= length digits = value
  | otherwise = computeNumberFromDigits digits (index + 1) (value + digits !! index * 10 ^ (length digits - 1 - index))

deduceValue :: String -> Int
deduceValue line =
  computeNumberFromDigits digits 0 0
  where
    [recordedStrings, outputString] = splitOn " | " line
    segmentMap = deduceSegments (splitOn " " recordedStrings)
    digits = map (decodeDigit segmentMap) (splitOn " " outputString)

task1 :: [String] -> IO ()
task1 chars = do
  let outputValues = concatMap getOutputValues chars
  let result = sum (map (countValuesOfDigit outputValues) [1, 4, 7, 8])

  printf "Task 1: result=%d\n" result

task2 :: [String] -> IO ()
task2 chars = do
  let result = sum (map deduceValue chars)

  printf "Task 2: result=%d\n" result

main = do
  content <- readFile inputFile
  let chars = lines content

  task1 chars
  task2 chars
