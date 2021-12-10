import Data.List (sort)
import Text.Printf

inputFile = "./input.txt"

type BracketDef = (Char, Char)

brackets = [('[',']'), ('{','}'), ('<','>'), ('(',')')]

getBracketScore :: Char -> Int
getBracketScore c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137

getOpenBracketCloseType :: Char -> [BracketDef] -> Char
getOpenBracketCloseType _ [] = error "Illegal character"
getOpenBracketCloseType compare ((open,close):definitions)
  | compare == open = close
  | otherwise = getOpenBracketCloseType compare definitions

charIsOpenBracket :: Char -> BracketDef -> Bool
charIsOpenBracket c (open,_) = c == open

isSomeOpenBracket :: Char -> Bool
isSomeOpenBracket c = length (filter (charIsOpenBracket c) brackets) == 1

charIsCloseBracket :: Char -> Char -> Bool
charIsCloseBracket maybeClose open = maybeClose == getOpenBracketCloseType open brackets

getIncorrectClosingCharacterLoop :: String -> [Char] -> Maybe Char
getIncorrectClosingCharacterLoop [] _ = Nothing
getIncorrectClosingCharacterLoop (char:line) []
  | isSomeOpenBracket char = getIncorrectClosingCharacterLoop line [char]
  | otherwise = error "Encountered closing bracket with empty stack"
getIncorrectClosingCharacterLoop (char:line) stack
  | isSomeOpenBracket char = getIncorrectClosingCharacterLoop line (char : stack)
  | charIsCloseBracket char (head stack) = getIncorrectClosingCharacterLoop line (tail stack)
  | otherwise = Just char

getIncorrectClosingCharacter :: String -> Maybe Char
getIncorrectClosingCharacter line = getIncorrectClosingCharacterLoop line []

getCorruptCharacterScore :: Maybe Char -> Int
getCorruptCharacterScore = maybe 0 getBracketScore

getCorruptScore :: [String] -> Int
getCorruptScore chars = sum (map (getCorruptCharacterScore . getIncorrectClosingCharacter) chars)

lineIsNotCorrupt :: String -> Bool
lineIsNotCorrupt line = case getIncorrectClosingCharacter line of
                          Just incorrect -> False
                          Nothing -> True

removeCorruptLines :: [String] -> [String]
removeCorruptLines = filter lineIsNotCorrupt

completeLineLoop :: String -> ([Char], String) -> String
completeLineLoop [] ([], result) = result
completeLineLoop [] (s:stack, result) =
  completeLineLoop [] (stack, result ++ [getOpenBracketCloseType s brackets])
completeLineLoop (char:line) (stack, result)
  | isSomeOpenBracket char = completeLineLoop line (char : stack, result)
  | charIsCloseBracket char (head stack) = completeLineLoop line (drop 1 stack, result)
  | otherwise = error "Corrupt line"

getLineCompletion :: String -> String
getLineCompletion line = completeLineLoop line ([], "")

getCompletionBracketScore :: Char -> Int
getCompletionBracketScore c
  | c == ')' = 1
  | c == ']' = 2
  | c == '}' = 3
  | c == '>' = 4

scoreLineCompletionLoop :: String -> Int -> Int
scoreLineCompletionLoop rest score =
  foldl (\ score c -> (score * 5) + getCompletionBracketScore c) score rest

scoreLineCompletion :: String -> Int
scoreLineCompletion line = scoreLineCompletionLoop line 0

scoreCompletedLine :: String -> Int
scoreCompletedLine line = scoreLineCompletion (getLineCompletion line)

median :: [Int] -> Int
median as = sort as !! (length as `div` 2)

scoreAllCompletedLines :: [String] -> Int
scoreAllCompletedLines chars = median (map scoreCompletedLine (removeCorruptLines chars))

task1 :: [String] -> IO ()
task1 chars = do
  let result = getCorruptScore chars
  printf "Task 1: score=%d\n" result

task2 :: [String] -> IO ()
task2 chars = do
  let result = scoreAllCompletedLines chars
  printf "Task 2: score=%d\n" result

main = do
  content <- readFile inputFile
  let chars = lines content

  task1 chars
  task2 chars
