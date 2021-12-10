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

task1 :: [String] -> IO ()
task1 chars = do
  let result = getCorruptScore chars
  printf "Task 1: score=%d\n" result

main = do
  content <- readFile inputFile
  let chars = lines content

  task1 chars
