import Text.Printf

inputFile = "./input.txt"

data Snail = Snail { left :: Either Int Snail
                   , right :: Either Int Snail
                   }

printSnailPart :: Either Int Snail -> String
printSnailPart part = case part of
                        Left v -> show v
                        Right s -> printSnail s

printSnail :: Snail -> String
printSnail snail = "["++printSnailPart (left snail)++","++printSnailPart (right snail)++"]"

instance Show Snail where
  show (Snail left right) = printSnail (Snail left right)

printSnails :: [Snail] -> IO ()
printSnails [] = do return ()
printSnails (s:snails) = do
  print s
  printSnails snails

toInt :: Char -> Int
toInt c = read [c] :: Int

intValue :: Either Int Snail -> Int
intValue value = case value of
                   Left v -> v
                   Right s -> error ("value ("++show value++") is a snail not an int")

snailValue :: Either Int Snail -> Snail
snailValue value = case value of
                   Left v -> error ("value ("++show value++") is an int not a snail")
                   Right s -> s

isSimpleSnail :: Snail -> Bool
isSimpleSnail snail = case left snail of
                        Left l -> case right snail of
                                    Left r -> True
                                    Right r -> False
                        Right l -> False

getNestLevels :: Either Int Snail -> Int
getNestLevels snail =
  case snail of
    Left v -> 0
    Right s -> 1 + max (getNestLevels (left s)) (getNestLevels (right s))

getFirstNestedPair :: Int -> [Int] -> Snail -> [Int]
getFirstNestedPair threshold stack snail
  | threshold <= 0 =
    if isSimpleSnail snail
       then stack
       else case left snail of
              Right l -> getFirstNestedPair 0 (stack++[0]) l
              Left l -> case right snail of
                          Right r -> getFirstNestedPair 0 (stack++[1]) r
                          Left r -> error "could not find a nested pair"

  | nestLevelsLeft > threshold = getFirstNestedPair (threshold-1) (stack++[0]) (snailValue (left snail))
  | nestLevelsRight <= threshold = []
  | otherwise = getFirstNestedPair (threshold-1) (stack++[1]) (snailValue (right snail))

  where nestLevelsLeft = getNestLevels (left snail)
        nestLevelsRight = getNestLevels (right snail)

getNestedPairForExplode = getFirstNestedPair 3 []

getSnailChild :: Snail -> [Int] -> Either Int Snail
getSnailChild snail [] = Right snail
getSnailChild snail (s:stack) =
  case next of
    Right v -> getSnailChild v stack
    Left v -> if null stack then Left v else error "reached simple value before stack was clear"
  where next = if s == 0 then left snail else right snail

getLastSingleValue :: Snail -> [Int] -> [Int]
getLastSingleValue snail stack =
  case elem of
    Left v -> stack
    Right sibling -> case right sibling of
                     Left v -> stack++[1]
                     Right s -> getLastSingleValue snail (stack++[1])
  where elem = getSnailChild snail stack

getSingleValueToLeftOfPairLoop :: Bool -> Snail -> [Int] -> [Int]
getSingleValueToLeftOfPairLoop _ _ [] = []
getSingleValueToLeftOfPairLoop hasDriftedLeft snail stack
  | last stack == 1 = getLastSingleValue snail (init stack ++ [0])
  | last stack == 0 && hasDriftedLeft = getLastSingleValue snail stack
  | length stack == 1 = []
  | otherwise = getSingleValueToLeftOfPairLoop (hasDriftedLeft || (last.init) stack == 1) snail ((init.init) stack ++ [0])

getSingleValueToLeftOfPair = getSingleValueToLeftOfPairLoop False

getFirstSingleValue :: Snail -> [Int] -> [Int]
getFirstSingleValue snail stack =
  case elem of
    Left v -> stack
    Right sibling -> case left sibling of
                       Left v -> stack++[0]
                       Right s -> getFirstSingleValue snail (stack++[0])
  where elem = getSnailChild snail stack

getSingleValueToRightOfPair :: Snail -> [Int] -> [Int]
getSingleValueToRightOfPair snail stack
  | null stack = []
  | last stack == 1 = getSingleValueToRightOfPair snail (init stack)
  | otherwise = getFirstSingleValue snail (init stack ++ [1])

setAtIndex :: (Either Int Snail -> Either Int Snail) -> Snail -> [Int] -> Snail
setAtIndex getNewValue snail stack
  | null stack = snail
  | length stack == 1 =
    case head stack of
      0 -> Snail (getNewValue (left snail)) (right snail)
      1 -> Snail (left snail) (getNewValue (right snail))
  | otherwise =
    case head stack of
      0 -> case left snail of
             Right l -> Snail (Right (setAtIndex getNewValue l (tail stack))) (right snail)
             Left l -> error ("reached single value (l) with non-empty stack "++show stack)
      1 -> case right snail of
             Right r -> Snail (left snail) (Right (setAtIndex getNewValue r (tail stack)))
             Left r -> error ("reached single value (r) with non-empty stack "++show stack)

explodeSnailForSure :: Snail -> [Int] -> (Bool, Snail)
explodeSnailForSure snail firstNestedPair = (True, replacedWithZero)
  where leftOf = getSingleValueToLeftOfPair snail firstNestedPair
        rightOf = getSingleValueToRightOfPair snail firstNestedPair
        pairValues = getSnailChild snail firstNestedPair
        leftValue = (intValue . left . snailValue) pairValues
        rightValue = (intValue . right . snailValue) pairValues

        incrementedLeft = setAtIndex (\ v -> Left (intValue v + leftValue)) snail leftOf
        incrementedRight = setAtIndex (\ v -> Left (intValue v + rightValue)) incrementedLeft rightOf
        replacedWithZero = setAtIndex (\ _ -> Left 0) incrementedRight firstNestedPair

explodeSnail :: Snail -> (Bool, Snail)
explodeSnail snail
  | null firstNestedPair = (False, snail)
  | otherwise = explodeSnailForSure snail firstNestedPair
  where firstNestedPair = getNestedPairForExplode snail

splitSnailValue :: Int -> Snail
splitSnailValue n = Snail (Left l) (Left r)
  where l = n `div` 2
        r = n - l

splitSnailLoop :: Snail -> Bool -> (Bool, Snail)
splitSnailLoop snail True = (True, snail)
splitSnailLoop snail False =
  case left snail of
    Left l -> if l >= 10
                 then (True, Snail (Right (splitSnailValue l)) (right snail))
                 else case right snail of
                        Left r -> if r >= 10
                                     then (True, Snail (Left l) (Right (splitSnailValue r)))
                                     else (False, snail)
                        Right r -> (isSplit, Snail (Left l) (Right next))
                          where (isSplit, next) = splitSnailLoop r False
    Right l -> if isSplit
                  then (isSplit, Snail (Right next) (right snail))
                  else case right snail of
                         Left r -> if r >= 10
                                     then (True, Snail (Right l) (Right (splitSnailValue r)))
                                     else (False, snail)
                         Right r -> (isRightSplit, Snail (Right l) (Right nextRight))
                           where (isRightSplit, nextRight) = splitSnailLoop r False
                  where (isSplit, next) = splitSnailLoop l False

splitSnail :: Snail -> (Bool, Snail)
splitSnail snail = splitSnailLoop snail False

reduceSnail :: Snail -> Snail
reduceSnail snail
  | isExploded = reduceSnail explodedSnail
  | isSplit = reduceSnail snailWithSplit
  | otherwise = snail
  where
    (isExploded, explodedSnail) = explodeSnail snail
    (isSplit, snailWithSplit) = if isExploded then (False, explodedSnail) else splitSnail snail

addSnails :: Snail -> Snail -> Snail
addSnails a b = reduceSnail (Snail (Right a) (Right b))

sumSnails :: [Snail] -> Snail
sumSnails snails
  | length snails == 2 = addSnails (head snails) (last snails)
  | otherwise = sumSnails (addSnails s (head rest) : tail rest)
    where (s:rest) = snails

getPartMagnitude :: Either Int Snail -> Int
getPartMagnitude part = case part of
                          Left v -> v
                          Right s -> getMagnitude s

getMagnitude :: Snail -> Int
getMagnitude snail = 3 * getPartMagnitude (left snail) + 2 * getPartMagnitude (right snail)

getMaximumFromIndexToRest :: [Snail] -> Int -> Int
getMaximumFromIndexToRest snails i = maximum (map (getMagnitude . addSnails current) rest)
  where rest = take i snails ++ drop (i+1) snails
        current = snails !! i

getLargestSumMagnitude :: [Snail] -> Int
getLargestSumMagnitude snails = maximum (map (getMaximumFromIndexToRest snails) [0..(length snails - 1)])

parseSnailLoop :: String -> [Either Int Snail] -> Snail
parseSnailLoop "" stack
  | length stack /= 1 = error ("Invalid final stack length of "++show (length stack))
  | otherwise = case head stack of
                  Right snail -> snail
                  Left v -> error ("Invalid final stack value of "++show v)
parseSnailLoop str stack
  | char == '[' = parseSnailLoop (tail str) stack
  | char == ']' = parseSnailLoop (tail str) (Right (Snail (stack !! 1) (head stack)) : drop 2 stack)
  | char == ',' = parseSnailLoop (tail str) stack
  | otherwise = parseSnailLoop (tail str) (Left (toInt (head str)) : stack)

  where char = head str

parseSnail :: String -> Snail
parseSnail str = parseSnailLoop str []

task1 :: [Snail] -> IO ()
task1 snails = do
  let result = getMagnitude (sumSnails snails)
  printf "Task 1: result=%d\n" result

task2 :: [Snail] -> IO ()
task2 snails = do
  let result = getLargestSumMagnitude snails
  printf "Task 2: result=%d\n" result

main :: IO ()
main = do
  content <- readFile inputFile
  let snails = map parseSnail (lines content)

  task1 snails
  task2 snails
