{-# LANGUAGE LambdaCase #-}
import Data.List (findIndex, sortBy)
import Debug.Trace
import Text.Printf
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(second))

inputFile = "./input.txt"

toInt :: String -> Int
toInt c = read c :: Int

toIntChar c = toInt [c]

data ALU = ALU { w :: Int
               , x :: Int
               , y :: Int
               , z :: Int }

instance Eq ALU where
  (ALU _ _ _ z1) == (ALU _ _ _ z2) = mod z1 26 == mod z2 26

instance Show ALU where
  show (ALU w x y z) = "w="++show w++",x="++show x++",y="++show y++",z="++show z

type SetVar = ALU -> Int -> ALU

type GetVar = ALU -> Int

type GetSetVar = (GetVar, SetVar)

data InstructInp = InstructInp { setInp :: SetVar
                                  , ic :: String }

instance Show InstructInp where
  show (InstructInp _ lc) = "inp "++show lc

data InstructSet = InstructSet { iType :: String
                               , left :: GetVar
                               , right :: Either Int GetVar
                               , set :: SetVar
                               , lc :: String 
                               , rc :: String }

instance Show InstructSet where
  show (InstructSet iType _ _ _ lc rc) = show iType++" "++show lc++" "++show rc

type Instruction = Either InstructInp InstructSet

newtype Program = Program [Instruction]

instance Show Program where
  show (Program instructions) = concatMap ((++"\n") . show) instructions

runInstructionSet :: ALU -> InstructSet -> ALU
runInstructionSet alu (InstructSet iType left right set _ _)
  | iType == "add" = set alu (leftValue + rightValue)
  | iType == "mul" = set alu (leftValue * rightValue)
  | iType == "div" = set alu ((abs leftValue `div` rightValue) * (if leftValue < 0 then -1 else 1))
  | iType == "mod" = set alu (mod leftValue rightValue)
  | iType == "eql" = set alu (if leftValue == rightValue then 1 else 0)
  where
    leftValue = left alu
    rightValue = case right of
                   Left v -> v
                   Right getVar -> getVar alu

runProgramStep :: ALU -> Int -> Instruction -> (ALU, Bool)
runProgramStep alu input instruction = result
  where
    result =
      case instruction of
        Left (InstructInp set _) -> (set alu input, True)
        Right instruction -> (runInstructionSet alu instruction, False)

runFullProgram :: Program -> [Int] -> ALU
runFullProgram (Program program) = runLoop initialALU program
  where
    initialALU = ALU 0 0 0 0
    runLoop alu [] _ = alu
    runLoop alu (instruction:nextInstructions) inputs= runLoop nextALU nextInstructions nextInputs
      where
        (nextALU, wasInput) = runProgramStep alu (head inputs) instruction
        nextInputs = if wasInput then tail inputs else inputs

getFinalZValue :: Program -> Int -> Int
getFinalZValue program modelNumber = z (runFullProgram program inputs)
  where inputs = map toIntChar (show modelNumber)

modelNumberIsValid :: Program -> Int -> Bool
modelNumberIsValid program modelNumber
  | result == 0 = True
  | result > 0 = False
  | result < 0 = error "returned z was < 0; MONAD program invalid!"
  where result = getFinalZValue program modelNumber

changeModelNumber :: Int -> Int -> Int
changeModelNumber d n = if '0' `elem` show next then changeModelNumber d next else next
  where next = n + d

increment = changeModelNumber 1
decrement = changeModelNumber (-1)

findLargestValidModelNumber :: Program -> Int
findLargestValidModelNumber program = runLoop 99999936617577 -- 99999999999999
  where runLoop modelNumber
          | modelNumberIsValid program modelNumber = -- traceShow ("is valid; n="++show modelNumber)
              modelNumber
          | modelNumber > 11111111111111 = runLoop nextModelNumber
          where nextModelNumber = decrement modelNumber
-- 
-- findAllValidModelNumbers :: Program -> [Int]
-- findAllValidModelNumbers program = runLoop 11111111111111
--   where getNextModelNumber modelNumber
--           | '0' `elem` show next = getNextModelNumber next
--           | otherwise = next
--           where next = modelNumber + 1
--         runLoop modelNumber
--           | modelNumberIsValid program modelNumber = traceShow ("is valid; n="++show modelNumber)
--             (modelNumber:runLoop nextModelNumber)
--           | modelNumber < 99999999999999 = runLoop nextModelNumber
--           where nextModelNumber = traceShow ("next="++show modelNumber) (getNextModelNumber modelNumber)

getSetter :: String -> SetVar
getSetter str = case str of
  "w" -> (\ (ALU _ x y z) v -> ALU v x y z)
  "x" -> (\ (ALU w _ y z) v -> ALU w v y z)
  "y" -> (\ (ALU w x _ z) v -> ALU w x v z)
  "z" -> (\ (ALU w x y _) v -> ALU w x y v)

readProgram :: [String] -> Program
readProgram lines = Program (loop lines)
  where loop [] = []
        loop (c:chars)
          | ins == "inp" = Left (InstructInp (getSetter (pieces !! 1)) (pieces !! 1)) : loop chars
          | otherwise = Right nextInstruction : loop chars

          where pieces = words c
                ins = head pieces

                nextInstruction = InstructSet
                  ins
                  (case pieces !! 1 of
                    "w" -> w
                    "x" -> x
                    "y" -> y
                    "z" -> z
                  )
                  (case pieces !! 2 of
                    "w" -> Right w
                    "x" -> Right x
                    "y" -> Right y
                    "z" -> Right z
                    v -> Left (toInt v)
                  )
                  (getSetter (pieces !! 1))
                  (pieces !! 1)
                  (pieces !! 2)

filterOrderedUnique :: Eq b => [(a, b)] -> [(a, b)]
filterOrderedUnique items = loop items []
  where loop :: Eq b => [(a,b)] -> [(a,b)] -> [(a,b)]
        loop remaining reduction = if null filteredRemaining then reduction else next
          where filteredRemaining = filter (\ (_,b0) -> (not . any (\ (_,b1) -> b0 == b1)) reduction) remaining
                nextReduction = head filteredRemaining:reduction
                next = loop (tail filteredRemaining) nextReduction

runProgramSlice :: Program -> Int -> ALU -> ALU
runProgramSlice (Program []) _ alu = alu
runProgramSlice (Program (i:instructions)) input alu = runProgramSlice (Program instructions) input (fst (runProgramStep alu input i))

parseNumber :: [Int] -> Int
parseNumber = toInt . concatMap show . reverse

splitProgramToNextInput :: [Instruction] -> ([Instruction], [Instruction])
splitProgramToNextInput program =
  case nextInputIndex of
    Just i' -> (i:take i' instructions, drop i' instructions)
    Nothing -> (i:instructions, [])
  where (i:instructions) = program
        nextInputIndex = findIndex (\case
                                    Left _ -> True
                                    Right _ -> False) instructions

splitProgram :: Program -> [Program]
splitProgram (Program program) = map Program (loop program)
  where loop instructions = case nextInputIndex of
                              Nothing -> [instructions]
                              Just i -> take (i+1) instructions : loop (drop (i+1) instructions)
          where nextInputIndex = findIndex (\case
                                             Left _ -> True
                                             Right _ -> False) (drop 1 instructions)

isDivZ :: Program -> Bool
isDivZ (Program program) = any (\case
                                Right (InstructSet iType _ right _ lc _) ->
                                  case right of
                                    Left d -> iType == "div" && lc == "z" && d > 1
                                    Right _ -> False
                                Left _ -> False) program

findOptimalInput :: [Program] -> Maybe [Int]
findOptimalInput split = loop 0 (ALU 0 0 0 0) []
  where loop p alu ns
          | p >= length split = Nothing
          | p == length split - 1 && (not . null) finishedOption = Just ((fst . head) finishedOption : ns)
          | otherwise = loopNext nextOptions
          where prog = split !! p
                Program prog' = prog
                options = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
                finishedOption' = filter (\ (_, alu') -> z alu' == 0) options
                finishedOption = traceShow ("p="++show p++", finished="++show (map fst finishedOption'))
                  finishedOption'
                -- decreasedIndex = findIndex (\ (_, alu') -> z alu' < z alu) options
                -- nextOptions' = case decreasedIndex of
                --                 Nothing -> reverse options
                --                 Just i -> [options !! i]
                nextOptions' = if isDivZ prog
                                 then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options
                                 else options
                nextOptions = traceShow ("p="++show p++", opts="++show (map fst nextOptions')++", ns="++show ns)
                  nextOptions'
                loopNext [] = Nothing
                loopNext ((o, alu'):os) =
                  case loop (p+1) alu' (o:ns) of
                    Nothing -> loopNext os
                    Just result -> Just result

main = do
  content <- readFile inputFile
  let program = readProgram (lines content)
  let split = splitProgram program

  -- let optimalInput = findOptimalInput split
  -- print optimalInput
  let optimalInput = [9,9,3,9,2,8,7,9,9,6,9,9,2,1]
  let result = parseNumber optimalInput
  print result
  print (getFinalZValue program result)

  -- let p = 0
  -- let alu = ALU 0 0 0 0
  -- let ns = [] :: [Int]
  -- let zs = [z alu]

  -- let prog = split !! p

  -- let options = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map fst options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "a=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map fst options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "b=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map fst options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "c=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map fst options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "d=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "e=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "f=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "g=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "h=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "i=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "j=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "k=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)

  -- let p' = p+1
  -- let p = p'
  -- let i = 0
  -- let alu = (snd . (!!i)) options
  -- let ns' = ns ++ [(fst . (!!i)) options]
  -- let zs' = zs ++ [z alu]
  -- let ns = ns'
  -- let zs = zs'
  -- printf "l=%d: ns=%s, zs=%s\n---\n" (last ns) (concatMap show ns) (concatMap ((++",") . show) zs)

  -- let prog = split !! p

  -- let options' = (reverse . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
  -- let options = if isDivZ prog
  --                     then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options'
  --                     else options'
  -- printf "p=%d, isDiv=%s, " p ((show . isDivZ) prog)
  -- print (map (z . snd) options)
