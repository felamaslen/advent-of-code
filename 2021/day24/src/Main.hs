{-# LANGUAGE LambdaCase #-}
import Data.List (findIndex, sortBy)
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

runProgramSlice :: Program -> Int -> ALU -> ALU
runProgramSlice (Program []) _ alu = alu
runProgramSlice (Program (i:instructions)) input alu = runProgramSlice (Program instructions) input (fst (runProgramStep alu input i))

parseNumber :: [Int] -> Int
parseNumber = toInt . concatMap show . reverse

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

findOptimalInput :: ([(Int, ALU)] -> [(Int, ALU)]) -> [Program] -> Maybe [Int]
findOptimalInput reverseOrId split = loop 0 (ALU 0 0 0 0) []
  where loop p alu ns
          | p >= length split = Nothing
          | p == length split - 1 && (not . null) finishedOption = Just ((fst . head) finishedOption : ns)
          | otherwise = loopNext nextOptions
          where prog = split !! p
                Program prog' = prog
                options = (reverseOrId . map (\ i -> (i, runProgramSlice prog i alu))) [1..9]
                finishedOption = filter (\ (_, alu') -> z alu' == 0) options
                nextOptions = if isDivZ prog
                                then filter (\ (_, alu') -> z alu' <= z alu `div` 26) options
                                else options
                loopNext [] = Nothing
                loopNext ((o, alu'):os) =
                  case loop (p+1) alu' (o:ns) of
                    Nothing -> loopNext os
                    Just result -> Just result

findLargestInput = findOptimalInput reverse
findSmallestInput = findOptimalInput id

task1 = maybe 0 parseNumber . findLargestInput
task2 = maybe 0 parseNumber . findSmallestInput

main = do
  content <- readFile inputFile
  let program = (splitProgram . readProgram) (lines content)

  printf "Task 1: largest valid model = %d\n" (task1 program)
  printf "Task 2: largest valid model = %d\n" (task2 program)
