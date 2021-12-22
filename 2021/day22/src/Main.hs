{-# LANGUAGE LambdaCase #-}
import Data.List.Split
import Text.Printf
import Data.Maybe (fromMaybe)

inputFile = "./input.txt"

data Cuboid = Cuboid { x :: (Int, Int)
                     , y :: (Int, Int)
                     , z :: (Int, Int) }

instance Show Cuboid where
  show (Cuboid (x1,x2) (y1,y2) (z1,z2)) = "x="++show x1++".."++show x2++","++
    " y="++show y1++".."++show y2++","++
    " z="++show z1++".."++show z2

data Step = Step { on :: Bool
                 , ranges :: Cuboid }

instance Show Step where
  show (Step on c) = "\n"++
    (if on then "on" else "off")++" "++show c

toInt :: String -> Int
toInt c = read c :: Int

getSteps :: [String] -> [Step]
getSteps = map getStep
  where getStep line = Step (onOrOff == "on") (Cuboid xs ys zs)
          where [onOrOff, ranges] = words line
                splitRange range = (l, r)
                  where [l, r] = map toInt (splitOn ".." (drop 2 range))
                [xs, ys, zs] = map splitRange (splitOn "," ranges)

truncateStep :: Int -> Int -> Step -> Maybe Step
truncateStep rMin rMax (Step on (Cuboid x y z)) =
  case [x', y', z'] of
    [Just x', Just y', Just z'] -> Just (Step on (Cuboid x' y' z'))
    _ -> Nothing

    where truncateRange (t0,t1)
            | t0 > rMax || t1 < rMin = Nothing
            | otherwise = Just (min rMax (max t0 rMin), max rMin (min t1 rMax))
          x' = truncateRange x
          y' = truncateRange y
          z' = truncateRange z

truncateSteps :: Int -> Int -> [Step] -> [Step]
truncateSteps _ _ [] = []
truncateSteps rMin rMax (s:ss) = case truncateStep rMin rMax s of
                                   Just s' -> s':truncateSteps rMin rMax ss
                                   Nothing -> truncateSteps rMin rMax ss

getOverlap :: Cuboid -> Cuboid -> Maybe Cuboid
getOverlap (Cuboid (ax1,ax2) (ay1,ay2) (az1,az2)) (Cuboid (bx1,bx2) (by1,by2) (bz1,bz2))
  | ox1 > ox2 || oy1 > oy2 || oz1 > oz2 = Nothing
  | otherwise = Just (Cuboid (ox1,ox2) (oy1,oy2) (oz1,oz2))
    where (ox1,ox2) = (max ax1 bx1, min ax2 bx2)
          (oy1,oy2) = (max ay1 by1, min ay2 by2)
          (oz1,oz2) = (max az1 bz1, min az2 bz2)

makeCuboid :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Maybe Cuboid
makeCuboid (x1,x2) (y1,y2) (z1,z2)
  | x1 > x2 || y1 > y2 || z1 > z2 = Nothing
  | otherwise = Just (Cuboid (x1,x2) (y1,y2) (z1,z2))

getPartOfCuboidNotOverlapping :: Cuboid -> Cuboid -> [Cuboid]
getPartOfCuboidNotOverlapping main subset =
  case overlap of
    Nothing -> [main]
    Just (Cuboid (ox1,ox2) (oy1,oy2) (oz1,oz2)) ->
      (map (\case
            Nothing -> error "filter didn't work"
            Just c' -> c') .
      filter (\case
              Nothing -> False
              Just _ -> True))
      [makeCuboid (x1,ox1-1) (y1,y2) (z1,z2)
      ,makeCuboid (ox2+1,x2) (y1,y2) (z1,z2)
      ,makeCuboid (ox1,ox2) (oy2+1,y2) (z1,z2)
      ,makeCuboid (ox1,ox2) (y1,oy1-1) (z1,z2)
      ,makeCuboid (ox1,ox2) (oy1,oy2) (oz2+1,z2)
      ,makeCuboid (ox1,ox2) (oy1,oy2) (z1,oz1-1)]
  where overlap = getOverlap main subset
        Cuboid (x1,x2) (y1,y2) (z1,z2) = main

switchOn :: [Cuboid] -> Cuboid -> [Cuboid]
switchOn switchedOn toSwitch = switchedOn ++ loop switchedOn [toSwitch]
  where loop _ [] = []
        loop [] parts = parts
        loop (o:os) parts = loop os (concatMap (`getPartOfCuboidNotOverlapping` o) parts)

switchOff :: [Cuboid] -> Cuboid -> [Cuboid]
switchOff switchedOn toSwitch = concatMap (`getPartOfCuboidNotOverlapping` toSwitch) switchedOn

countCuboids :: [Cuboid] -> Int
countCuboids = foldr (\ (Cuboid (x1,x2) (y1,y2) (z1,z2)) c -> c + (x2-x1+1)*(y2-y1+1)*(z2-z1+1)) 0

reduceNumSwitchedOn :: [Step] -> Int
reduceNumSwitchedOn steps = loop 0 []
  where loop i switchedOn
          | i >= length steps = countCuboids switchedOn
          | otherwise = loop (i+1) next
          where Step on cuboid = steps !! i
                next = (if on then switchOn else switchOff) switchedOn cuboid

getAllRanges :: Cuboid -> [String]
getAllRanges (Cuboid (x1,x2) (y1,y2) (z1,z2)) = loopX x1
  where loopX x
          | x <= x2 = loopY x y1 ++ loopX (x+1)
          | otherwise = []
        loopY x y
          | y <= y2 = loopZ x y z1 ++ loopY x (y+1)
          | otherwise = []
        loopZ x y z
          | z <= z2 = (show x++","++show y++","++show z) : loopZ x y (z+1)
          | otherwise = []

printRanges :: [Cuboid] -> String
printRanges = concatMap ((++"\n\n") . unlines . getAllRanges)

task1 :: [Step] -> Int
task1 = reduceNumSwitchedOn . truncateSteps (-50) 50

task2 :: [Step] -> Int
task2 = reduceNumSwitchedOn

main = do
  content <- readFile inputFile
  let steps = (getSteps . lines) content

  printf "Task 1: %d\n" (task1 steps)
  printf "Task 2: %d\n" (task2 steps)
