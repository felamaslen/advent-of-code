{-# LANGUAGE LambdaCase #-}
import Data.Bifunctor.Compat.Repl (Bifunctor(bimap))
import Data.List (findIndex, sortBy)
import Data.List.Split
import Data.Maybe (fromMaybe)
import Debug.Trace
import Text.Printf

inputFile = "./input.txt"

data Room = Room { room :: Int
                 , top :: Bool }

instance Eq Room where
  (Room rA tA) == (Room rB tB) = rA == rB && tA == tB

data Amphipod = Amphipod { aId :: Int
                         , amphipodType :: String
                         , destinationRoom :: Int
                         , position :: Either Int Room
                         , moveCost :: Int }

instance Eq Amphipod where
  (Amphipod idA _ _ _ _) == (Amphipod idB _ _ _ _) = idA == idB

instance Show Amphipod where
  show (Amphipod aId t _ _ _) = show aId++" - "++show t

data Burrow = Burrow { hallway :: [Maybe Amphipod]
                     , rooms :: [[Maybe Amphipod]]
                     , roomSize :: Int
                     , cost :: Int }

instance Eq Burrow where
  (Burrow hA rA _ _) == (Burrow hB rB _ _) = hA == hB && rA == rB

instance Show Burrow where
  show (Burrow hallway rooms roomSize cost) = "cost="++show cost++"\n" ++
    concatMap (const "#") [0..12] ++ "\n#" ++
    map (\case
         Just amphipod -> (head.amphipodType) amphipod
         Nothing -> '.'
        ) hallway ++ "#\n###" ++
    concatMap (\ a -> maybe '.' (head . amphipodType) (head a) : "#") rooms ++ "##\n" ++
    concatMap (\ i -> "  #" ++ concatMap (\ a -> maybe '.' (head . amphipodType) (a !! i) : "#") rooms ++ "\n") [1..roomSize - 1]

type Simulation = [Burrow]

type Move = (Either Int (Int, Int), Either Int (Int, Int))

getAmphipodType :: Char -> String
getAmphipodType c
  | c == 'A' = "Amber"
  | c == 'B' = "Bronze"
  | c == 'C' = "Copper"
  | c == 'D' = "Desert"
  | otherwise = error ("Invalid amphipod type "++[c])

getMoveCost :: Char -> Int
getMoveCost c
  | c == 'A' = 1 -- amber
  | c == 'B' = 10 -- bronze
  | c == 'C' = 100 -- copper
  | c == 'D' = 1000 -- desert

getDestinationRoom :: Char -> Int
getDestinationRoom c
  | c == 'A' = 0
  | c == 'B' = 1
  | c == 'C' = 2
  | c == 'D' = 3

getRoomAmphipod :: Int -> Int -> Int -> Char -> Maybe Amphipod
getRoomAmphipod aId room index c
  | c == '.' = Nothing
  | otherwise = Just (Amphipod aId (getAmphipodType c) (getDestinationRoom c) (Right (Room room (index == 0))) (getMoveCost c))

getHallAmphipod :: Int -> Int -> Char -> Maybe Amphipod
getHallAmphipod aId pos c
  | c == '.' = Nothing
  | otherwise = Just (Amphipod aId (getAmphipodType c) (getDestinationRoom c) (Left pos) (getMoveCost c))

readInput :: [String] -> Burrow
readInput chars = Burrow hallway rooms roomSize 0
  where hall = chars !! 1
        hallChars = (init . drop 1) hall
        numHall = length hallChars
        hallway = map (\ i -> getHallAmphipod i i (hallChars !! i)) [0..numHall - 1]
        children = map (map head . splitOn "#" . take 7 . drop 3) ((init . drop 2) chars)
        numRooms = (length . head) children
        roomSize = length children
        rooms = map (\ i -> map (\ j -> getRoomAmphipod (i * roomSize + j) i j ((children !! j) !! i)) [0..roomSize - 1]) [0..numRooms - 1]

roomIndexToHallwayIndex i = 2 * (i+1)
hallwayIndexToRoomIndex h = h `div` 2 - 1

getPossibleMovesFromHallway :: Burrow -> [Burrow]
getPossibleMovesFromHallway (Burrow hallway rooms roomSize cost) = fromPosition 0
  where
    fromPosition :: Int -> [Burrow]
    fromPosition h
      | h < length hallway = next ++ fromPosition (h+1)
      | otherwise = []

      where nextHallway = take h hallway ++ [Nothing] ++ drop (h+1) hallway
            amphipod = hallway !! h
            next = case amphipod of
                     Nothing -> []
                     Just _ -> if freeToRoom then loopToRoom 0 else []

            Just (Amphipod _ _ dest _ moveCost) = amphipod
            hDest = roomIndexToHallwayIndex dest
            destRoom = rooms !! dest
            left = min hDest h
            right = max hDest h
            hallwaySteps = if h < hDest
                              then (take (right-left) . drop (left+1)) hallway
                              else (take (right-left) . drop left) hallway
            freeToRoom = not (any (\case
                                   Nothing -> False
                                   Just _ -> True) hallwaySteps)
            loopToRoom j =
              case destRoom !! j of
                Just _ -> []
                Nothing -> if j < length destRoom - 1
                              then case destRoom !! (j+1) of
                                     Just _ ->
                                       [burrowMovedToDestination | (not . any (\case
                                         Just (Amphipod _ _ neighbourDest _ _) -> neighbourDest /= dest
                                         Nothing -> False)) (drop (j+1) destRoom)]
                                     Nothing -> loopToRoom (j+1)
                              else [burrowMovedToDestination]
                  where burrowMovedToDestination = Burrow
                          nextHallway
                          (take dest rooms ++ [take j destRoom ++ [amphipod] ++ drop (j+1) destRoom] ++ drop (dest+1) rooms)
                          roomSize
                          (cost + moveCost * (abs (h - hDest) + (j+1)))

getPossibleMovesFromRooms :: Burrow -> [Burrow]
getPossibleMovesFromRooms burrow = getMoveOptionsFromRoom 0 0 []
  where
    Burrow hallway rooms roomSize cost = burrow

    getMoveOptionsFromRoom :: Int -> Int -> [Burrow] -> [Burrow]
    getMoveOptionsFromRoom i j reduction
      | i >= length rooms = reduction
      | j >= roomSize = getMoveOptionsFromRoom (i+1) 0 reduction
      | otherwise =
        case amphipod of
          Nothing -> getMoveOptionsFromRoom i (j+1) reduction
          Just _ -> case hallway !! h of
            Just _ -> getMoveOptionsFromRoom (i+1) 0 reduction
            Nothing ->
              case straightToDestinationOption of
                Just option -> [option]
                Nothing -> getMoveOptionsFromRoom (i+1) 0 (filteredHallwayOptions ++ reduction)

      where amphipod = (rooms !! i) !! j
            Just (Amphipod _ _ i' _ moveCost) = amphipod
            h = roomIndexToHallwayIndex i
            reducePossibleLeft j
              | j > 1 = case (hallway !! j, hallway !! (j-1)) of
                          (Nothing, Nothing) -> j : reducePossibleLeft (j-2)
                          _ -> []
              | j == 1 = case (hallway !! j, hallway !! (j-1)) of
                           (Nothing, Nothing) -> [j, j-1]
                           (Nothing, Just _) -> [j]
                           _ -> []
              | otherwise = []

            reducePossibleRight j
              | j < 9 = case (hallway !! j, hallway !! (j+1)) of
                          (Nothing, Nothing) -> j : reducePossibleRight (j+2)
                          _ -> []
              | j == 9 = case (hallway !! j, hallway !! (j+1)) of
                           (Nothing, Nothing) -> [j, j+1]
                           (Nothing, Just _) -> [j]
                           _ -> []
              | otherwise = []

            hallwayOptions = reducePossibleLeft (h-1) ++ reducePossibleRight (h+1)

            toHallwayOptions = map (\ hStep -> Burrow
               (take hStep hallway ++ [amphipod] ++ drop (hStep+1) hallway)
               roomsVacatedI
               roomSize
               (cost + moveCost * (abs (hStep - h) + (j+1)))
             ) hallwayOptions

            filteredHallwayOptions = if i' /= i || any (\case
                                                        Just (Amphipod _ _ neighbourDest _ _) -> neighbourDest /= i
                                                        Nothing -> True) (drop (j+1) (rooms !! i))
                                        then toHallwayOptions
                                        else []

            h' = roomIndexToHallwayIndex i'
            canMoveNextToDestRoom = any (\ o -> (o > h && (o+1) == h') || (o < h && (o-1) == h')) hallwayOptions

            straightToDestinationOption =
              if canMoveNextToDestRoom
                 then toDestinationLoop 0
                 else Nothing

              where toDestinationLoop j' =
                      case (rooms !! i') !! j' of
                        Nothing -> if j' < roomSize - 1
                                      then case (rooms !! i') !! (j'+1) of
                                             Just _ -> if any (\case
                                                               Just (Amphipod _ _ neighbourDest _ _) -> neighbourDest /= i'
                                                               Nothing -> False) (drop (j'+1) (rooms !! i'))
                                                          then Nothing
                                                          else burrowMovedToDestination
                                             Nothing -> toDestinationLoop (j'+1)
                                      else burrowMovedToDestination
                        Just _ -> Nothing

                      where destRoom = map (const Nothing) [0..j'-1] ++ [amphipod] ++ drop (j'+1) (rooms !! i')
                            burrowMovedToDestination = Just (Burrow
                              hallway
                              (take i' roomsVacatedI ++ [destRoom] ++ drop (i'+1) roomsVacatedI)
                              roomSize
                              (cost + moveCost * ((j+1) + abs (h'-h) + (j'+1))))

            nextRoom = map (const Nothing) [0..j] ++ drop (j+1) (rooms !! i)
            roomsVacatedI = take i rooms ++ [nextRoom] ++ drop (i+1) rooms

getPossibleMoves :: Simulation -> [Simulation]
getPossibleMoves simulation = map (:simulation) filteredMoves
  where (s:history) = simulation
        fromHallway = getPossibleMovesFromHallway s
        fromRooms = getPossibleMovesFromRooms s
        nextMoves = if null fromHallway then fromRooms else fromHallway
        filteredMoves = filter (`notElem` history) nextMoves

isFinishedBurrow :: Burrow -> Bool
isFinishedBurrow (Burrow _ rooms _ _) = (not . any (\ i ->
  any (\case
       Just (Amphipod _ _ dest _ _) -> dest /= i
       Nothing -> True
      ) (rooms !! i)
  )) [0..length rooms - 1]

isFinished :: Simulation -> Bool
isFinished (s:history) = isFinishedBurrow s

optimiseSimulation :: Simulation -> Maybe (Int, Simulation)
optimiseSimulation simulation = loop [simulation] Nothing
  where loop :: [Simulation] -> Maybe (Int, Simulation) -> Maybe (Int, Simulation)
        loop [] best = best
        loop children best
          | (not.null) finished && (cost.head.head) finished < bestCost = Just ((cost.head.head) finished, head finished)
          | (not.null) finished = best
          | otherwise = loop filteredSiblings firstResult

            where (s:rest) = children
                  bestCost = case best of
                               Nothing -> 100000
                               Just (c, _) -> c

                  finished = filter isFinished children

                  firstResult = loop (getPossibleMoves s) best

                  filteredSiblings = case firstResult of
                               Nothing -> rest
                               Just (bc, _) -> filter (\ sim -> (cost.head) sim < bc) rest

task1 :: [String] -> Int
task1 chars = maybe (error "could not find solution to task 1") fst (optimiseSimulation [burrow])
  where burrow = readInput chars

task2 :: [String] -> Int
task2 chars = maybe (error "could not find solution to task 2") fst (optimiseSimulation [burrow])
  where burrow = readInput (take 3 chars ++ ["  #D#C#B#A#\n"] ++ ["  #D#B#A#C#\n"] ++ drop 3 chars)

main = do
  content <- readFile inputFile
  let chars = lines content

  printf "Task 1: cost=%d\n" (task1 chars)
  printf "Task 2: cost=%d\n" (task2 chars)
