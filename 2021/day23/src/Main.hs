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
                     , rooms :: [(Maybe Amphipod, Maybe Amphipod)]
                     , cost :: Int }

instance Eq Burrow where
  (Burrow hA rA _) == (Burrow hB rB _) = hA == hB && rA == rB

instance Show Burrow where
  show (Burrow hallway rooms cost) = "cost="++show cost++"\n" ++
    concatMap (const "#") [0..12] ++ "\n#" ++
    map (\case
         Just amphipod -> (head.amphipodType) amphipod
         Nothing -> '.'
        ) hallway  ++ "#\n###" ++
    [maybe '.' (head . amphipodType) ((fst . (!!0)) rooms)] ++ "#" ++
    [maybe '.' (head . amphipodType) ((fst . (!!1)) rooms)] ++ "#" ++
    [maybe '.' (head . amphipodType) ((fst . (!!2)) rooms)] ++ "#" ++
    [maybe '.' (head . amphipodType) ((fst . (!!3)) rooms)] ++ "###\n  #" ++
    [maybe '.' (head . amphipodType) ((snd . (!!0)) rooms)] ++ "#" ++
    [maybe '.' (head . amphipodType) ((snd . (!!1)) rooms)] ++ "#" ++
    [maybe '.' (head . amphipodType) ((snd . (!!2)) rooms)] ++ "#" ++
    [maybe '.' (head . amphipodType) ((snd . (!!3)) rooms)] ++ "#\n  " ++
    map (const '#') [0..8] ++ "\n"

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
readInput chars = Burrow hallway amphipods 0
  where [_, hall, l0, l1, _] = chars
        hallChars = (init . drop 1) hall
        numHall = length hallChars
        hallway = map (\ i -> getHallAmphipod i i (hallChars !! i)) [0..numHall - 1]
        top = map head (splitOn "#" ((take 7 . drop 3) l0))
        bottom = map head (splitOn "#" ((take 7 . drop 3) l1))
        children = zip top bottom
        amphipods = map (\ i -> bimap (getRoomAmphipod (numHall+i*2) i 0) (getRoomAmphipod (numHall+i*2+1) i 1) ((!!i) children)) [0..length children - 1]

roomIndexToHallwayIndex i = 2 * (i+1)
hallwayIndexToRoomIndex h = h `div` 2 - 1

getPossibleMovesFromHallway :: Burrow -> [Burrow]
getPossibleMovesFromHallway (Burrow hallway rooms cost) = fromPosition 0
  where
    fromPosition :: Int -> [Burrow]
    fromPosition i
      | i < length hallway = next ++ fromPosition (i+1)
      | otherwise = []
      where next = case hallway !! i of
                     Nothing -> []
                     Just amphipod ->
                       if freeToRoom
                          then case destRoom of
                                 (Nothing, Nothing) ->
                                   [Burrow nextHallway (take dest rooms ++ [(Nothing, Just amphipod)] ++ drop (dest+1) rooms) (cost + moveCost * (hallwayDiff + 2))]
                                 (Nothing, Just neighbour) ->
                                   [Burrow nextHallway (take dest rooms ++ [(Just amphipod, Just neighbour)] ++ drop (dest+1) rooms) (cost + moveCost * (hallwayDiff + 1))
                                     | destinationRoom neighbour == dest]
                                 _ -> []

                           else []
                       where (Amphipod _ _ dest _ moveCost) = amphipod
                             destHallwayIndex = roomIndexToHallwayIndex dest
                             left = min destHallwayIndex i
                             right = max destHallwayIndex i
                             hallwaySteps = if i < destHallwayIndex
                                               then (take (right-left) . drop (left+1)) hallway
                                               else (take (right-left) . drop left) hallway
                             freeToRoom = not (any (\case
                                                    Nothing -> False
                                                    Just a -> True) hallwaySteps)
                             hallwayDiff = abs (roomIndexToHallwayIndex dest - i)
                             destRoom = rooms !! dest
                             nextHallway = take i hallway ++ [Nothing] ++ drop (i+1) hallway

getPossibleMovesFromRooms :: Burrow -> [(Burrow, Int)]
getPossibleMovesFromRooms burrow = fromRoom 0
  where
    Burrow hallway rooms cost = burrow

    getMoveOptionsFromRoom :: Int -> Amphipod -> Int -> [(Burrow, Int)]
    getMoveOptionsFromRoom i amphipod roomPosition =
      case hallway !! h of
        Just _ -> []
        Nothing -> case straightToDestination of
                     Nothing -> map (\ j -> (
                         Burrow
                           (take j hallway ++ [Just amphipod] ++ drop (j+1) hallway)
                           (take i rooms ++ [nextRoom] ++ drop (i+1) rooms)
                           (cost + moveCost * (abs (j-h) + fromCost))
                        ,destinationRoom amphipod
                       )) hallwayOptions
                     Just option -> [option]

      where (Amphipod _ _ dest _ moveCost) = amphipod
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

            straightToDestinationOptions =
              (filter (\case
                      Nothing -> False
                      Just _ -> True ) .
              map ((\case
                   Nothing -> Nothing
                   Just o ->
                     case (hallway !! o, rooms !! room) of
                     (Just _, _) -> Nothing
                     (Nothing, (Nothing, Just neighbour)) ->
                       if destinationRoom neighbour == room
                         then Just (Burrow hallway movedFrom (cost + moveCost * (abs (o-h) + fromCost + 1)), (-1)::Int)
                         else Nothing
                         where movedTo = take room rooms ++ [(Just amphipod, Just neighbour)] ++ drop (room+1) rooms
                               movedFrom = take i movedTo ++ [nextRoom] ++ drop (i+1) movedTo
                     (Nothing, (Nothing, Nothing)) ->
                       Just (Burrow hallway movedFrom (cost + moveCost * (abs (o-h) + fromCost + 2)), (-1)::Int)
                         where movedTo = take room rooms ++ [(Nothing, Just amphipod)] ++ drop (room+1) rooms
                               movedFrom = take i movedTo ++ [nextRoom] ++ drop (i+1) movedTo
                     _ -> Nothing

                    where room = hallwayIndexToRoomIndex o) .

                  (\ o ->
                    if o > h && hallwayIndexToRoomIndex (o+1) == dest
                       then Just (o+1)
                       else if o < h && hallwayIndexToRoomIndex (o-1) == dest
                       then Just (o-1)
                       else Nothing))) hallwayOptions

            straightToDestination = if null straightToDestinationOptions
                                       then Nothing
                                       else head straightToDestinationOptions

            nextRoom = if roomPosition == 0
                          then (Nothing, (snd . (!!i)) rooms)
                          else (Nothing, Nothing)
            fromCost = roomPosition + 1

    fromRoom :: Int -> [(Burrow, Int)]
    fromRoom i
      | i < length rooms = next ++ fromRoom (i+1)
      | otherwise = []
      where next = case rooms !! i of
                     (Nothing, Nothing) -> []
                     (Nothing, Just amphipod) -> getMoveOptionsFromRoom i amphipod 1
                     (Just amphipod, Just neighbour) ->
                       if destinationRoom amphipod == destinationRoom neighbour && destinationRoom amphipod == i
                          then []
                          else getMoveOptionsFromRoom i amphipod 0

-- heuristic :: Burrow -> Int
-- heuristic burrow = costHeuristic
--   where allRooms = rooms burrow
--         costHeuristic = sum (map (\ i ->
--           case allRooms !! i of
--             (Nothing, Nothing) -> 2 * 10 ^ i
--             (Nothing, Just (Amphipod _ _ dest _ _)) -> 10 ^ i * (if dest == i then 1 else 2)
--             (Just (Amphipod _ _ dest _ _), Nothing) -> 10 ^ i * (if dest == i then 1 else 2)
--             (Just (Amphipod _ _ destTop _ _), Just (Amphipod _ _ destBottom _ _)) ->
--               10 ^ i * ((if destTop == i then 0 else 1) + (if destBottom == i then 0 else 1))
--           ) [0..length allRooms - 1])

-- heuristic :: Burrow -> Int
-- heuristic burrow = 2 * length allRooms - numRoomCellsFilled
--   where allRooms = rooms burrow
--         numRoomCellsFilled = sum (map (\ i ->
--           case allRooms !! i of
--             (Nothing, Nothing) -> 0
--             (Nothing, Just (Amphipod _ _ dest _ _)) -> if dest == i then 1 else 0
--             (Just (Amphipod _ _ dest _ _), Nothing) -> if dest == i then 1 else 0
--             (Just (Amphipod _ _ destTop _ _), Just (Amphipod _ _ destBottom _ _)) ->
--               (if destTop == i then 1 else 0) + (if destBottom == i then 1 else 0)
--           ) [0..length allRooms - 1])

getPossibleMoves :: Int -> Simulation -> [Simulation]
getPossibleMoves maxCost simulation = map (:simulation) filteredMoves
  where (s:history)= simulation
        fromHallway = getPossibleMovesFromHallway s
        fromRooms = getPossibleMovesFromRooms s
        nextMoves = if null fromHallway then map fst fromRooms else fromHallway
        -- allPossibleMoves = getPossibleMovesFromHallway s ++
        --   (map fst . sortBy (\ (_, a) (_, b) -> compare a b)) (getPossibleMovesFromRooms s)
        -- filteredMoves = (filter (\ a -> cost a < maxCost) . filter (`notElem` history)) allPossibleMoves
        filteredMoves = filter (`notElem` history) nextMoves

isFinishedBurrow :: Burrow -> Bool
isFinishedBurrow (Burrow _ rooms _) = not (any (\ i ->
  case rooms !! i of
    (Nothing, _) -> True
    (_, Nothing) -> True
    (Just (Amphipod _ _ destA _ _), Just (Amphipod _ _ destB _ _)) -> destA /= destB || destA /= i
  ) [0..length rooms - 1])

isFinished :: Simulation -> Bool
isFinished (s:history) = isFinishedBurrow s

optimiseSimulation :: Simulation -> Maybe (Int, Simulation)
optimiseSimulation simulation = loop 0 [simulation] Nothing
  where loop :: Int -> [Simulation] -> Maybe (Int, Simulation) -> Maybe (Int, Simulation)
        loop depth [] best = best
        loop depth children best
          | depth >= 20 = best
          | (not.null) finished && (cost.head.head) finished < bestCost =
            traceShow ("found! depth="++show depth++", cost="++show ((cost.head.head) finished)) (Just ((cost.head.head) finished, head finished))
          | (not.null) finished = best
          | otherwise = loop depth filteredSiblings firstResult

          -- | isFinished s = traceShow ("found! cost="++show ((cost.head) s)) (loop depth filteredSiblings (Just nextBest))
          -- | otherwise = loop depth filteredAncestors firstResult
            where (s:rest) = children
                  bestCost = case best of
                               Nothing -> 100000
                               Just (c, _) -> c

                  finished = (sortBy (\ a b -> compare ((cost.head) a) ((cost.head) b)) . filter isFinished) children

                  firstResult = loop (depth+1) (getPossibleMoves bestCost s) best

                  filteredSiblings = case firstResult of
                               Nothing -> rest
                               Just (bc, _) -> filter (\ sim -> (cost.head) sim < bc) rest


                  -- thisCost = cost (head s)
                  -- nextBest = case best of
                  --              Nothing -> (thisCost, s)
                  --              Just (bc, bs) -> if thisCost < bc then (thisCost, s) else (bc, bs)
                  -- filteredSiblings = filter (\ sim -> (cost . head) sim < fst nextBest) rest

                  -- firstMoves = (getPossibleMoves bestCost s)

                  -- firstResult = traceShow ("getPossibleMoves; depth="++show depth++", bestCost="++show bestCost++", nmoves="++show (length firstMoves)) (loop (depth+1) firstMoves best)

                  -- filteredAncestors = case firstResult of
                  --                       Nothing -> rest
                  --                       Just (max, _) -> filter (\ sim -> (cost . head) sim < max) rest

main = do
  content <- readFile inputFile
  let burrow = readInput (lines content)
  print burrow

  -- let simulation = [burrow]
  -- let best = Nothing
  -- let bestCost = case best of
  --                  Nothing -> 100000
  --                  Just (c, _) -> c
  -- let (s:rest) = [simulation]
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- let (s:rest) = moves
  -- let moves = getPossibleMoves bestCost s
  -- printf "nmoves=%d\n" (length moves)

  -- printf "finished index=%s, cost=%d\n" (show (findIndex isFinished moves)) ((cost.(!!0).(!!0)) moves)
  -- print (length moves)

  -- let (s:rest) = moves

  -- let nextSimulation = getPossibleMoves simulation
  -- print nextSimulation

  let result = optimiseSimulation [burrow]
  print result
  -- printf "%s" (concatMap ((++"\n") . show) (reverse stack))
  
  -- let bestCost = Nothing
  -- let bestStack = []
  -- let stack = []
  -- let (n:next) = [burrow]
  -- let nextStack = n:stack
  -- let nIsFinished = isFinished n
  -- let validMoves = (filter (`notElem` stack) .
  --                 (case bestCost of
  --                    Nothing -> id
  --                    Just c -> filter (\ b -> cost b < c)
  --                 ) .
  --                 getPossibleMoves) n

  -- print validMoves

  -- printf "NEXT OPTIONS=\n"
  -- print ((getPossibleMoves) burrow)
