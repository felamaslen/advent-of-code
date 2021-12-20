{-# LANGUAGE LambdaCase #-}
import Data.List.Split
import Debug.Trace
import Text.Printf
import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe)

inputFile = "./input.txt"

toInt :: String -> Int
toInt c = read c :: Int

data Point = Point { x :: Int
                   , y :: Int
                   , z :: Int }

instance Show Point where
  show (Point x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Eq Point where
  (Point x1 y1 z1) == (Point x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

type Dist = Int

dist :: Point -> Point -> Dist -- cartesian distance squared
dist (Point x1 y1 z1) (Point x2 y2 z2) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

-- 3D transform matrix
data Orientation = Orientation { row0 :: Point
                               , row1 :: Point
                               , row2 :: Point
                               }

instance Show Orientation where
  show (Orientation (Point o11 o12 o13) (Point o21 o22 o23) (Point o31 o32 o33)) = "\n"++
    show o11++" "++show o12++" "++show o13++"\n"++
    show o21++" "++show o22++" "++show o23++"\n"++
    show o31++" "++show o32++" "++show o33++"\n"

instance Eq Orientation where
  (Orientation a1 a2 a3) == (Orientation b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3

data Scanner = Scanner { index :: Int
                       , position :: Maybe Point
                       , orientation :: Maybe Orientation
                       , beacons :: [Point]
                       }

zeroOrientation = Orientation (Point 1 0 0) (Point 0 1 0) (Point 0 0 1)

printBeacons :: [Point] -> String
printBeacons (b:bs)
  | null bs = show b++"\n\n"
  | otherwise = show b++",\n"++printBeacons bs

instance Show Scanner where
  show (Scanner index position orientation beacons) = "\n--- scanner "++
    show index++" ("++show position++", "++show orientation++") ---\n"++
      printBeacons beacons

instance Eq Scanner where
  (Scanner i _ _ _) == (Scanner j _ _ _) = i == j

getScannerLoop :: [String] -> Scanner -> (Scanner, [String])
getScannerLoop [] scanner = (scanner, [])
getScannerLoop (l:lines) scanner
  | null l = (scanner, lines)
  | otherwise = getScannerLoop lines
    (Scanner
      (index scanner)
      (position scanner)
      (orientation scanner)
      (beacons scanner ++ [Point x y z])
    )
    where [x,y,z] = map toInt (splitOn "," l)

getInitialScanners :: [String] -> [Scanner]
getInitialScanners [] = []
getInitialScanners (l:lines) = nextScanner : getInitialScanners nextLines
  where [_, _, scannerIndex, _] = words l
        isZero = toInt scannerIndex == 0
        (nextScanner, nextLines) = getScannerLoop lines
          (Scanner
            (toInt scannerIndex)
            (if isZero then Just (Point 0 0 0) else Nothing)
            (if isZero then Just zeroOrientation else Nothing)
            []
          )

type OverlapReduction = ([Point], [[Point]]) -- points from A, possible match groups from B

extendsGroup :: [Point] -> Point -> [Point] -> Point -> Bool
extendsGroup group a candidate b = distInA == distInB
  where distInA = sort (map (`dist` a) group)
        distInB = sort (map (`dist` b) candidate)

filterUniqueShapes :: [OverlapReduction] -> [OverlapReduction]
filterUniqueShapes [] = []
filterUniqueShapes ((a0,bs):rs) = (a0,bs) : filterUniqueShapes (filter (\ (a1,_) -> not (any (`elem` a0) a1)) rs)

sortByDistance :: Point -> [Point] -> [Point]
sortByDistance p = sortBy (\ p1 p2 -> compare (p1 `dist` p) (p2 `dist` p))

extendReduction :: [Point] -> [Point] -> OverlapReduction -> [OverlapReduction]
extendReduction _ _ (aGroup, []) = []
extendReduction as bs (aGroup, bCandidates) = result
  where aCandidates = filter (`notElem` aGroup) as

        loopThroughACandidates [] = []
        loopThroughACandidates (ac0:acRest) =
          if null bCandidates
             then []
             else loopThroughBCandidates bCandidates ++ loopThroughACandidates acRest

          where loopThroughBCandidates [] = []
                loopThroughBCandidates (bc0:bcRest) =
                  if null bExtensionCandidates
                     then loopThroughBCandidates bcRest
                     else loopThroughExtensionCandidates bExtensionCandidates ++ loopThroughBCandidates bcRest

                  where bExtensionCandidates = filter (`notElem` bc0) bs
                        loopThroughExtensionCandidates [] = []
                        loopThroughExtensionCandidates (be0:beRest) =
                          if extendsGroup aGroup ac0 bc0 be0
                             then (sortByDistance ac0 (ac0:aGroup), [sortByDistance be0 (be0:bc0)]) : loopThroughExtensionCandidates beRest
                             else loopThroughExtensionCandidates beRest

        nextReductions = loopThroughACandidates aCandidates
        filteredReductions = filterUniqueShapes nextReductions
        result = if length aGroup >= 12 || null filteredReductions
                    then [(aGroup, bCandidates)]
                    else concatMap (extendReduction as bs) filteredReductions

getLargestOverlappingShapeFromAToB :: [Point] -> [Point] -> Maybe ([Point], [Point])
getLargestOverlappingShapeFromAToB [] _ = Nothing
getLargestOverlappingShapeFromAToB (a:as) bs
  | length shapeA >= 12 = Just (shapeA, head shapesB)
  | otherwise = next
  where bCandidates = map (: []) bs
        initialReduction = ([a], bCandidates)
        (shapeA, shapesB) = head (extendReduction as bs initialReduction)
        next = getLargestOverlappingShapeFromAToB as bs

getOverlappingBeacons :: Scanner -> Scanner -> Maybe ([Point], [Point])
getOverlappingBeacons scannerA scannerB = getLargestOverlappingShapeFromAToB (beacons scannerA) (beacons scannerB)

quarterTurn :: Int -> (Int, Int)
quarterTurn i
  | i == 0 = (1,0)
  | i == 1 = (0,1)
  | i == 2 = (-1,0)
  | i == 3 = (0,-1)

orientRotateAroundX q = Orientation (Point 1 0 0) (Point 0 c (-s)) (Point 0 s c)
  where (c, s) = quarterTurn q

orientRotateAroundY q = Orientation (Point c 0 s) (Point 0 1 0) (Point (-s) 0 c)
  where (c, s) = quarterTurn q

orientRotateAroundZ q = Orientation (Point c (-s) 0) (Point s c 0) (Point 0 0 1)
  where (c, s) = quarterTurn q

composeOrientate :: Orientation -> Orientation -> Orientation
composeOrientate
  (Orientation (Point a11 a12 a13) (Point a21 a22 a23) (Point a31 a32 a33))
  (Orientation (Point b11 b12 b13) (Point b21 b22 b23) (Point b31 b32 b33)) =
    Orientation
      (Point
        (a11 * b11 + a12 * b21 + a13 * b23)
        (a11 * b12 + a12 * b22 + a13 * b23)
        (a11 * b13 + a12 * b23 + a13 * b33))
      (Point
        (a21 * b11 + a22 * b21 + a23 * b23)
        (a21 * b12 + a22 * b22 + a23 * b23)
        (a21 * b13 + a22 * b23 + a23 * b33))
      (Point
        (a31 * b11 + a32 * b21 + a33 * b31)
        (a31 * b12 + a32 * b22 + a33 * b32)
        (a31 * b13 + a32 * b23 + a33 * b33))


rotateX :: Int -> Point -> Point
rotateX i (Point x y z) = Point x (c * y - s * z) (s * y + c * z)
  where (c, s) = quarterTurn i

rotateY :: Int -> Point -> Point
rotateY i (Point x y z) = Point (c * x + s * z) y (-s * x + c * z)
  where (c, s) = quarterTurn i

rotateZ :: Int -> Point -> Point
rotateZ i (Point x y z) = Point (c * x - s * y) (s * x + c * y) z
  where (c, s) = quarterTurn i

rotatePoint :: Point -> Point -> Point
rotatePoint (Point rx ry rz) = rotateX rx . rotateY ry . rotateZ rz

possibleTransforms =
  [zeroOrientation
  ,orientRotateAroundX 1
  ,orientRotateAroundX 2
  ,orientRotateAroundX 3
  ,orientRotateAroundZ 1
  ,composeOrientate (orientRotateAroundY 1) (orientRotateAroundZ 1)
  ,composeOrientate (orientRotateAroundY 2) (orientRotateAroundZ 1)
  ,composeOrientate (orientRotateAroundY 3) (orientRotateAroundZ 1)
  ,orientRotateAroundZ 2
  ,composeOrientate (orientRotateAroundX 1) (orientRotateAroundZ 2)
  ,composeOrientate (orientRotateAroundX 2) (orientRotateAroundZ 2)
  ,composeOrientate (orientRotateAroundX 3) (orientRotateAroundZ 2)
  ,orientRotateAroundZ 3
  ,composeOrientate (orientRotateAroundY 1) (orientRotateAroundZ 3)
  ,composeOrientate (orientRotateAroundY 2) (orientRotateAroundZ 3)
  ,composeOrientate (orientRotateAroundY 3) (orientRotateAroundZ 3)
  ,orientRotateAroundY 1
  ,composeOrientate (orientRotateAroundX 1) (orientRotateAroundY 1)
  ,composeOrientate (orientRotateAroundX 2) (orientRotateAroundY 1)
  ,composeOrientate (orientRotateAroundX 3) (orientRotateAroundY 1)
  ,orientRotateAroundY 3
  ,composeOrientate (orientRotateAroundX 1) (orientRotateAroundY 3)
  ,composeOrientate (orientRotateAroundX 2) (orientRotateAroundY 3)
  ,composeOrientate (orientRotateAroundX 3) (orientRotateAroundY 3)
  ]

transformPoint :: Orientation -> Point -> Point
transformPoint (Orientation (Point r11 r12 r13) (Point r21 r22 r23) (Point r31 r32 r33)) (Point x y z) =
  Point (r11 * x + r12 * y + r13 * z) (r21 * x + r22 * y + r23 * z) (r31 * x + r32 * y + r33 * z)

neg :: Point -> Point
neg (Point x y z) = Point (-x) (-y) (-z)

inverseOrientation :: Orientation -> Orientation
inverseOrientation orient = inverse
  where Orientation (Point r11 r12 r13) (Point r21 r22 r23) (Point r31 r32 r33) = orient
        cofactors = Orientation
          (Point (r22 * r33 - r23 * r32) (r23 * r31 - r21 * r33) (r21 * r32 - r22 * r31))
          (Point (r13 * r32 - r12 * r33) (r11 * r33 - r13 * r31) (r12 * r31 - r11 * r32))
          (Point (r12 * r23 - r13 * r22) (r13 * r21 - r11 * r23) (r11 * r22 - r12 * r21))
        Orientation (Point c11 c12 c13) (Point c21 c22 c23) (Point c31 c32 c33) = cofactors
        adjoint = Orientation
          (Point c11 c21 c31)
          (Point c12 c22 c32)
          (Point c13 c23 c33)
        Orientation (Point a11 a12 a13) (Point a21 a22 a23) (Point a31 a32 a33) = adjoint
        determinant = r11 * c11 + r12 * c12 + r13 * c13
        inverse = Orientation
          (Point (a11 `div` determinant) (a12 `div` determinant) (a13 `div` determinant))
          (Point (a21 `div` determinant) (a22 `div` determinant) (a23 `div` determinant))
          (Point (a31 `div` determinant) (a32 `div` determinant) (a33 `div` determinant))

untransformPoint :: Orientation -> Point -> Point
untransformPoint orientation
  | orientation == zeroOrientation = transformPoint zeroOrientation
  | otherwise = transformPoint (inverseOrientation orientation)

getScannerPositionWithTransform :: [Point] -> [Point] -> Orientation -> Maybe (Point, Orientation)
getScannerPositionWithTransform a b orient
  | maximum dx == minimum dx && maximum dy == minimum dy && maximum dz == minimum dz =
    Just (Point (head dx) (head dy) (head dz), orient)
  | otherwise = -- traceShow ("tried-> dx="++show dx++"; dy="++show dy++"; dz="++show dz)
                  Nothing
  where diff = zipWith (\ (Point x1 y1 z1) (Point x2 y2 z2) -> Point (x1-x2) (y1-y2) (z1-z2))
          a (map (transformPoint orient) b)
        dx = map x diff
        dy = map y diff
        dz = map z diff

-- getScannerPosition :: [Point] -> [Point] -> (Point, Orientation)
-- getScannerPosition a b
--   | isInvariantZ = (Point dx dy (head dz), tInvariantXY)
--   | otherwise = error "could not find transform invariant for Z coordinates"
--   where getInvariantXTransform :: Int -> Int -> (Int, Orientation)
--         getInvariantXTransform y z
--           | isInvariantX = (head dx, t)
--           | y == 3 && z == 3 = error "could not find transform invariant for X coordinates"
--           | y < 3 = getInvariantXTransform (y+1) z
--           | otherwise = getInvariantXTransform 0 (z+1)
--           where t = composeOrientate (orientRotateAroundY y) (orientRotateAroundZ z)
--                 dx = zipWith (-) (map x a) (map (x . transformPoint t) b)
--                 isInvariantX = maximum dx == minimum dx
-- 
--         getInvariantXYTransform :: Orientation -> Int -> (Int, Orientation)
--         getInvariantXYTransform tInvariantX x
--           | isInvariantY = (head dy, t)
--           | x == 3 = error "could not find transform invariant for Y coordinates"
--           | otherwise = getInvariantXYTransform tInvariantX (x+1)
--           where t = composeOrientate (orientRotateAroundX x) tInvariantX
--                 dy = zipWith (-) (map y a) (map (y . transformPoint t) b)
--                 isInvariantY = maximum dy == minimum dy
-- 
--         (dx, tInvariantX) = getInvariantXTransform 0 0
--         (dy, tInvariantXY) = getInvariantXYTransform tInvariantX 0
-- 
--         dz = zipWith (-) (map z a) (map (z . transformPoint tInvariantXY) b)
--         isInvariantZ = maximum dz == minimum dz
        

-- Get position, orientation of scanner B relative to scanner A, based on their overlapping beacons
getScannerPosition :: [Point] -> [Point] -> (Point, Orientation)
getScannerPosition pointsA pointsB = reduceScannerPosition False possibleTransforms
  where reduceScannerPosition True [] = error "points do not overlap in any orientation"
        reduceScannerPosition False [] = reduceScannerPosition True possibleTransforms
        reduceScannerPosition reverse (t:transforms) =
          case getScannerPositionWithTransform a b t of
            Nothing -> reduceScannerPosition reverse transforms
            Just (point, orient) -> if reverse
                                       then (untransformPoint orient (neg point), inverseOrientation orient)
                                       else (point, orient)
          where (a, b) = if reverse then (pointsB, pointsA) else (pointsA, pointsB)

normalisePoints :: Point -> Orientation -> Point -> Point
normalisePoints (Point px py pz) orient point = Point x y z
  where (Point xr yr zr) = transformPoint orient point
        (Point x y z) = Point (xr + px) (yr + py) (zr + pz)

normaliseScannerPosition :: Scanner -> [Point] -> [Point] -> Scanner
normaliseScannerPosition scanner theirPoints ourPoints = nextScanner
  where (pos, orient) = getScannerPosition theirPoints ourPoints
        nextScanner = Scanner (index scanner) (Just pos) (Just orient) (beacons scanner)

normaliseScannerBeacons :: Scanner -> Scanner
normaliseScannerBeacons scanner =
  case position scanner of
    Nothing -> scanner
    Just pos -> case orientation scanner of
                  Nothing -> scanner
                  Just orient -> Scanner (index scanner) (Just pos) (Just orient) normalisedBeacons
                    where normalisedBeacons = map (normalisePoints pos orient) (beacons scanner)

normaliseScanner :: Scanner -> [Point] -> [Point] -> Scanner
normaliseScanner scanner theirPoints ourPoints =
  normaliseScannerBeacons (normaliseScannerPosition scanner theirPoints ourPoints)

deduceScannerPositions :: [Scanner] -> [Scanner]
deduceScannerPositions scanners = deduceScannerPositionsLoop initialQueue initialNormalised initialNormalised
  where initialQueue = filter (\ c -> index c /= 0) scanners
        initialNormalised = filter (\ c -> index c == 0) scanners

        deduceScannerPositionsLoop :: [Scanner] -> [Scanner] -> [Scanner] -> [Scanner]
        deduceScannerPositionsLoop [] normalisedScanners _ = normalisedScanners
        deduceScannerPositionsLoop queue normalisedScanners fromScanners
          | null queue = normalisedScanners
          | null withOverlappingBeacons && null nextFromOptions = error "could not find a valid scanner to compare from"
          | null withOverlappingBeacons = deduceScannerPositionsLoop queue normalisedScanners (head nextFromOptions : fromScanners)
          | otherwise = deduceScannerPositionsLoop nextQueue nextNormalisedScanners fromScanners
          where compareFrom = head fromScanners
                withOverlappingBeacons = (
                  filter (\ (_, overlappingBeacons) -> case overlappingBeacons of
                              Nothing -> False
                              Just _ -> True) .
                  map (\ q -> (q, getOverlappingBeacons compareFrom q)))
                  queue
                nextFromOptions = filter (`notElem` fromScanners) normalisedScanners
                (q, (a, b)) = case head withOverlappingBeacons of
                                (q', Just (a', b')) -> (q', (a', b'))
                                (_, Nothing) -> error "filter didn't work properly?"
                nextQueue = filter (\ s -> index s /= index q) queue
                nextNormalisedScanners = traceShow ("normalised scanner "++show (index q))
                  (normalisedScanners ++ [normaliseScanner q a b])

filterUnique :: [Point] -> [Point]
filterUnique [] = []
filterUnique (p:points)
  | null points = [p]
  | p `elem` points = filterUnique points
  | otherwise = p : filterUnique points

getUniqueBeacons :: [Scanner] -> [Point]
getUniqueBeacons scanners = filterUnique (concatMap beacons scanners)

printRows :: Show a => [a] -> IO ()
printRows [] = do return ()
printRows (r:rs) = do
  print r
  printRows rs

sortByX :: [Point] -> [Point]
sortByX = sortBy (\ (Point x1 _ _) (Point x2 _ _) -> compare x1 x2)

sortByY :: [Point] -> [Point]
sortByY = sortBy (\ (Point _ y1 _) (Point _ y2 _) -> compare y1 y2)

sortByZ :: [Point] -> [Point]
sortByZ = sortBy (\ (Point _ _ z1) (Point _ _ z2) -> compare z1 z2)

mapDist :: [Point] -> [Int]
mapDist (p:ps)
  | null ps = []
  | otherwise = dist p (head ps) : mapDist ps

task1 :: [Scanner] -> IO ()
task1 scanners = do
  let result = (length . getUniqueBeacons . deduceScannerPositions) scanners
  printf "Task 1: numBeacons=%d\n" result

-- findMatchingOrient :: [Point] -> [Point] -> Int -> Int -> Int -> (Point, Orientation)
-- findMatchingOrient a b rx ry rz = fromMaybe nextLoop result
--   where t = composeOrientate (orientRotateAroundX rx) (composeOrientate (orientRotateAroundY ry) (orientRotateAroundZ rz))
--         result = traceShow (show (Point rx ry rz)) (getScannerPositionWithTransform a b t)
--         nextLoop
--           | rx == 3 && ry == 3 && rz == 3 = error "points do not overlap in any orientation"
--           | rz < 3 = findMatchingOrient a b rx ry (rz+1)
--           | ry < 3 = findMatchingOrient a b rx (ry+1) (-3)
--           | otherwise = findMatchingOrient a b (rx+1) (-3) (-3)


main = do
  content <- readFile inputFile
  let scanners = getInitialScanners (lines content)

  -- let deduced = deduceScannerPositions scanners
  -- print deduced

  -- let queue = filter (\ c -> index c /= 0) scanners
  -- let normalisedScanners = filter (\ c -> index c == 0) scanners
  -- let fromScanners = normalisedScanners
  -- printf "ql=%d, nl=%d, fl=%d\n" (length queue) (length normalisedScanners) (length fromScanners)

  -- let compareFrom = head fromScanners
  -- let withOverlappingBeacons = (
  --       filter (\ (_, overlappingBeacons) -> case overlappingBeacons of
  --                   Nothing -> False
  --                   Just _ -> True) .
  --       map (\ q -> (q, getOverlappingBeacons compareFrom q)))
  --       queue
  -- -- let nextFromOptions = filter (`notElem` fromScanners) queue
  -- let (q, (a, b)) = case head withOverlappingBeacons of
  --                     (q', Just (a', b')) -> (q', (a', b'))
  --                     (_, Nothing) -> error "filter didn't work properly?"
  -- let nextQueue = filter (\ s -> index s /= index q) queue
  -- let queue = nextQueue
  -- let nextNormalisedScanners = normalisedScanners ++ [normaliseScanner q a b]
  -- let normalisedScanners = nextNormalisedScanners

  -- printf "ql=%d, nl=%d, fl=%d\n" (length queue) (length normalisedScanners) (length fromScanners)

  -- let compareFrom = head fromScanners
  -- let withOverlappingBeacons = (
  --       filter (\ (_, overlappingBeacons) -> case overlappingBeacons of
  --                   Nothing -> False
  --                   Just _ -> True) .
  --       map (\ q -> (q, getOverlappingBeacons compareFrom q)))
  --       queue
  -- -- let nextFromOptions = filter (`notElem` fromScanners) queue
  -- let (q, (a, b)) = case head withOverlappingBeacons of
  --                     (q', Just (a', b')) -> (q', (a', b'))
  --                     (_, Nothing) -> error "filter didn't work properly?"
  -- let nextQueue = filter (\ s -> index s /= index q) queue
  -- let queue = nextQueue
  -- let nextNormalisedScanners = normalisedScanners ++ [normaliseScanner q a b]
  -- let normalisedScanners = nextNormalisedScanners

  -- printf "ql=%d, nl=%d, fl=%d\n" (length queue) (length normalisedScanners) (length fromScanners)

  -- let compareFrom = head fromScanners
  -- let withOverlappingBeacons = (
  --       filter (\ (_, overlappingBeacons) -> case overlappingBeacons of
  --                   Nothing -> False
  --                   Just _ -> True) .
  --       map (\ q -> (q, getOverlappingBeacons compareFrom q)))
  --       queue
  -- let nextFromOptions = filter (`notElem` fromScanners) normalisedScanners
  -- let fromScanners = nextFromOptions
  -- -- let nextFromOptions = filter (`notElem` fromScanners) queue
  -- -- let (q, (a, b)) = case head withOverlappingBeacons of
  -- --                     (q', Just (a', b')) -> (q', (a', b'))
  -- --                     (_, Nothing) -> error "filter didn't work properly?"
  -- -- let nextQueue = filter (\ s -> index s /= index q) queue
  -- -- let queue = nextQueue
  -- -- let nextNormalisedScanners = normalisedScanners ++ [normaliseScanner q a b]
  -- -- let normalisedScanners = nextNormalisedScanners

  -- printf "ql=%d, nl=%d, fl=%d\n" (length queue) (length normalisedScanners) (length fromScanners)

  -- let compareFrom = head fromScanners
  -- let withOverlappingBeacons = (
  --       filter (\ (_, overlappingBeacons) -> case overlappingBeacons of
  --                   Nothing -> False
  --                   Just _ -> True) .
  --       map (\ q -> (q, getOverlappingBeacons compareFrom q)))
  --       queue
  -- -- let nextFromOptions = filter (`notElem` fromScanners) queue
  -- -- let fromScanners = nextFromOptions
  -- let nextFromOptions = filter (`notElem` fromScanners) queue
  -- let (q, (a, b)) = case head withOverlappingBeacons of
  --                     (q', Just (a', b')) -> (q', (a', b'))
  --                     (_, Nothing) -> error "filter didn't work properly?"
  -- let nextQueue = filter (\ s -> index s /= index q) queue
  -- let queue = nextQueue
  -- let nextNormalisedScanners = normalisedScanners ++ [normaliseScanner q a b]
  -- let normalisedScanners = nextNormalisedScanners

  -- printf "ql=%d, nl=%d, fl=%d\n" (length queue) (length normalisedScanners) (length fromScanners)
  -- -- print nextNormalisedScanners

  -- print (head initialQueue)
  -- print initialNormalised

  task1 scanners

  -- let (a,b) = case getOverlappingBeacons (head scanners) (scanners !! 27) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner27 = normaliseScanner (scanners !! 27) a b

  -- let (a,b) = case getOverlappingBeacons (head scanners) (scanners !! 36) of
  --               Nothing -> error "wtf"
  --               Just r -> r

  -- let t = composeOrientate (orientRotateAroundY 1) (orientRotateAroundZ 0)
  -- let dx = zipWith (-) (map x a) (map (x . transformPoint t) b)
  -- let isInvariantX = maximum dx == minimum dx
  -- printf "isInvariantX=%s\n" (show isInvariantX)
  -- print dx

  -- let tInvariantX = t

  -- let t = composeOrientate (orientRotateAroundX (0)) tInvariantX
  -- let dy = zipWith (-) (map y a) (map (y . transformPoint t) b)
  -- let isInvariantY = maximum dy == minimum dy
  -- printf "isInvariantY=%s\n" (show isInvariantY)
  -- print dy

  -- let scanner36 = normaliseScanner (scanners !! 36) a b

  -- let (a,b) = case getOverlappingBeacons scanner27 (scanners !! 19) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner19 = normaliseScanner (scanners !! 19) a b

  -- let (a,b) = case getOverlappingBeacons scanner27 (scanners !! 21) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner21 = normaliseScanner (scanners !! 21) a b

  -- let (a,b) = case getOverlappingBeacons scanner27 (scanners !! 28) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner28 = normaliseScanner (scanners !! 28) a b

  -- let (a,b) = case getOverlappingBeacons scanner27 (scanners !! 35) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner35 = normaliseScanner (scanners !! 35) a b

  -- let (a,b) = case getOverlappingBeacons scanner36 (scanners !! 32) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner32 = normaliseScanner (scanners !! 32) a b

  -- let (a,b) = case getOverlappingBeacons scanner19 (scanners !! 31) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner31 = normaliseScanner (scanners !! 31) a b

  -- let (a,b) = case getOverlappingBeacons scanner19 (scanners !! 34) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner34 = normaliseScanner (scanners !! 34) a b

  -- let (a,b) = case getOverlappingBeacons scanner21 (scanners !! 13) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner13 = normaliseScanner (scanners !! 13) a b

  -- let (a,b) = case getOverlappingBeacons scanner21 (scanners !! 33) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner33 = normaliseScanner (scanners !! 33) a b

  -- let (a,b) = case getOverlappingBeacons scanner28 (scanners !! 39) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner39 = normaliseScanner (scanners !! 39) a b

  -- let (a,b) = case getOverlappingBeacons scanner35 (scanners !! 11) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner11 = normaliseScanner (scanners !! 11) a b

  -- let (a,b) = case getOverlappingBeacons scanner35 (scanners !! 18) of
  --               Nothing -> error "wtf"
  --               Just r -> r
  -- let scanner18 = normaliseScanner (scanners !! 18) a b

  -- let idx = (head . filter (\ i ->
  --             case getOverlappingBeacons (scanners !! 35) (scanners !! i) of
  --               Nothing -> False
  --               Just _ -> True)) ([1..10] ++ [12] ++ [14..17] ++ [20] ++ [22..26] ++ [29..30] ++ [37..38])
  -- print idx

  -- print (zip a b)

  -- -- (-3,-3,-3); then rotate around X or Z axis
  -- print "finding invariant X transform"
  -- let tInvariantX = composeOrientate (orientRotateAroundY 1) (orientRotateAroundZ 0)
  -- let diff = zipWith (\ (Point x1 y1 z1) (Point x2 y2 z2) -> Point (x1-x2) (y1-y2) (z1-z2))
  --             a (map (transformPoint tInvariantX) b)
  -- let dx = map x diff
  -- let dy = map y diff
  -- let dz = map z diff
  -- printf "dx=%s\n" (show dx)
  -- printf "dy=%s\n" (show dy)
  -- printf "dz=%s\n" (show dz)

  -- print "finding invariant XY transform"
  -- let tInvariantXY = composeOrientate (orientRotateAroundX 2) tInvariantX
  -- let diff = zipWith (\ (Point x1 y1 z1) (Point x2 y2 z2) -> Point (x1-x2) (y1-y2) (z1-z2))
  --             a (map (transformPoint tInvariantXY) b)
  -- let dx = map x diff
  -- let dy = map y diff
  -- let dz = map z diff
  -- printf "dx=%s\n" (show dx)
  -- printf "dy=%s\n" (show dy)
  -- printf "dz=%s\n" (show dz)

  -- print "finding invariant XZ transform"
  -- let tInvariantXZ = composeOrientate (orientRotateAroundX (-3)) tInvariantX
  -- let diff = zipWith (\ (Point x1 y1 z1) (Point x2 y2 z2) -> Point (x1-x2) (y1-y2) (z1-z2))
  --             a (map (transformPoint tInvariantXZ) b)
  -- let dx = map x diff
  -- let dy = map y diff
  -- let dz = map z diff
  -- printf "dx=%s\n" (show dx)
  -- printf "dy=%s\n" (show dy)
  -- printf "dz=%s\n" (show dz)

  -- print "finding invariant XYZ transform"
  -- let tInvariantXYZ = composeOrientate (orientRotateAroundZ 0) tInvariantXZ
  -- let diff = zipWith (\ (Point x1 y1 z1) (Point x2 y2 z2) -> Point (x1-x2) (y1-y2) (z1-z2))
  --             a (map (transformPoint tInvariantXYZ) b)
  -- let dx = map x diff
  -- let dy = map y diff
  -- let dz = map z diff
  -- printf "dx=%s\n" (show dx)
  -- printf "dy=%s\n" (show dy)
  -- printf "dz=%s\n" (show dz)

  -- print tInvariantXZ
  -- print (orientRotateAroundZ 0)
  -- print (composeOrientate (orientRotateAroundZ 0) tInvariantXZ)
  -- print tInvariantXYZ

  -- let result = findMatchingOrient a b (-3) (-3) (-3)
  -- print result

  -- let t = composeOrientate (orientRotateAroundX 1) (composeOrientate (orientRotateAroundY 0) (orientRotateAroundZ 0))

  -- let diff = zipWith (\ (Point x1 y1 z1) (Point x2 y2 z2) -> Point (x1-x2) (y1-y2) (z1-z2))
  --             a (map (transformPoint t) b)

  -- let dx = map x diff
  -- let dy = map y diff
  -- let dz = map z diff

  -- printf "%s, %s, %s\n" (show dx) (show dy) (show dz)

  -- print (getOverlappingBeacons (head scanners) (scanners !! idx))
