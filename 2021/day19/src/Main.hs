{-# LANGUAGE LambdaCase #-}
import Data.List.Split
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

manhattanDist :: Point -> Point -> Dist
manhattanDist (Point x1 y1 z1) (Point x2 y2 z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

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
  | otherwise = Nothing
  where diff = zipWith (\ (Point x1 y1 z1) (Point x2 y2 z2) -> Point (x1-x2) (y1-y2) (z1-z2))
          a (map (transformPoint orient) b)
        dx = map x diff
        dy = map y diff
        dz = map z diff

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
                nextNormalisedScanners = normalisedScanners ++ [normaliseScanner q a b]

filterUnique :: [Point] -> [Point]
filterUnique [] = []
filterUnique (p:points)
  | null points = [p]
  | p `elem` points = filterUnique points
  | otherwise = p : filterUnique points

getUniqueBeacons :: [Scanner] -> [Point]
getUniqueBeacons scanners = filterUnique (concatMap beacons scanners)

getLargestManhattanDistance :: [Scanner] -> Int
getLargestManhattanDistance [] = 0
getLargestManhattanDistance (sA:scanners)
  | null scanners = 0
  | otherwise = max (maximum (map (manhattanDist posA) rest)) (getLargestManhattanDistance scanners)
  where posA = case position sA of
                 Just p -> p
                 Nothing -> error "all scanners must be normalised"
        rest = map (\s -> case position s of
                            Just p -> p
                            Nothing -> error "all scanners must be normalised") scanners

task1 :: [Scanner] -> IO ()
task1 normalisedScanners = do
  let result = (length . getUniqueBeacons) normalisedScanners
  printf "Task 1: numBeacons=%d\n" result

task2 :: [Scanner] -> IO ()
task2 normalisedScanners = do
  let result = getLargestManhattanDistance normalisedScanners
  printf "Task 2: manhattanDistance=%d\n" result

main = do
  content <- readFile inputFile
  let normalisedScanners = (deduceScannerPositions . getInitialScanners) (lines content)

  task1 normalisedScanners
  task2 normalisedScanners
