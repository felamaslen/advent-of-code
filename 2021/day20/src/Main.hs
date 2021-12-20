import Debug.Trace
import Text.Printf

inputFile = "./input.txt"

newtype Image = Image [[Char]]

instance Show Image where
  show (Image chars) = unlines chars

type Enhancer = String

getInitialParameters :: [String] -> (Enhancer, Image)
getInitialParameters chars = getInitialParametersLoop chars ("", Image [])
  where getInitialParametersLoop [] result = result
        getInitialParametersLoop (l:rest) (enhancer, Image input)
          | null rest = (enhancer, Image (input ++ [l]))
          | null input && not (null l) = getInitialParametersLoop rest (enhancer ++ l, Image [])
          | null l = getInitialParametersLoop (tail rest) (enhancer, Image (input ++ take 1 rest))
          | otherwise = getInitialParametersLoop rest (enhancer, Image (input ++ [l]))
            where next = head rest

binToDec :: [Int] -> Int
binToDec str = binToDecLoop str (length str - 1)
  where binToDecLoop [] _ = 0
        binToDecLoop (b:bin) i = b * 2 ^ i + binToDecLoop bin (i-1)

charToBin :: Char -> Int
charToBin c
  | c == '.' = 0
  | c == '#' = 1
  | otherwise = error "invalid character"

pixelsToNumber :: [[Char]] -> Int
pixelsToNumber = binToDec . concatMap (map charToBin)

mergeImage :: [[Char]] -> [[Char]] -> Int -> Int -> [[Char]]
mergeImage to from ox oy = take oy to ++
  map (\ y -> take ox (to !! y) ++
    from !! (y - oy) ++
    drop (ox + length (from !! (y - oy))) (to !! y)
  ) [oy..oy + length from - 1] ++
    drop (oy + length from) to

enhanceImage :: Enhancer -> Image -> Bool -> Image
enhanceImage enhancer (Image image) alreadyEnhanced = Image result
  where maxX = length (head image) - 1
        maxY = length image - 1

        enhanceImageRow :: Int -> [Char]
        enhanceImageRow y = map (enhanceImagePixel y) [-2..maxX+2]

        pixelAt x y
          | x < 0 || x > maxX || y < 0 || y > maxY = if alreadyEnhanced then head enhancer else '.'
          | otherwise = (image !! y) !! x

        getPixelsAround :: Int -> Int -> [[Char]]
        getPixelsAround x y = [[nw, nn, ne], [ww, middle, ee], [sw, ss, se]]
          where nw = pixelAt (x-1) (y-1)
                nn = pixelAt x (y-1)
                ne = pixelAt (x+1) (y-1)
                ee = pixelAt (x+1) y
                se = pixelAt (x+1) (y+1)
                ss = pixelAt x (y+1)
                sw = pixelAt (x-1) (y+1)
                ww = pixelAt (x-1) y
                middle = pixelAt x y

        enhanceImagePixel :: Int -> Int -> Char
        enhanceImagePixel y x = enhancedPixel
          where pixelsAround = getPixelsAround x y
                boxNumber = pixelsToNumber pixelsAround
                enhancedPixel = enhancer !! boxNumber

        result = map enhanceImageRow [-2..maxY+2]

enhanceNTimes :: Int -> Enhancer -> Image -> Image
enhanceNTimes n enhancer = enhanceLoop 0
  where enhanceLoop i result
          | i >= n = result
          | otherwise = enhanceLoop (i+1) (enhanceImage enhancer result (i>0))

enhanceTwice = enhanceNTimes 2

countLitPixels :: Image -> Int
countLitPixels (Image image) = countLoop image
  where countLoop [] = 0
        countLoop (r:rows) = length (filter (== '#') r) + countLoop rows

task1 :: Enhancer -> Image -> IO ()
task1 enhancer input = do
  let enhancedTwice = enhanceTwice enhancer input

  let numLitPixels = countLitPixels enhancedTwice
  printf "Task 1: numLitPixels=%d\n" numLitPixels

main = do
  content <- readFile inputFile
  let (enhancer, input) = getInitialParameters (lines content)
  task1 enhancer input
