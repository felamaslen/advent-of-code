import Text.Printf
import Data.Maybe (fromMaybe)

inputFile = "./input.txt"

toInt :: Char -> Int
toInt c = read [c] :: Int

hexToDec :: Char -> Int
hexToDec c
  | c == 'A' = 10
  | c == 'B' = 11
  | c == 'C' = 12
  | c == 'D' = 13
  | c == 'E' = 14
  | c == 'F' = 15
  | otherwise = toInt c

decToBin :: Int -> String
decToBin n
  | n <= 0 = ""
  | otherwise = decToBin ((n - remainder) `div` 2) ++ show remainder
  where remainder = mod n 2

binToDecLoop :: [Int] -> Int -> Int
binToDecLoop [] _ = 0
binToDecLoop (b:bin) i = b * 2 ^ i + binToDecLoop bin (i+1)

binToDec :: String -> Int
binToDec str = binToDecLoop (reverse (map toInt str)) 0

padLeft :: Int -> String -> String
padLeft n str
  | length str < n = padLeft n ("0"++str)
  | otherwise = str

convertHexCharToBinary :: Char -> String
convertHexCharToBinary c = padLeft 4 (decToBin (hexToDec c))

convertToBinary :: String -> String
convertToBinary = concatMap convertHexCharToBinary

data Packet = Packet {
                     version :: Int
                     , typeId :: Int
                     , value :: Maybe Int
                     , numBits :: Int
                     , children :: [Packet]
                     } deriving (Show)

literalTypeId = 4

isOperator :: Packet -> Bool
isOperator packet = typeId packet /= literalTypeId

sumPacketNumBits :: [Packet] -> Int
sumPacketNumBits [] = 0
sumPacketNumBits (p:packets) = sumPacketNumBits packets + numBits p + sumPacketNumBits (children p)

sumPacketVersions :: [Packet] -> Int
sumPacketVersions [] = 0
sumPacketVersions (p:packets) = version p + sumPacketVersions (children p) + sumPacketVersions packets

parseLiteralPacketLoop :: String -> Int -> String -> Int -> Packet
parseLiteralPacketLoop reduction i binary version
  | ended = Packet
      version
      literalTypeId
      (Just (binToDec next))
      (6 + 5 * i)
      []
  | otherwise = parseLiteralPacketLoop next (i+1) (drop 5 binary) version
  where ended = head binary == '0'
        next = reduction ++ take 4 (drop 1 binary)

parseLiteralPacket = parseLiteralPacketLoop "" 1

parsePacketsLoopFromLiteral :: String -> Int -> Int -> [Packet] -> [Packet]
parsePacketsLoopFromLiteral binary limit version stack =
  parsePacketsLoop remaining limit (stack ++ [packet])
  where packet = parseLiteralPacket binary version
        remaining = drop (numBits packet - 6) binary

parsePacketsLoopFromOperatorNumBits :: String -> Int -> Int -> Int -> [Packet] -> [Packet]
parsePacketsLoopFromOperatorNumBits binary limit version typeId stack =
  parsePacketsLoop remaining limit (stack ++ [packet])
  where nBits = binToDec (take 15 binary)
        slice = take nBits (drop 15 binary)
        remaining = drop (nBits + 15) binary
        children = parsePackets slice
        packet = Packet version typeId Nothing (7+15) children

parsePacketsLoopFromOperatorNumSubPackets :: String -> Int -> Int -> Int -> [Packet] -> [Packet]
parsePacketsLoopFromOperatorNumSubPackets binary limit version typeId stack =
  parsePacketsLoop (drop (nBits - 7) binary) limit (stack ++ [packet])
    where numChildren = binToDec (take 11 binary)
          children = parsePacketsLoop (drop 11 binary) numChildren []
          packet = Packet version typeId Nothing (7+11) children
          nBits = numBits packet + sumPacketNumBits children

parsePacketsLoopFromOperator :: String -> Int -> Int -> Int -> [Packet] -> [Packet]
parsePacketsLoopFromOperator binary limit version typeId stack
  | null binary = []
  | lengthTypeId == 0 = parsePacketsLoopFromOperatorNumBits (tail binary) limit version typeId stack
  | lengthTypeId == 1 = parsePacketsLoopFromOperatorNumSubPackets (tail binary) limit version typeId stack
  | otherwise = error "Invalid length type ID"
  where lengthTypeId = binToDec (take 1 binary)

parsePacketsLoop :: String -> Int -> [Packet] -> [Packet]
parsePacketsLoop binary limit stack
  | null remaining || (binToDec binary == 0) || (limit > 0 && length stack >= limit) = stack
  | typeId == 4 = parsePacketsLoopFromLiteral remaining limit version stack
  | otherwise = parsePacketsLoopFromOperator remaining limit version typeId stack
  where version = binToDec (take 3 binary)
        typeId = binToDec (take 3 (drop 3 binary))
        remaining = drop 6 binary

parsePackets :: String -> [Packet]
parsePackets binary = parsePacketsLoop binary 0 []

applyOperator :: Int -> [Int] -> Int
applyOperator op stack
  | op == 0 = sum stack
  | op == 1 = product stack
  | op == 2 = minimum stack
  | op == 3 = maximum stack
  | (op == 5 || op == 6 || op == 7) && length stack /= 2 = error ("gt/lt/eq op with stack of length "++show (length stack))
  | op == 5 = if head stack > last stack then 1 else 0
  | op == 6 = if head stack < last stack then 1 else 0
  | op == 7 = if head stack == last stack then 1 else 0

getOpName :: Int -> String
getOpName op
  | op == 0 = "sum"
  | op == 1 = "prod"
  | op == 2 = "min"
  | op == 3 = "max"
  | op == 5 = "gt"
  | op == 6 = "lt"
  | op == 7 = "eq"

reducePacket :: Packet -> Int
reducePacket packet
  | isOperator packet = applyOperator (typeId packet) (map reducePacket (children packet))
  | otherwise = fromMaybe 0 (value packet)

pad :: Int -> String
pad 0 = ""
pad n = ' ' : pad (n-1)

printPackets :: [Packet] -> Int -> IO ()
printPackets [] _ = do return ()
printPackets (p:packets) n = do
  let padding = pad n
  printf "%stypeId=%d, version=%d, v=%s\n" padding (typeId p) (version p) (show (value p))
  printPackets (children p) (n+1)
  printPackets packets n

task1 :: String -> IO ()
task1 transmission = do
  let versionSum = sumPacketVersions (parsePackets (convertToBinary transmission))
  printf "Task 1: versionSum=%d\n" versionSum

task2 :: String -> IO ()
task2 transmission = do
  let opReduction = reducePacket (head (parsePackets (convertToBinary transmission)))
  printf "Task 2: opReduction=%d\n" opReduction

main = do
  content <- readFile inputFile
  let transmission = head (lines content)

  task1 transmission
  task2 transmission
