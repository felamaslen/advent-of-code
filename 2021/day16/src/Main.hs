import Text.Printf

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

type Packet = (Int, Maybe Int)

parseLiteralPacketLoop :: Int -> String -> String -> [Packet]
parseLiteralPacketLoop version binary reduction
  | head next == '0' = (version, Just (binToDec nextReduction)) : parseBinaryPackets rest
  | otherwise = parseLiteralPacketLoop version rest nextReduction 
  where next = take 5 binary
        nextReduction = reduction ++ drop 1 next
        rest = drop 5 binary

parseLiteralPacket :: Int -> String -> [Packet]
parseLiteralPacket _ [] = []
parseLiteralPacket version binary = parseLiteralPacketLoop version binary ""

parseOperatorPacketTotalLength :: Int -> String -> [Packet]
parseOperatorPacketTotalLength version binary = operatorPacket ++ restPackets
  where totalLength = binToDec (take 15 binary)
        subPackets = parseBinaryPackets (take totalLength (drop 15 binary))
        operatorPacket = (version, Nothing) : subPackets
        rest = drop (15 + totalLength) binary
        restPackets = parseBinaryPackets rest

parseOperatorPacketNumPackets :: Int -> String -> [Packet]
parseOperatorPacketNumPackets version binary = operatorPacket ++ restPackets
  where numSubPackets = binToDec (take 11 binary)
        nextPackets = parseBinaryPackets (drop 11 binary)
        subPackets = take numSubPackets nextPackets
        operatorPacket = (version, Nothing) : subPackets
        restPackets = drop numSubPackets nextPackets

parseOperatorPacket :: Int -> String -> [Packet]
parseOperatorPacket _ [] = []
parseOperatorPacket version binary
  | lengthTypeId == '0' = parseOperatorPacketTotalLength version rest
  | lengthTypeId == '1' = parseOperatorPacketNumPackets version rest
  where lengthTypeId = head binary
        rest = tail binary

parseBinaryPackets :: String -> [Packet]
parseBinaryPackets [] = []
parseBinaryPackets binary
  | typeId == 4 = parseLiteralPacket version rest
  | otherwise = parseOperatorPacket version rest
  where version = binToDec (take 3 binary)
        typeId = binToDec (take 3 (drop 3 binary))
        rest = drop 6 binary

parseTransmissionSumVersions :: String -> Int
parseTransmissionSumVersions transmission = sum (map fst packets)
  where binary = convertToBinary transmission
        packets = parseBinaryPackets binary

task1 :: String -> IO ()
task1 transmission = do
  let versionSum = parseTransmissionSumVersions transmission
  printf "Task 1: versionSum=%d\n" versionSum

main = do
  content <- readFile inputFile
  let transmission = head (lines content)

  task1 transmission

