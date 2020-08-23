module LineCode where


import           Data.Char


type Bit = Int -- single bit 0 or 1


-- convert an integer in binary to decimal
binToDec :: [Bit] -> Int
binToDec = foldr (\b acc -> b + acc * 2) 0


-- convert an integer in decimal to binary
decToBin :: Int -> [Bit]
decToBin 0 = []
decToBin n = mod n 2 : decToBin (div n 2)


-- pad the bit sequence so that it has the length of exactly 8
zpadding :: [Bit] -> [Bit]
zpadding bits = take 8 (bits ++ repeat 0)


segment :: Int -> [Bit] -> [[Bit]]
segment _ [] = []
segment n xs = take n xs : segment n (drop n xs)


segment8 :: [Bit] -> [[Bit]]
segment8 = segment 8


decode :: [Bit] -> String
decode = map (chr . binToDec) . segment8


encode :: String -> [Bit]
encode = concatMap (zpadding . decToBin . ord)


transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
