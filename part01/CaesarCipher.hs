module CaesarCipher where

import           Data.Char

-- NOTE: only encode lower-case characters, upper-case characters
-- and punctuations remain unchanged

charToInt :: Char -> Int
charToInt c = ord c - ord 'a'


intToChar :: Int -> Char
intToChar n = chr (ord 'a' + n)


shift :: Int -> Char -> Char
shift n c | isLower c = intToChar (mod (charToInt c + n) 26)
          | otherwise = c


encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]


percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100


chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ (o - e) ^ 2 / e | (o, e) <- zip os es ]


count :: Eq a => a -> [a] -> Int
count x xs = length [ x' | x' <- xs, x' == x ]


lowers :: String -> Int
lowers xs = length [ x | x <- xs, isLower x ]


freqs :: String -> [Float]
freqs xs = [ percent (count x xs) n | x <- ['a' .. 'z'] ] where n = lowers xs


positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [0 ..], x' == x ]


rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs


crack :: String -> String
crack xs = encode (-factor) xs
 where
  factor = head (positions (minimum chitab) chitab)
  chitab = [ chisqr (rotate n table') table | n <- [0 .. 25] ]
  table' = freqs xs


table :: [Float]
table =
  [ 8.1  -- a
  , 1.5  -- b
  , 2.8  -- c
  , 4.2  -- d
  , 12.7 -- e
  , 2.2  -- f
  , 2.0  -- g
  , 6.1  -- h
  , 7.0  -- i
  , 0.2  -- j
  , 0.8  -- k
  , 4.0  -- l
  , 2.4  -- m
  , 6.7  -- n
  , 7.5  -- o
  , 1.9  -- p
  , 0.1  -- q
  , 6.0  -- r
  , 6.3  -- s
  , 9.0  -- t
  , 2.8  -- u
  , 1.0  -- v
  , 2.4  -- w
  , 0.2  -- x
  , 2.0  -- y
  , 0.1  -- z
  ]

