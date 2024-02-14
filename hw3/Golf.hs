module Golf where

import Data.Bool (bool)
import Data.ByteString (intercalate)

every :: Int -> [a] -> [a]
every n xs = case drop (n - 1) xs of
  y : ys -> y : every n ys
  [] -> []

skips :: [a] -> [[a]]
skips [] = []
skips xs = (`every` xs) <$> [1 .. (length xs)]

localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs)
  | all (< y) [x, z] = y : localMaxima (y : z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima _ = []

countIn :: Integer -> [Integer] -> Integer
countIn n = sum . map (const 1) . filter (== n)

ha :: String
ha = replicate 10 '=' ++ "\n" ++ concatMap show [0 .. 9] ++ "\n"

hl :: Integer -> [Integer] -> String
hl n = concatMap (bool " " "*" <$> (>= n))

histogram :: [Integer] -> String
histogram xs = unlines (map (`hl` freqs) [maxFreq, maxFreq - 1 .. 1]) ++ ha
  where
    freqs = map (`countIn` xs) [0 .. 9]
    maxFreq = maximum freqs

histogram' :: [Integer] -> String
histogram' xs = unlines ((`hl` c) <$> [m, m - 1 .. 1]) ++ "==========\n0123456789\n"
  where
    c = (`countIn` xs) <$> [0 .. 9]
    m = maximum c