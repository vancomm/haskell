{-# OPTIONS_GHC -Wall #-}

import Data.Bool (bool)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = bool [] <$> ((:) <$> (`mod` 10) <*> (toDigitsRev . (`div` 10))) <*> (> 0)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (&&) <$> ((==) 0 . (`mod` 10) <$> sumDigits . doubleEveryOther . toDigits) <*> (> 0)
