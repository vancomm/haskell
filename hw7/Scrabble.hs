module Scrabble where

import Data.Char (toLower)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = 1
  | c' `elem` "dg" = 2
  | c' `elem` "bcmp" = 3
  | c' `elem` "fhvwy" = 4
  | c' `elem` "k" = 5
  | c' `elem` "jx" = 8
  | c' `elem` "qz" = 10
  | otherwise = 0
  where
    c' = toLower c

scoreString :: String -> Score
scoreString = mconcat <$> fmap score

getScore :: Score -> Int
getScore (Score i) = i