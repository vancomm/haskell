{-# OPTIONS_GHC -Wall #-}

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a