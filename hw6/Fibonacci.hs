{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.List (foldl', foldl1')

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

instance (Show a) => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = Cons z (streamFromSeed f (f z))

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap succ ruler)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0 + b0) (a' + b')
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (streamMap (* a0) b' + a' * b)

instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = q
    where
      q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

data Matrix = Matrix Integer Integer Integer Integer deriving (Show)

instance Num Matrix where
  (*) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) = Matrix a3 b3 c3 d3
    where
      a3 = a1 * a2 + b1 * c2
      b3 = a1 * b2 + b1 * d2
      c3 = c1 * a2 + d1 * c2
      d3 = c1 * b2 + d1 * d2

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = f
  where
    (Matrix _ f _ _) = Matrix 1 1 1 0 ^ n