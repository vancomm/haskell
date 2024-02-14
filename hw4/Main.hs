{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.List (foldl')

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = product $ map (subtract 2) $ filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n = sum evens + fun2' nextOdd
  where
    evens = takeWhile even $ iterate (`div` 2) n
    nextOdd = foldl' (\_ x -> x `div` 2) (3 * n + 1) evens

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr build Leaf
  where
    build x Leaf = Node 0 Leaf x Leaf
    build x (Node h Leaf v Leaf) = Node (h + 1) (build x Leaf) v Leaf
    build x (Node h l@(Node {}) v Leaf) = Node h l v (build x Leaf)
    build x (Node h Leaf v r@(Node {})) = Node h (build x Leaf) v r
    build x (Node h l@(Node hl _ _ _) v r@(Node hr _ _ _))
      | hl < hr = Node (nhl + 1) nl v r
      | otherwise = Node (nrl + 1) l v nr
      where
        nl@(Node nhl _ _ _) = build x l
        nr@(Node nrl _ _ _) = build x r

xor :: [Bool] -> Bool
xor xs = foldr (\_ acc -> not acc) False $ filter id xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : [2 * x + 1 | x <- [1 .. n], x `notElem` excluded]
  where
    excluded = [i + j + 2 * i * j | i <- [1 .. n], j <- [1 .. n], i < j]
