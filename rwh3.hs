import Data.List (sortBy)
import Data.Ord (comparing)

length' :: [a] -> Integer
length' xs = sum [1 | _ <- xs]

mean' :: [Integer] -> Double
mean' xs = fromIntegral (sum xs) / fromIntegral (length' xs)

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs
  | head xs == last xs = (isPalindrome . init . tail) xs
  | otherwise = False

sortListsByLength :: [[a]] -> [[a]]
sortListsByLength = sortBy (comparing length)

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = x
intersperse' s (x : xs) = x ++ [s] ++ intersperse' s xs

data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

data Point = Point Float Float
  deriving (Eq, Show)

data Direction
  = DLeft
  | DRight
  | DStraight
  deriving (Show)

direction :: Point -> Point -> Point -> Direction
direction (Point ax ay) (Point bx by) (Point cx cy)
  | val == 0 = DStraight
  | val < 0 = DRight
  | val > 0 = DLeft
  where
    val = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

directions :: [Point] -> [Direction]
directions (a : b : c : xs) = direction a b c : directions xs
directions _ = []