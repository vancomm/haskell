module JoinList where

import Buffer
import Editor
import Scrabble
import Sized (Size (Size), Sized (size), getSize)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ l = l
l +++ Empty = l
l +++ r = Append (tag l <> tag r) l r

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append s l r)
  | i < 0 || i >= listSize = Nothing
  | i < leftSize = indexJ i l
  | otherwise = indexJ (i - leftSize) r
  where
    listSize = getSize . size $ s
    leftSize = getSize . size . tag $ l
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i s@(Single _ _)
  | i <= 0 = s
dropJ i a@(Append s l r)
  | i <= 0 = a
  | i < leftSize = dropJ i l +++ r
  | i < listSize = dropJ (i - leftSize) r
  where
    listSize = (getSize . size) s
    leftSize = (getSize . size . tag) l
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i s@(Single _ _)
  | i <= 0 = Empty
takeJ i a@(Append s l r)
  | i <= 0 = Empty
  | i < leftSize = takeJ i l
  | i < listSize = l +++ takeJ (i - leftSize) r
  where
    listSize = (getSize . size) s
    leftSize = (getSize . size . tag) l
takeJ _ l = l

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr ((+++) . (\x -> Single (scoreString x, 1) x)) Empty . lines
  line = indexJ
  replaceLine i s l = takeJ i l +++ fromString s +++ dropJ (i + 1) l
  numLines = getSize . snd . tag
  value = getScore . fst . tag

main =
  runEditor
    editor
    ( fromString $
        unlines
          [ "This buffer is for notes you don't want to save, and for",
            "evaluation of steam valve coefficients.",
            "To load a different file, type the character L followed",
            "by the name of the file."
          ] ::
        JoinList (Score, Size) String
    )