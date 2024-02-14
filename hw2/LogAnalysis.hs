{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Text.Read (readMaybe)

parseMessage :: String -> LogMessage
parseMessage msg =
  case words msg of
    ("I" : t : ws) -> case readMaybe t of
      Just time -> LogMessage Info time (unwords ws)
      _ -> Unknown msg
    ("W" : t : ws) -> case readMaybe t of
      Just time -> LogMessage Warning time (unwords ws)
      _ -> Unknown msg
    ("E" : l : t : ws) -> case (readMaybe l, readMaybe t) of
      (Just level, Just time) -> LogMessage (Error level) time (unwords ws)
      _ -> Unknown msg
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ timestamp _) tree@(Node left l@(LogMessage _ t _) right) =
  case compare timestamp t of
    LT -> Node (insert msg left) l right
    GT -> Node left l (insert msg right)
    _ -> tree
insert msg@(LogMessage {}) Leaf = Node Leaf msg Leaf
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right
inOrder Leaf = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = [s | (LogMessage (Error level) _ s) <- inOrder $ build msgs, level > 50]