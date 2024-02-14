{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = interact wordCount
  where
    wordCount input = show (length (words input)) ++ "\n"