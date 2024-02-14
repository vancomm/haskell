import Data.Char (toUpper)

-- main = do
--   putStrLn "hi!!! whats your name?"
--   name <- getLine
--   let coolName = toUpper <$> name
--   putStrLn $ "hey " ++ coolName ++ " youre cool!"

-- main =
--   putStrLn "hi!!! whats your name?"
--     >>= const getLine
--     >>= (\x -> (<$>) toUpper <$> getLine)
--     >>= (\x -> putStrLn $ "hey " ++ x ++ " youre cool!")

main =
  interact $
    unlines
      . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome")
      . lines
  where
    isPalindrome = (==) <*> reverse

isPalindrome1 :: String -> Bool
isPalindrome1 x = x == reverse x

isPalindrome2 :: String -> Bool
isPalindrome2 = (==) <*> reverse

isPalindrome3 :: String -> Bool
isPalindrome3 = reverse >>= (==)
