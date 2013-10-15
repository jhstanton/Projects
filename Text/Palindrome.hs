{-
  Palindrome.hs

  Simply checks if a word is a palindrome
-}

-- using builtins
simple word = reverse word == word

withOutReverse word = aux word [] == word
  where
    aux [] acc     = acc
    aux (c:cs) acc = aux cs (c:acc)
