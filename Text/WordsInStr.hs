{-
  WordsInStr.hs

  Counts the number of words in a string
-}

  

-- uses builtins
simple str = length $ words str

-- less straight forward, doesn't use the words builtin
withOutWords []  = 0
withOutWords str = length $ aux str [] []
  where 
    aux [] word acc      = word : acc
    aux (c:cs) word acc  = if c == ' '
                           then aux cs [] (word:acc)
                           else aux cs (c:word) acc
