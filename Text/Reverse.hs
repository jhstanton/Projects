{-
  Reverse.hs

  Reverses a string
-}

myReverse str = aux str [] 
  where
    aux [] acc     = acc
    aux (c:cs) acc = aux cs (c:acc) 
