{-
  Caesar.hs
  
  Implements a basic caesar cypher. 
-}
import Data.List

main = 
  do 
  putStr "Enter any alphabetical string to encrypt: "
  str <- getLine
  putStr "Enter the positive integer key: "
  keyStr <- getLine
  let key = read keyStr::Int
  let strs = words str
  let encryptedWords = map (\x ->encrypt x key) strs
  let decryptedWords = map (\x ->decrypt x key) encryptedWords
  putStr "Encrypted string: "
  putStrLn $ intercalate " " encryptedWords
  putStr "Decrypted back: "
  putStrLn $ intercalate " " decryptedWords
  
-- wraps infinitely
lower = ['a' .. 'z'] ++ lower
upper = ['A' .. 'Z'] ++ upper

shift word amt = 
  map (\x -> if elem x (take 26 lower)
             then head $ drop amt $ dropWhile (/= x) lower
             else head $ drop amt $ dropWhile (/= x) upper) word 

encrypt word key = shift word key 

decrypt word key = 
  if key > 26 
  then decrypt word (key - 26)
  else shift word (26 - key)
  
