{-
  Binary and Decimal Converter
-}
module Main (main) where

main = 
  do
  print "Enter 1 to convert binary, 2 for decimal, q to quit"
  choice <- getLine
  if choice == "q"
  then do 
    print "Closing application"
  else do 
    print "Enter number: "
    numberStr <- getLine
    let number = read numberStr::Int in  
      if choice == "2"
      then do
        let digits = getDigits number
        print (toDec $ map fromIntegral digits)
        main
      else do
        print (showBin $ toBin number)
        main
  where
    showBin ns = foldl (++) "" $ map show ns

-- makes a list of each digit
getDigits x           = aux x []
  where aux n acc  
          | n < 10    = n : acc
          | otherwise = aux (div n 10) ((mod n 10):acc)

-- makes a list of binary digits
toBin x               = aux x []
  where aux n acc      
          | n == 0    = if length acc == 0
                        then 0 : acc
                        else acc
          | otherwise = (aux (div n 2) ((mod n 2) : acc))


-- makes a decimal integer from a binary list
toDec xs = 
  sum $ map (\(x, y) -> x * 2** (fromIntegral y)) binAndPow
  where
    binAndPow = zip xs (reverse [0..(length xs - 1)])
