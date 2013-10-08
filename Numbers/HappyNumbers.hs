{-
  Find the first 8 happy numbers
  As stated in the project list - 
  "A happy number is defined by the following process. Starting with any 
   positive integer, replace the number by the sum of the squares of its 
   digits, and repeat the process until the number equals 1 (where it will 
   stay), or it loops endlessly in a cycle which does not include 1."
-}

-- used as an upper bound to avoid an infinite loop
upper = 100
numHappy = 8

main =
  do
  print (take numHappy allHappy)

-- makes a list of digits
getDigits n           = aux n []
  where aux x acc 
          | x < 10    =  x:acc 
          | otherwise =  aux (div x 10) ((mod x 10):acc)

-- returns the next cycle, which is hopefully 1
nextCycle n = sum $ map (\x -> x*x) $ getDigits n 

-- actually cycles through until upper or 1 is found
cycleThrough n upper 
  | upper <= 0 = n
  | otherwise  = cycleThrough (nextCycle n) (upper-1)

-- creates a stream of every happy number
-- found within an upper bound of 'upper'
allHappy = filter (\x -> cycleThrough x upper == 1) [2..]
