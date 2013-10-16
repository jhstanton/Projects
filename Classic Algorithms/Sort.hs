{-
  Sort.hs
 
  Implements functions for merge sort,
  bubble sort and quick sort

-}

mergeSort []  = []
mergeSort [x] = [x] 
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where
    halfLength = div (length xs) 2
    left =  take halfLength xs
    right = drop halfLength xs
    merge ys []         = ys
    merge [] zs         = zs
    merge (y:ys) (z:zs) = 
      if y < z
      then y : merge ys (z:zs)
      else z : merge (y:ys) zs

bubbleSort xs = bubble xs [] xs
  where
    bubble [] [] [] = []
    bubble [x] new orig      = 
      let newRev = reverse (x:new) in
      if newRev == orig
      then newRev
      else bubble newRev [] newRev
    bubble (x:y:xs) new orig =
      if x < y
      then bubble (y:xs) (x:new) orig
      else bubble (x:xs) (y:new) orig


quickSort []  = []
quickSort [x] = [x]
quickSort xs  = quickSort less ++ (pivot : quickSort more)
  where
    pivotPoint = div (length xs) 2
    pivot = xs !! pivotPoint
    -- remove the pivot element from xs
    (front, back) = splitAt pivotPoint xs
    xsNoPivot = if null back
                then front
                else front ++ (tail back)
    (less, more) = partition xsNoPivot [] []
    
    partition [] less more     = (less, more)
    partition (y:ys) less more =
      if y <= pivot 
      then partition ys (y:less) more
      else partition ys less (y:more)   

-- test cases
xs = [200, 1, 20, 3, 5, -1, -2, -2, -200, 5, 7, 9, 30, 9]
ys = [ -1, 5, 10, 2, 3, 20, 3, -1]
