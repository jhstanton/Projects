{-
  Sort.hs
 
  Implements functions for merge sort,
  bubble sort and quick sort

-}

{-
  Standard merge sort using the heap to store new lists
-}
mergeSort []  = []
mergeSort [x] = [x] 
mergeSort xs  = merge (mergeSort left) (mergeSort right) where
  halfLength = div (length xs) 2
  left =  take halfLength xs
  right = drop halfLength xs
  merge ys []         = ys
  merge [] zs         = zs
  merge (y:ys) (z:zs) = --run through list once to do each divided merge and final merge
    if y < z
    then y : merge ys (z:zs)
    else z : merge (y:ys) zs

{-
  Less than normal bubble sort, swaps elements of a list and recalls the 
  sorting algorithm if not sorted. Same basic premise - run through the list
  and swap elements as spotted out of order. Not very efficient
-}
bubbleSort xs = bubble xs [] xs where
  bubble [] [] [] = []
  bubble [x] new orig      = 
    let newRev = reverse (x:new) in
    if newRev == orig -- return the list if it is sorted
    then newRev
    else bubble newRev [] newRev
  bubble (x:y:xs) new orig =
    if x < y  -- swap x and y if necessary
    then bubble (y:xs) (x:new) orig
    else bubble (x:xs) (y:new) orig

{-
  Again, not quite a standard quickSort since this is not in place. Still uses a 'pivot'
  to sort around. Similar to mergesort in speed and algorithm, but I expect this is slower
  in this case since it has to repeatedly find the element defined at the pivot point. 
-}
quickSort []  = []
quickSort [x] = [x]
quickSort xs  = quickSort less ++ (pivot : quickSort more) where
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
