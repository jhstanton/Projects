{-
  Collatz.hs
-}

-- Precondition: n > 1
collatz 1 = 0
collatz n = if odd n
            then 1 + collatz (3 * n + 1)
            else 1 + collatz (div n 2)

-- Tail recursive version
-- Precondition: n > 1
collTCO 1 acc = acc
collTCO n acc = if odd n
                then collTCO (3 * n + 1) (acc + 1)
                else collTCO (div n 2) (acc + 1)
