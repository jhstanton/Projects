{-
  Simple Sieve of Eratosthenes. 

-}

sieve (n:ns) = n : sieve (filter (\x -> x `mod` n /= 0) ns)

primes = sieve [2..]
