-- Prime Sequence
-- 2009-03-20 16:36:01

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:qs) = p : sieve (filter ((0 /=) . (`mod` p )) qs) 

main = print $  primes !! 1000000
