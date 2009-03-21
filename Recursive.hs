stepWays :: Integer -> Integer
-- n段の一歩手前は(n-1)段または(n-2)段目
stepWays 0 = 1
stepWays 1 = stepWays 0
stepWays n = stepWays (n-1) + stepWays (n-2)

stepWays2 :: Integer -> (Integer, Integer)
stepWays2 0 = (1, 1)
stepWays2 n = (b, a+b)
  where
    (a, b) = stepWays2 (n-1)

fastStepWays :: Integer -> Integer
fastStepWays = fst . stepWays2

main = print $ fastStepWays 100
