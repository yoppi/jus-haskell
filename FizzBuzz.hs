-- FizzBuzz not use *devide*
-- 2009-03-20 16:31:23

fizz, buzz, fizzbuzz :: [String]
fizzbuzz = zipWith f [0..] (zipWith (++) fizz buzz)
  where 
    f n "" = show n
    f _ s  = s

fizz = cycle (take 3 ("Fizz":repeat ""))
buzz = cycle (take 5 ("Buzz":repeat ""))

main = print $ take 100 (tail fizzbuzz)
