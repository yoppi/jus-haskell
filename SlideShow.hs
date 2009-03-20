-- SlideShow that console demo application.
-- 2009-03-20 16:54:49

module Main where

import System.Environment
import qualified System.IO.UTF8 as U


main :: IO ()
main = getArgs >>= U.readFile . head >>= U.putStr
