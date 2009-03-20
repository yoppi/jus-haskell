-- SlideShow that console demo application.
-- 2009-03-20 16:54:49

module Main where

import qualified System.IO.UTF8 as U

main :: IO ()
main = U.getContens >>= U.putStr
