-- SlideShow that console demo application.
-- 2009-03-20 16:54:49

module Main where

import Data.List
import System.Environment
import qualified System.IO.UTF8 as U

type Line      = String
type Paragraph = [Line]

paragraphs :: [Line] -> [Paragraph]
paragraphs = unfoldr phi
  where phi [] = Nothing
        phi ls = case break hr ls of
                   (xs, _:ys) -> Just (xs, ys) 
                   xsys       -> Just xsys
        hr l = "-----" `isPrefixOf` l

main :: IO ()
main = inputSetup >>= outputParagraph . head . paragraphs . lines

inputSetup :: IO String
inputSetup = getArgs >>= U.readFile . head

outputParagraph :: Paragraph -> IO ()
outputParagraph = U.putStr . unlines
