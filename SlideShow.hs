-- SlideShow that console demo application.
-- 2009-03-20 16:54:49

module Main where

import Data.List
import System.Cmd
import System.Environment
import System.IO
import qualified System.IO.UTF8 as U
import System.Timeout

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
main = inputSetup 
     >>= mapM_ displayParagraph . paragraphs . lines

inputSetup :: IO String
inputSetup = getArgs >>= U.readFile . head

displayParagraph :: Paragraph -> IO ()
displayParagraph = (pause >>) . (clear >>) . outputParagraph

outputParagraph :: Paragraph -> IO ()
outputParagraph = mapM_ displayLine

displayLine :: Line -> IO ()
displayLine = (>> dilay) . outputLine

outputLine :: Line -> IO ()
outputLine = U.putStrLn

pause :: IO ()
pause = timeout (-1) getChar >> return ()

clear :: IO ()
clear = system "clear" >> return ()

delay :: IO ()
delay = timeout (10^5) >> return ()
