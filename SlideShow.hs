-- SlideShow that console demo application.
-- 2009-03-20 16:54:49

module Main where

import Codec.Binary.UTF8.String
import Data.List
import System.Cmd
import System.Environment
import System.Exit
import System.IO
import qualified System.IO.UTF8 as U
import System.Posix
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
main =   initialize
     >>  inputSetup 
     >>= mapM_ displayParagraph . paragraphs . lines

initialize :: IO ()
initialize = hSetBuffering stdin NoBuffering
           >> hSetBuffering stdout NoBuffering
           >> stdinNoEcho

stdinNoEcho :: IO ()
stdinNoEcho =   getTerminalAttributes stdInput
            >>= flip (setTerminalAttributes stdInput) Immediately
              . flip withoutMode EnableEcho
 
inputSetup :: IO String
inputSetup = getArgs >>= U.readFile . head

displayParagraph :: Paragraph -> IO ()
displayParagraph = (pause >>) . (clear >>) . outputParagraph

outputParagraph :: Paragraph -> IO ()
outputParagraph = mapM_ (dispatchLine displayLine)

dispatchLine :: (Line -> IO ()) -> Line -> IO ()
dispatchLine dl l
  | ":p" `isPrefixOf` l = pause
  | ":c" `isPrefixOf` l = clear
  | "% " `isPrefixOf` l = putStr "% " >> pause >> dl (drop 2 l)
  | not (null prompt)   = putStr prompt >> pause >> dl rest
  | otherwise           = dl l
    where
      (prompt, rest) = case break ('>' ==) l of
                         (_, [])        -> ("", "")
                         (xs, _:' ':ys) -> if ' ' `elem` xs then ("", "")
                                           else (xs++"> ", ys)
                         _              -> ("", "")

displayLine :: Line -> IO ()
displayLine = (>> delay (10^5)) . (>> newline) . outputLine

outputLine :: Line -> IO ()
outputLine = mapM_ displayChar

displayChar :: Char -> IO ()
displayChar = (>> delay (10^5)) . outputChar

outputChar :: Char -> IO ()
outputChar = putStr. encodeString . (:[])

pause :: IO ()
pause = timeout (-1) getChar >>= selector

selector :: Maybe Char -> IO ()
selector mc = case mc of
                Just 'q' -> exitSuccess
                _        -> return ()

clear :: IO ()
clear = system "clear" >> return ()

delay :: Int -> IO ()
delay iv = timeout iv getChar >> return ()

newline :: IO ()
newline = putChar '\n'
