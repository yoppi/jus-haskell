-- SlideShow that console demo application.
-- 2009-03-20 16:54:49

module Main where

import Codec.Binary.UTF8.String
import Control.Monad.State
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

type Display = StateT Mode IO ()
data Mode = Normal | Delayed
type Modifier = Mode -> Mode

defaultMode :: Mode
defaultMode = Normal

defaultCharDelay :: Int
defaultCharDelay = 10^3

delayed, normal :: Modifier
delayed = const Delayed
normal  = const Normal

setDelayed, setNormal :: Display
setDelayed = modify delayed
setNormal  = modify normal

runDisplay :: Display -> IO ()
runDisplay dsp = evalStateT dsp defaultMode

slideShow :: String -> Display
slideShow = mapM_ displayParagraph . paragraphs . lines

main :: IO ()
main =   initialize
     >>  inputSetup 
     >>= runDisplay . slideShow

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

displayParagraph :: Paragraph -> Display
displayParagraph = (setNormal >>) .  (pause >>) . (clear >>) 
                 . outputParagraph

outputParagraph :: Paragraph -> Display
outputParagraph = mapM_ (dispatchLine displayLine)

dispatchLine :: (Line -> Display) -> Line -> Display
dispatchLine dl l
  | "-"  `isPrefixOf` l = dl (tail l) >> newline
  | "="  `isPrefixOf` l = dl (tail l)
  | ":e" `isPrefixOf` l = setDelayed
  | ":r" `isPrefixOf` l = setNormal
  | ":p" `isPrefixOf` l = setNormal >> pause
  | ":c" `isPrefixOf` l = setNormal >> clear
  | "% " `isPrefixOf` l = liftIO (putStr "% ") >> setDelayed 
                        >> pause >> dl (drop 2 l)
                        >> setNormal >> pause >> newline
  | not (null prompt)   = liftIO (putStr prompt) >> setDelayed
                        >> pause >> dl rest
                        >> setNormal >> pause >> newline
  | otherwise           = dl l >> newline
    where
      (prompt, rest) = case break ('>' ==) l of
                         (_, [])        -> ("", "")
                         (xs, _:' ':ys) -> if ' ' `elem` xs then ("", "")
                                           else (xs++"> ", ys)
                         _              -> ("", "")

displayLine :: Line -> Display
displayLine = outputLine

outputLine :: Line -> Display
outputLine = mapM_ displayChar

displayChar :: Char -> Display
displayChar = (get >>= branchOn nop (delay defaultCharDelay) >>)
            . outputChar

branchOn :: Display -> Display -> Mode -> Display
branchOn n d Normal = n
branchOn n d _      = d

outputChar :: Char -> Display
outputChar = liftIO . putStr. encodeString . (:[])

pause :: Display
pause = liftIO (timeout (-1) getChar) >>= selector

selector :: Maybe Char -> Display
selector mc = case mc of
                Just 'q' -> liftIO exitSuccess
                _        -> return ()

clear :: Display
clear = liftIO $ system "clear" >> return ()

delay :: Int -> Display
delay iv = liftIO $ timeout iv getChar >> return ()

newline :: Display
newline = liftIO $ putChar '\n'

nop :: Display
nop = return ()
