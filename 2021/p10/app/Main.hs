module Main where

import Control.Monad (when)
import Data.Set (fromList, member, null)
import Lib
import Prelude hiding (null)
import System.Environment (getArgs)
import System.Hclip (setClipboard)
import System.IO (stderr, stdout, hPutStr, hPutStrLn)

-- For running the different parts via CLI, utilizing pipes.  Input must be
-- piped in, debugging info will be pipe to stdout, and if only a single part
-- is selected (via flag "--pt1" or "--pt2") then the output will be piped to
-- stdout (where it can be piped into the clipboard or an HTTP request).
main :: IO ()
main = do
    contents <- getContents
    args <- fromList <$> getArgs
    let pt1 = member "--pt1" args || null args
    let pt2 = member "--pt2" args || null args
    when pt1 $ do
        hPutStrLn stderr "part 1:"
        let answerStr = show1 $ answer1 $ parse1 contents
        hPutStr stderr answerStr
        hPutStr stderr "\n"
        when (not pt2) $ putStr answerStr
    when pt2 $ do
        hPutStrLn stderr "part 2:"
        let answerStr = show2 $ answer2 $ parse2 contents
        hPutStr stderr answerStr
        hPutStr stderr "\n"
        when (not pt1) $ putStr answerStr

-- For running the differnt parts in a in a REPL.  Rather than pipes in and
-- out, this will read a file named input.txt, output the solution, and put its
-- string in the clipboard (beware of double quoted strings!).  Note that a
-- compiled and installed binary is substantially faster, but this has a faster
-- time to re-execute after changes.
rep1 :: IO _
rep1 = do
    contents <- readFile "./input.txt"
    let answer = answer1 $ parse1 contents
    setClipboard (show1 answer)
    return answer

rep2 :: IO _
rep2 = do
    contents <- readFile "./input.txt"
    let answer = answer2 $ parse2 contents
    setClipboard (show2 answer)
    return answer
