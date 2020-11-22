module Main where

import Control.Monad (when)
import Data.Set (fromList, member, null)
import Lib
import Prelude hiding (null)
import System.Environment (getArgs)
import System.IO (stderr, stdout, hPutStr, hPutStrLn)

main :: IO ()
main = do
    contents <- getContents
    args <- fromList <$> getArgs
    let pt1 = member "--pt1" args || null args
    let pt2 = member "--pt2" args || null args
    when pt1 $ do
        hPutStrLn stderr "part 1:"
        hPutStr stderr $ show1 $ answer1 $ parse1 contents
        hPutStr stderr "\n"
        when (not pt2) $ putStr $ show1 $ answer1 $ parse1 contents
    when pt2 $ do
        hPutStrLn stderr "part 2:"
        hPutStr stderr $ show2 $ answer2 $ parse2 contents
        hPutStr stderr "\n"
        when (not pt1) $ putStr $ show2 $ answer2 $ parse2 contents
