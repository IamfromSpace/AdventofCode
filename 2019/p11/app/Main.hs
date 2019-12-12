module Main where

import Control.Monad (when)
import Data.Set (fromList, member, null)
import Data.String (toString)
import Lib
import Prelude hiding (null)
import System.Environment (getArgs)

main :: IO ()
main = do
    contents <- getContents
    args <- fromList <$> getArgs
    let pt1 = member "--pt1" args || null args
    let pt2 = member "--pt2" args || null args
    when pt1 $ do
        putStrLn "part 1:"
        putStr $ toString $ answer1 $ parse1 contents
    when pt2 $ do
        putStrLn "\npart 2:"
        putStr $ toString $ parse2 contents
    putStrLn ""
