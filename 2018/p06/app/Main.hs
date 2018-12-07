module Main where

import Lib

main :: IO ()
main = do
    contents <- getContents
    print $ answer1 $ parse1 contents
    print $ answer2 $ parse2 contents
