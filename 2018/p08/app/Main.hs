module Main where

import Lib

main :: IO ()
main = do
    contents <- getContents
    putStrLn "part 1:"
    print $ answer1 $ parse1 contents
    putStrLn "part 2:"
    print $ answer2 $ parse2 contents
