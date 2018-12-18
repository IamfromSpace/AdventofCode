module Main where

import Lib

main :: IO ()
main = do
    contents <- getContents
    putStrLn "part 1:"
    putStr $ answer1 $ parse1 contents
    putStrLn ""
    putStrLn "part 2:"
    putStr $ answer2 $ parse2 contents
    putStrLn ""
