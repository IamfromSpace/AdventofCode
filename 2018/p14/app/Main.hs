module Main where

import Lib

main :: IO ()
main = do
    contents <- getContents
    putStrLn "part 1:"
    x <- answer1 $ parse1 contents
    putStr x
    putStrLn "part 2:"
    y <- answer2
    putStr y
    putStrLn ""
