module Main where

import Lib

main :: IO ()
main = do
    contents <- getContents
    putStrLn "part 1:"
    putStr $ show $ answer1 $ parse1 contents
    putStrLn "\npart 2:"
    putStr $ show $ answer2 $ parse2 contents
    putStrLn ""
