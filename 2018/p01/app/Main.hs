module Main where

import Lib

main :: IO ()
main = fmap (show . answer . parse) getContents >>= putStr
