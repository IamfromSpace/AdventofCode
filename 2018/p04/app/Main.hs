module Main where

import Lib

main :: IO ()
main = fmap ((++ "\n") . show . answer . parse) getContents >>= putStr
