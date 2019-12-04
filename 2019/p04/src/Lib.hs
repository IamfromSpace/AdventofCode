module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , isValid
    , isValid2
    , doubleRun
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split (splitOn)
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

parse1 :: String -> (Int, Int)
parse1 x =
    let [a, b] = splitOn "-" x
    in (read a, read b)

parse2 :: String -> _
parse2 = parse1

isValid :: Int -> Bool
isValid x =
    let [a, b, c, d, e, f] = show x
        aNum = (read (a : [])) :: Int
        bNum = (read (b : [])) :: Int
        cNum = (read (c : [])) :: Int
        dNum = (read (d : [])) :: Int
        eNum = (read (e : [])) :: Int
        fNum = (read (f : [])) :: Int
        increasing =
            aNum <= bNum &&
            bNum <= cNum && cNum <= dNum && dNum <= eNum && eNum <= fNum
        double = a == b || b == c || c == d || d == e || e == f
    in increasing && double

answer1 :: _ -> _
answer1 (a, b) = length $ filter isValid $ [a .. b]

doubleRun :: Int -> Char -> String -> Bool
doubleRun 2 c (h:t) =
    if c == h
        then doubleRun 3 c t
        else True
doubleRun seen c (h:t) =
    if c == h
        then doubleRun (seen + 1) c t
        else doubleRun 0 c t
doubleRun 2 _ [] = True
doubleRun _ _ [] = False

-- Oof, neither fast nor good
isValid2 :: Int -> Bool
isValid2 x =
    let s = show x
        [a, b, c, d, e, f] = show x
        aNum = (read (a : [])) :: Int
        bNum = (read (b : [])) :: Int
        cNum = (read (c : [])) :: Int
        dNum = (read (d : [])) :: Int
        eNum = (read (e : [])) :: Int
        fNum = (read (f : [])) :: Int
        increasing =
            aNum <= bNum &&
            bNum <= cNum && cNum <= dNum && dNum <= eNum && eNum <= fNum
        double =
            doubleRun 0 '0' s ||
            doubleRun 0 '1' s ||
            doubleRun 0 '2' s ||
            doubleRun 0 '3' s ||
            doubleRun 0 '4' s ||
            doubleRun 0 '5' s ||
            doubleRun 0 '6' s ||
            doubleRun 0 '7' s || doubleRun 0 '8' s || doubleRun 0 '9' s
    in increasing && double

answer2 :: _ -> _
answer2 (a, b) = length $ filter isValid2 $ [a .. b]
