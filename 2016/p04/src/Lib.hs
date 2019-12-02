module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , parseLine
    , isReal
    , top5
    , rotateRoom
    ) where

--import Prelude hiding (lookup)
import Data.Map (Map)

--import Data.Set (Set)
import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Data.Char (chr, ord)
import Data.List (foldl', intercalate, sort)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as Map (alter, foldrWithKey)
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

parseLine :: String -> (String, Int, String)
parseLine s =
    let [x, y, z] = splitOneOf "[]" s
        (h:t) = reverse $ splitOn "-" x
    in (concat (reverse t), read h, y)

parse1 :: String -> [(String, Int, String)]
parse1 = fmap parseLine . lines

parseLine2 :: String -> (String, Int, String)
parseLine2 s =
    let [x, y, z] = splitOneOf "[]" s
        (h:t) = reverse $ splitOn "-" x
    in (intercalate " " (reverse t), read h, y)

parse2 :: String -> _
parse2 = fmap parseLine2 . lines

frequency :: String -> Map Char Integer
frequency =
    foldl'
        (\map c ->
             if c == '-' || c == ' '
                 then map
                 else Map.alter
                          (\case
                               Just x -> Just $ x - 1
                               Nothing -> Just $ -1)
                          c
                          map)
        mempty

flipMap :: Map a b -> [(b, a)]
flipMap = Map.foldrWithKey (\k v list -> (v, k) : list) mempty

top5 :: String -> String
top5 = fmap snd . take 5 . sort . flipMap . frequency

isReal :: (String, Int, String) -> Bool
isReal (x, _, y) = top5 x == y

-- time to completion: 21:10.24
answer1 :: [(String, Int, String)] -> Int
answer1 = sum . fmap (\(_, x, _) -> x) . filter isReal

rotate :: Int -> Char -> Char
rotate _ ' ' = ' '
rotate i c = chr (((ord c + i - 97) `mod` 26) + 97)

rotateRoom :: (String, Int, String) -> (String, Int, String)
rotateRoom (scramble, sid, chk) = (fmap (rotate sid) scramble, sid, chk)

-- time to completion: 36:07.95
-- Piped to `grep pole` to find the room from this output
answer2 :: [(String, Int, String)] -> [(String, Int, String)]
answer2 = fmap rotateRoom . filter isReal
