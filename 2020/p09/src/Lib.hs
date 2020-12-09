module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Prelude hiding ((++), init, lookup, map)
import Text.ParserCombinators.PArrow (MD, (>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.Read (readMaybe)

parse1 :: String -> [Int]
parse1 = fmap read . lines

parse2 :: String -> _
parse2 = parse1

adds :: [Int] -> [Int]
adds (h:t) = fmap ((+) h) t <> adds t
adds [] = []

find :: Int -> [Int] -> Int
find n xs =
    let s = Set.fromList $ adds (take n xs)
    in if Set.member (xs !! n) s
           then find n (List.tail xs)
           else (xs !! n)

answer1 :: [Int] -> Int
answer1 = find 25

tryTotal :: Int -> [Int] -> Maybe [Int]
tryTotal target xs =
    case take 1 $
         reverse $
         List.takeWhile (\x -> fst x <= target) $
         fmap (\n -> (sum $ take n xs, n)) [2 .. 100] of
        [stretch] ->
            if fst stretch == target
                then Just (take (snd stretch) xs)
                else Nothing
        _ -> Nothing

findTotal :: Int -> [Int] -> Int
findTotal target xs =
    let x =
            List.sort $
            head $ Maybe.catMaybes $ fmap (tryTotal target) $ List.tails xs
    in head x + head (reverse x)

answer2 :: _ -> _
answer2 xs = findTotal (answer1 xs) xs

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 127

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: Int
ex2_1 = 62

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
