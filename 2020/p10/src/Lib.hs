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
import qualified Data.Either as Either
import Data.Foldable (fold, toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq(..), (<|), (|>))
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

answer1 :: [Int] -> _
answer1 xs =
    let pairs =
            filter (\x -> List.length x == 2) $
            fmap (take 2) $ List.tails $ List.sort xs
        ones = List.length $ filter (\[a, b] -> b - a == 1) pairs
        threes = List.length $ filter (\[a, b] -> b - a == 3) pairs
    in (ones + 1) * (threes + 1)

(!?) :: [a] -> Int -> Maybe a
(!?) xs i =
    case drop i xs of
        (x:_) -> Just x
        _ -> Nothing

elseNothing :: Maybe Bool -> Maybe a -> Maybe a
elseNothing b i = do
    b' <- b
    if b'
        then i
        else Nothing

arrangements :: [Int] -> [Sum Int]
arrangements (a:t@(_:_)) =
    let tArr = arrangements t
        ifDrop i = elseNothing ((\x -> x - a <= 3) <$> (t !? i)) (tArr !? i)
    in (fold ((tArr !? 0) <> ifDrop 1 <> ifDrop 2)) : tArr
arrangements (_:_) = [Sum 1]
arrangements [] = []

answer2 :: [Int] -> Int
answer2 = getSum . head . arrangements . List.sort . (:) 0

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 220

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: Int
ex2_1 = 8

ex2_2 :: Int
ex2_2 = 19208

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
