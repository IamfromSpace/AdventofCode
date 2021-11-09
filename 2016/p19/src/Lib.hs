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
import Data.Foldable (toList)
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

parse1 :: String -> Int
parse1 = read

parse2 :: String -> _
parse2 = parse1

round1 :: Int -> Seq Int -> (Seq Int, Int)
round1 currentIndex s =
    let nextIndex = (currentIndex + 1) `mod` Seq.length s
    in (Seq.deleteAt nextIndex s, currentIndex + 1)

round' :: _ -> Int -> Seq Int -> Seq Int
round' f i s =
    if i >= Seq.length s
        then s
        else let (s', i') = f i s
             in round' f i' s'

fullRound :: _ -> Seq Int -> Seq Int
fullRound f = round' f 0

fullGame :: _ -> Seq Int -> Seq Int
fullGame f s =
    let n = fullRound f s
    in if length n <= 1
           then n
           else fullGame f n

-- 54:32 (oof)
answer1 :: Int -> _
answer1 i = fullGame round1 $ Seq.fromList [1 .. i]

round12 :: Int -> Seq Int -> (Seq Int, Int)
round12 currentIndex s =
    let nextIndex = (currentIndex + (Seq.length s `div` 2)) `mod` Seq.length s
    in ( Seq.deleteAt nextIndex s
       , if nextIndex > currentIndex
             then currentIndex + 1
             else currentIndex)

-- 1:16:27 (Would be 94 on the leaderboard!)
answer2 :: _ -> _
answer2 i = fullGame round12 $ Seq.fromList [1 .. i]

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: _
ex1_1 = undefined

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: _
ex2_1 = undefined

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
