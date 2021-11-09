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

numParser = PA.many1 PA.digit >>> arr read

lineParser = (numParser >>! PA.char '-') &&& numParser

parser :: MD _ [(Int, Int)]
parser = PA.sepBy1 lineParser (PA.char '\n')

runP p = either (error . concat) id . PA.runParser p

-- always ordered
parse1 :: String -> _
parse1 = runP parser

parse2 :: String -> _
parse2 = parse1

grow' :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
grow' a@(a1, a2) b@(b1, b2) =
    if a2 > b2
        then [a]
        else if a2 + 1 >= b1
                 then [(a1, b2)]
                 else [a, b]

grow :: [(Int, Int)] -> [(Int, Int)]
grow (a:b:t) =
    case grow' a b of
        [a] -> grow (a : t)
        [a, b] -> a : grow (b : t)
grow [a] = [a]
grow [] = []

-- 37:17 (ouch; 13min remembering how arrows worked)
answer1 :: [(Int, Int)] -> _
answer1 xs =
    let sorted = List.sort xs
    in (+) 1 $ snd $ head $ grow sorted

countAllowed :: [(Int, Int)] -> Int
countAllowed ((_, a2):b@(b1, _):t) = b1 - a2 - 1 + countAllowed (b : t)
countAllowed [(_, b2)] = 4294967295 - b2
countAllowed [] = 0

-- 41:03
answer2 :: _ -> _
answer2 = countAllowed . grow . List.sort

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
