module Lib where

import AdventOfCode.Util ()
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Monad (guard)
import qualified Control.Monad.State.Lazy as Stae

-- cryptonite is unfortunately slower!
-- long term should just hide hash details in a `String -> String` utility
import qualified Crypto.Hash as Hash
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import Data.Foldable (foldMap, toList)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
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
import Data.Word (Word8)
import Prelude hiding ((++), init, lookup, map)

parse1 :: String -> String
parse1 = id

parse2 :: String -> _
parse2 = parse1

hashIndex :: String -> Int -> String
hashIndex bs i =
    show $ (Hash.hash (BS8.pack (bs <> show i)) :: Hash.Digest Hash.MD5)

repeats3 :: Eq a => [a] -> [a]
repeats3 (a:b:c:t) =
    (if (a == b && a == c)
         then (:) a
         else id)
        (repeats3 (b : c : t))
repeats3 _ = []

repeats5 :: Eq a => [a] -> [a]
repeats5 (a:b:c:d:e:t) =
    (if (a == b && a == c && a == d && a == e)
         then (:) a
         else id)
        (repeats5 (b : c : d : e : t))
repeats5 _ = []

isKeyMonadic :: _ -> String -> [Int]
isKeyMonadic h bs = do
    i <- [0 ..]
    let asHash = h bs i
    three <- List.take 1 (repeats3 asHash)
    j <- [i + 1 .. i + 1000]
    guard (List.elem three (repeats5 (h bs j)))
    return i

-- 01:14:34.8
answer1 :: _ -> _
answer1 bs = isKeyMonadic hashIndex bs !! 63

stretchIndex :: String -> Int -> String
stretchIndex bs i =
    (iterate
         (show . (Hash.hash :: ByteString -> Hash.Digest Hash.MD5) . BS8.pack)
         (bs <> show i)) !!
    2017

-- 01:38:34.267
answer2 :: _ -> _
answer2 bs = isKeyMonadic stretchIndex bs !! 63

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
