module Lib where

import AdventOfCode.Util ()
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
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

isKey :: Ord a => [[a]] -> Int -> Bool
isKey xs i =
    case (repeats3 (xs !! i)) of
        [] -> False
        threes ->
            any
                (\i' ->
                     let fives = Set.fromList $ repeats5 (xs !! i')
                     in (Set.size (Set.fromList threes `Set.intersection` fives) >
                         0))
                [i + 1 .. i + 1000]

triples :: Ord a => IntMap [a] -> Int -> Set a
triples xs i =
    Set.fromList (List.take 1 (repeats3 (Maybe.fromJust (IntMap.lookup i xs))))

quints :: Ord a => IntMap [a] -> Int -> Set a
quints xs i = Set.fromList (repeats5 (Maybe.fromJust (IntMap.lookup i xs)))

isKeyLazy :: Ord a => IntMap (Set a) -> IntMap (Set a) -> Int -> Bool
isKeyLazy ts qs i =
    let t = Maybe.fromJust (IntMap.lookup i ts)
    in if Set.size t == 0
           then False
           else any (\q -> (Set.size (Set.intersection t q)) > 0) $
                fmap
                    (\i' -> Maybe.fromJust (IntMap.lookup i' qs))
                    [i + 1 .. i + 1000]

-- 01:14:34.8
answer1 :: _ -> _
answer1 bs =
    let xs = IntMap.fromAscList $ fmap (\i -> (i, hashIndex bs i)) [0 .. 30000]
        ts = IntMap.fromAscList $ fmap (\i -> (i, triples xs i)) [0 .. 30000]
        qs = IntMap.fromAscList $ fmap (\i -> (i, quints xs i)) [0 .. 30000]
    in List.last $ List.take 64 $ List.filter (isKeyLazy ts qs) $ [0 .. 30000]

stretchIndex :: String -> Int -> String
stretchIndex bs i =
    (iterate
         (show . (Hash.hash :: ByteString -> Hash.Digest Hash.MD5) . BS8.pack)
         (bs <> show i)) !!
    2017

-- 01:38:34.267
answer2 :: _ -> _
answer2 bs =
    let xs =
            IntMap.fromAscList $
            fmap (\i -> (i, stretchIndex bs i)) [0 .. 30000]
        ts = IntMap.fromAscList $ fmap (\i -> (i, triples xs i)) [0 .. 30000]
        qs = IntMap.fromAscList $ fmap (\i -> (i, quints xs i)) [0 .. 30000]
    in List.last $ List.take 64 $ List.filter (isKeyLazy ts qs) $ [0 .. 30000]

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
