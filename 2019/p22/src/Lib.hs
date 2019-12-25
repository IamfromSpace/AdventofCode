module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import AdventOfCode.Util (elmTrace, findCycle)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List (foldl')
import Data.List.Split ()

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ()
import Data.Semigroup (Semigroup, stimes)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

data Shuffle
    = Cut Int
    | Deal Int
    | NewStack
    deriving (Show)

parseLine :: String -> Shuffle
parseLine ('c':'u':'t':' ':t) = Cut (read t)
parseLine ('d':'e':'a':'l':' ':'w':'i':'t':'h':' ':'i':'n':'c':'r':'e':'m':'e':'n':'t':' ':t) =
    Deal (read t)
parseLine "deal into new stack" = NewStack
parseLine s = error ("bad! " ++ s)

parse1 :: String -> _
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

deal :: Int -> Seq Int -> Int -> Seq Int -> Seq Int
deal !i !r !n !seq =
    let len = Seq.length seq
        into = (i * n) `mod` len
    in if i == len
           then r
           else deal
                    (i + 1)
                    (Seq.update into (fromJust $ Seq.lookup i seq) r)
                    n
                    seq

shuffle :: Shuffle -> Seq Int -> Seq Int
shuffle NewStack !seq = Seq.reverse seq
shuffle (Cut i) !seq =
    let (a, b) =
            if i < 0
                then Seq.splitAt (Seq.length seq + i) seq
                else Seq.splitAt i seq
    in b <> a
shuffle (Deal i) !seq = deal 0 seq i seq

manyShuffle :: Int -> [Shuffle] -> Seq Int
manyShuffle n = foldl' (\seq sh -> shuffle sh seq) (Seq.fromList [0 .. n - 1])

answer1 :: _ -> _
answer1 = Seq.elemIndexL 2019 . manyShuffle 10007

--An entry is a set of (originalIndex, newIndex) for
--just the first dealCount, the remainder can be derived
--Essentially, this is the "first player's stack" in the deal
mkCacheEntry :: Int -> Int -> Map Int Int
mkCacheEntry deckSize increment =
    let go !m !fromIndex =
            if fromIndex >= deckSize -- This doesn't work for certain sizes
                then m
                else let toIndexA = (fromIndex * increment) `mod` deckSize
                         (toIndex, fromIndex') =
                             if toIndexA <= increment
                                 then (toIndexA, fromIndex)
                                 else ( ((fromIndex + 1) * increment) `mod`
                                        deckSize
                                      , fromIndex + 1)
                     in if toIndex > increment
                            then error
                                     ("dead! " ++
                                      show increment ++ " " ++ show fromIndex')
                            else go
                                     (Map.insert toIndex fromIndex' m)
                                     (fromIndex' + (deckSize `div` increment))
    in go mempty 0

mkCaches :: Int -> [Int] -> Map Int (Map Int Int)
mkCaches deckSize = foldr (\i -> Map.insert i (mkCacheEntry deckSize i)) mempty

mkCachesFromShuffles :: Int -> [Shuffle] -> Map Int (Map Int Int)
mkCachesFromShuffles deckSize =
    mkCaches deckSize .
    catMaybes .
    fmap
        (\case
             Deal n -> Just n
             _ -> Nothing)

backtrackDeal :: Map Int (Map Int Int) -> Int -> Int -> Int
backtrackDeal cache n toIndex =
    let nCache = fromJust $ Map.lookup n cache
        (offset, firstParentIndex) = toIndex `divMod` n
    in offset + fromJust (Map.lookup firstParentIndex nCache)

backtrack :: Map Int (Map Int Int) -> Int -> Shuffle -> Int -> Int
backtrack _ deckSize NewStack toIndex = deckSize - toIndex - 1
backtrack _ deckSize (Cut n) toIndex = (toIndex + deckSize + n) `mod` deckSize
backtrack cache _ (Deal n) toIndex = backtrackDeal cache n toIndex

manyBacktrack :: Map Int (Map Int Int) -> Int -> [Shuffle] -> Int -> Int
manyBacktrack cache deckSize shuffles toIndex =
    foldr (backtrack cache deckSize) toIndex shuffles

posMod a b = ((a `mod` b) + b) `mod` b

data Term =
    Term Integer
         Integer
    deriving (Show)

--cardCount = 10
--cardCount = 10007
cardCount = 119315717514047

instance Semigroup Term where
    Term m1 b1 <> Term m2 b2 =
        Term ((m1 * m2) `mod` cardCount) ((m2 * b1 + b2) `mod` cardCount)

instance Monoid Term where
    mempty = Term 1 0

toTerm :: Shuffle -> Term
toTerm NewStack = Term (-1) (-1)
toTerm (Cut n) = Term 1 (fromIntegral (-n))
toTerm (Deal n) = Term (fromIntegral n) 0

applyTerm :: Term -> Integer -> Integer
applyTerm (Term m b) x = (m * x + b) `mod` cardCount

-- Once I had the terms reduced I put this in Wolfram Alpha, lol
-- If it can do it quickly, clearly there's a way I that I should be able to...
-- Also, stimes is _sweet_, because its automatically O(logn)
answer2 :: [Shuffle] -> _
answer2 s = stimes 101741582076661 $ foldMap toTerm s
-- Failed approaches/their test cases:
{-answer2 shuffles =
    let dSize = 119315717514047
        fn = manyBacktrack (mkCachesFromShuffles dSize shuffles) dSize shuffles
    in foldr (\x (last, list) -> (x, (last - x) : list)) (0, []) $
       take 500 $ iterate fn 2020
       -}
{- answer2 shuffles =
    let dSize = 119315717514047
        fn = manyBacktrack (mkCachesFromShuffles dSize shuffles) dSize shuffles
    in findCycle fn 2020
    -}
--answer2 _ = mkCacheEntry 119315717514047 59
{-answer2 shuffles =
    let dSize = 10007
    in manyBacktrack
           (mkCachesFromShuffles dSize shuffles)
           dSize
           shuffles
           6696
           -}
--answer2 _ = fmap (backtrack mempty 10 (Cut (-4))) [0 .. 9]
--answer2 _ = fmap (backtrack mempty 10 (Cut 3)) [0 .. 9]
--answer2 _ = mkCaches 10 [7]
--answer2 _ = fmap (backtrackDeal (mkCaches 10 [3]) 3) [0 .. 9]
