module Lib where

import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding ((++), lookup, map)

process :: [(Bool, String)] -> Char -> [(Bool, String)]
process (h:t) c =
    case c of
        '[' -> ((True, "") : fmap List.reverse h : t)
        ']' -> (False, "") : fmap List.reverse h : t
        c -> fmap ((:) c) h : t

parse1 :: String -> _
parse1 = fmap (List.reverse . List.foldl' process [(False, "")]) . lines

parse2 :: String -> _
parse2 = parse1

hasAbba :: String -> Bool
hasAbba (a:b:t) = hasAbba' a b t
hasAbba _ = False

hasAbba' :: Char -> Char -> String -> Bool
hasAbba' a b (hb:ha:t) =
    (a == ha && b == hb && a /= b) || hasAbba' b hb (ha : t)
hasAbba' _ _ _ = False

supportsTls :: [(Bool, String)] -> Bool
supportsTls xs =
    all (\(_, sequence) -> not (hasAbba sequence)) (List.filter fst xs) &&
    any (\(_, sequence) -> hasAbba sequence) (List.filter (not . fst) xs)

-- about 27 minutes
answer1 :: _ -> _
answer1 = List.length . List.filter id . fmap supportsTls

allAbas :: String -> [(Char, Char)]
allAbas (a:b:t) = allAbas' a b t
allAbas _ = mempty

allAbas' :: Char -> Char -> String -> [(Char, Char)]
allAbas' a b (ha:t) =
    (if (a == ha && a /= b)
         then (:) (a, b)
         else id) $
    allAbas' b ha t
allAbas' _ _ _ = mempty

supportsSsl :: [(Bool, String)] -> Bool
supportsSsl xs =
    let inBracket =
            Set.fromList $
            fmap (\(a, b) -> (b, a)) $
            List.concatMap
                ((\(_, sequence) -> allAbas sequence))
                (List.filter fst xs)
        outBracket =
            Set.fromList $
            List.concatMap
                ((\(_, sequence) -> allAbas sequence))
                (List.filter (not . fst) xs)
    in Set.size (Set.intersection inBracket outBracket) > 0

-- About 42 minutes (total)
answer2 :: _ -> _
answer2 = List.length . List.filter id . fmap supportsSsl

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 1

ex1_2 :: Int
ex1_2 = 0

ex1_3 :: Int
ex1_3 = 0

ex1_4 :: Int
ex1_4 = 1

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
