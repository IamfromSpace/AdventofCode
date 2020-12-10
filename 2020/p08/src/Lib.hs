module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
import Control.Monad ((>=>))
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
import Text.ParserCombinators.PArrow ((>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.ParserCombinators.PArrow.MD (MD)
import Text.Read (readMaybe)

data Inst
    = Acc Int
    | Jmp Int
    | Nop Int
    deriving (Show, Eq)

parsePlussed :: String -> Int
parsePlussed = read . List.dropWhile ((==) '+')

parseInst :: String -> Inst
parseInst s =
    case Split.splitOn " " s of
        ["acc", x] -> Acc (parsePlussed x)
        ["jmp", x] -> Jmp (parsePlussed x)
        ["nop", x] -> Nop (parsePlussed x)
        _ -> error "Couldn't parse Inst"

parse1 :: String -> _
parse1 = Seq.fromList . fmap parseInst . lines

parse2 :: String -> _
parse2 = parse1

step :: Seq Inst -> (Int, Int) -> (Int, Int)
step insts (ip, acc) =
    case Maybe.fromJust $ Seq.lookup ip insts of
        Acc i -> (ip + 1, acc + i)
        Jmp i -> (ip + i, acc)
        Nop _ -> (ip + 1, acc)

run :: Seq Inst -> (Int, Int) -> [(Int, Int)]
run = iterate . step

findLoop :: Set Int -> [(Int, Int)] -> Int
findLoop seen ((ip, acc):t) =
    if Set.member ip seen
        then acc
        else findLoop (Set.insert ip seen) t
findLoop _ [] = error "machine halted!"

answer1 :: _ -> _
answer1 = findLoop mempty . flip run (0, 0)

findLoop2 :: Int -> Set Int -> [(Int, Int)] -> Maybe Int
findLoop2 n seen ((ip, acc):t) =
    if ip >= n
        then Just acc
        else if Set.member ip seen
                 then Nothing
                 else findLoop2 n (Set.insert ip seen) t
findLoop2 _ _ [] = error "machine halted!"

swap :: Int -> Seq Inst -> Maybe (Seq Inst)
swap i insts =
    case Maybe.fromJust $ Seq.lookup i insts of
        Acc _ -> Nothing
        Jmp x -> Just $ Seq.update i (Nop x) insts
        Nop x -> Just $ Seq.update i (Jmp x) insts

answer2 :: _ -> _
answer2 insts =
    head $
    Maybe.catMaybes $
    fmap
        (flip swap insts >=>
         (findLoop2 (Seq.length insts) mempty . (flip run (0, 0))))
        [0 ..]

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

ex2_1 :: Int
ex2_1 = 8

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
