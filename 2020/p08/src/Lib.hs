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
import Text.ParserCombinators.PArrow ((>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.ParserCombinators.PArrow.MD (MD)
import Text.Read (readMaybe)

data Inst
    = Acc Int
    | Jmp Int
    | Nop Int
    deriving (Show, Eq)

parseNum :: String -> Int
parseNum s =
    case head s of
        '-' -> (-1) * read (drop 1 s)
        '+' -> read (drop 1 s)

parseInst :: String -> Inst
parseInst s =
    case Split.splitOn " " s of
        ["acc", x] -> Acc (parseNum x)
        ["jmp", x] -> Jmp (parseNum x)
        ["nop", x] -> Nop (parseNum x)

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

findLoop :: Seq Inst -> Set Int -> (Int, Int) -> Int
findLoop insts seen state =
    let (ip, acc) = step insts state
        seen' = Set.insert ip seen
    in if Set.member ip seen
           then acc
           else findLoop insts seen' (ip, acc)

answer1 :: _ -> _
answer1 insts = findLoop insts mempty (0, 0)

findLoop2 :: Seq Inst -> Set Int -> (Int, Int) -> Maybe Int
findLoop2 insts seen state =
    let (ip, acc) = step insts state
        seen' = Set.insert ip seen
        cont =
            if Set.member ip seen
                then Nothing
                else findLoop2 insts seen' (ip, acc)
    in if ip >= Seq.length insts
           then Just acc
           else cont

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
        (\i -> swap i insts >>= (\insts' -> findLoop2 insts' mempty (0, 0)))
        [0 .. Seq.length insts - 1]

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
