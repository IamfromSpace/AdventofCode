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
import qualified Data.Either as Either
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

step :: Seq Inst -> (Int, Int) -> Either Int (Int, Int)
step insts (ip, acc) =
    maybe (Left acc) Right $
    fmap
        (\case
             Acc i -> (ip + 1, acc + i)
             Jmp i -> (ip + i, acc)
             Nop _ -> (ip + 1, acc))
        (Seq.lookup ip insts)

run :: Seq Inst -> Either Int (Int, Int) -> [Either Int (Int, Int)]
run = iterate . ((=<<) . step)

consume ::
       Set Int
    -> [(Int, Int)]
    -> [Either Int (Int, Int)]
    -> Either [(Int, Int)] Int
consume seen path (Right (ip, acc):t) =
    if Set.member ip seen
        then Left path
        else consume (Set.insert ip seen) ((ip, acc) : path) t
consume _ _ (Left acc:_) = Right acc
consume _ _ [] = error "iterations must be infinite!"

-- Return either the cycle (Left) or the final value of the accumulator (Right)
-- TODO: really we could just return the path in all cases and a Bool if it's a cycle or a halt
runToHalt :: Seq Inst -> Either [(Int, Int)] Int
runToHalt = consume mempty mempty . flip run (pure (0, 0))

answer1 :: _ -> _
answer1 = snd . head . Either.fromLeft [] . runToHalt

swap :: Int -> Seq Inst -> Maybe (Seq Inst)
swap i insts =
    case Maybe.fromJust $ Seq.lookup i insts of
        Acc _ -> Nothing
        Jmp x -> Just $ Seq.update i (Nop x) insts
        Nop x -> Just $ Seq.update i (Jmp x) insts

answer2 :: _ -> _
answer2 insts =
    head $
    Either.rights $
    fmap runToHalt $ Maybe.catMaybes $ fmap (flip swap insts) [0 ..]

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
