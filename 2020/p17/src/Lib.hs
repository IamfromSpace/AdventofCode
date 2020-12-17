module Lib where

import AdventOfCode.Util (Vector(..), multiLines)
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

--parseGrid ::
--       (Integral a, Num a) => ((a, a) -> Char -> b -> b) -> b -> String -> b
parse1 :: String -> Set (Vector (Integer, Integer, Integer))
parse1 =
    Util.parseGrid
        (\(x, y) c s ->
             if c == '#'
                 then Set.insert (Vector (x, y, 0)) s
                 else s)
        mempty

parse2 :: String -> Set (Vector (Integer, Integer, Integer, Integer))
parse2 =
    Util.parseGrid
        (\(x, y) c s ->
             if c == '#'
                 then Set.insert (Vector (x, y, 0, 0)) s
                 else s)
        mempty

neighbors :: [Vector (Integer, Integer, Integer)]
neighbors =
    drop 1 $
    App.liftA3 (\a b c -> Vector (a, b, c)) [0, 1, -1] [0, 1, -1] [0, 1, -1]

type S = Set (Vector (Integer, Integer, Integer))

getPoi :: S -> S
getPoi =
    List.foldl'
        (\s' p -> List.foldl' (flip Set.insert) s' $ fmap ((<>) p) $ neighbors)
        mempty .
    Set.toList

willBeActive :: Vector (Integer, Integer, Integer) -> S -> Bool
willBeActive p s =
    let count =
            length $
            filter id $ fmap (flip Set.member s) $ fmap ((<>) p) $ neighbors
    in if Set.member p s
           then count == 2 || count == 3
           else count == 3

step :: S -> S
step s = Set.fromList $ filter (flip willBeActive s) $ Set.toList $ getPoi s

answer1 :: _ -> _
answer1 input = Set.size $ step (step (step (step (step (step (input))))))

neighbors2 :: [Vector (Integer, Integer, Integer, Integer)]
neighbors2 =
    drop
        1
        ((\a b c d -> Vector (a, b, c, d)) <$> [0, 1, -1] <*> [0, 1, -1] <*>
         [0, 1, -1] <*>
         [0, 1, -1])

type S4 = Set (Vector (Integer, Integer, Integer, Integer))

getPoi2 :: S4 -> S4
getPoi2 =
    List.foldl'
        (\s' p -> List.foldl' (flip Set.insert) s' $ fmap ((<>) p) $ neighbors2)
        mempty .
    Set.toList

willBeActive2 :: Vector (Integer, Integer, Integer, Integer) -> S4 -> Bool
willBeActive2 p s =
    let count =
            length $
            filter id $ fmap (flip Set.member s) $ fmap ((<>) p) $ neighbors2
    in if Set.member p s
           then count == 2 || count == 3
           else count == 3

step2 :: S4 -> S4
step2 s = Set.fromList $ filter (flip willBeActive2 s) $ Set.toList $ getPoi2 s

answer2 :: _ -> _
answer2 input = Set.size $ step2 (step2 (step2 (step2 (step2 (step2 (input))))))

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 112

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
