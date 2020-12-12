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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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

input :: MD String String
input = PA.many PA.anyChar

--parseGrid ::
--       (Integral a, Num a) => ((a, a) -> Char -> b -> b) -> b -> String -> b
parse1 :: String -> Map (Integer, Integer) Bool
parse1 xs =
    Util.parseGrid
        (\p c m ->
             if c == 'L'
                 then Map.insert p False m
                 else m)
        Map.empty
        xs

parse2 :: String -> _
parse2 = parse1

step :: Map (Integer, Integer) Bool -> Map (Integer, Integer) Bool
step m =
    Map.foldrWithKey
        (\(x, y) currentlyOcc m' ->
             let occ1 = Map.lookup (x + 1, y) m
                 occ2 = Map.lookup (x - 1, y) m
                 occ3 = Map.lookup (x, y + 1) m
                 occ4 = Map.lookup (x, y - 1) m
                 occ5 = Map.lookup (x + 1, y + 1) m
                 occ6 = Map.lookup (x - 1, y - 1) m
                 occ7 = Map.lookup (x - 1, y + 1) m
                 occ8 = Map.lookup (x + 1, y - 1) m
                 occCount =
                     List.length $
                     filter id $
                     Maybe.catMaybes
                         [occ1, occ2, occ3, occ4, occ5, occ6, occ7, occ8]
             in if currentlyOcc
                    then Map.insert (x, y) (not (occCount >= 4)) m'
                    else Map.insert (x, y) (occCount == 0) m')
        mempty
        m

--findCycle :: Eq a => (a -> a) -> a -> (Integer, Integer, a)
answer1 :: _ -> _
answer1 = (\m -> List.length $ List.filter id $ Map.elems m) . fix step

look ::
       Map (Integer, Integer) Bool
    -> (Integer, Integer)
    -> (Integer, Integer)
    -> Bool
look m (x, y) (xOff, yOff) =
    let options =
            List.zip
                (takeWhile
                     (\xx -> xx >= 0 && xx <= 99)
                     [x + xOff,x + 2 * xOff ..])
                (takeWhile
                     (\xx -> xx >= 0 && xx <= 99)
                     [y + yOff,y + 2 * yOff ..])
        xxx = Maybe.catMaybes $ fmap (flip Map.lookup m) options
    in case xxx of
           [] -> False
           (h:_) -> h

step2 :: Map (Integer, Integer) Bool -> Map (Integer, Integer) Bool
step2 m =
    Map.foldrWithKey
        (\(x, y) currentlyOcc m' ->
             let occCount =
                     List.length $
                     filter id $
                     [ look m (x, y) (1, 0)
                     , look m (x, y) (-1, 0)
                     , look m (x, y) (0, 1)
                     , look m (x, y) (0, -1)
                     , look m (x, y) (1, 1)
                     , look m (x, y) (-1, -1)
                     , look m (x, y) (-1, 1)
                     , look m (x, y) (1, -1)
                     ]
             in if currentlyOcc
                    then Map.insert (x, y) (not (occCount >= 5)) m'
                    else Map.insert (x, y) (occCount == 0) m')
        mempty
        m

fix :: Eq a => (a -> a) -> a -> a
fix fn a =
    let next = fn a
    in if a == next
           then a
           else fix fn next

answer2 :: _ -> _
answer2 = (List.length . List.filter id . Map.elems) . fix step2

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
ex2_1 = 26

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
