module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , step
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

--import Data.Set (Set)
import AdventOfCode.Util (listToIndexMap)
import Control.Applicative (liftA2)
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split (splitOn)
import qualified Data.Map as Map (insert, lookup)
import Data.Maybe (fromJust)
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

parse1 :: String -> Map Int Int
parse1 = listToIndexMap . fmap read . splitOn ","

step :: (Maybe Int, Map Int Int) -> (Maybe Int, Map Int Int)
step (Nothing, inst) = (Nothing, inst)
step (Just i, inst) =
    case ( Map.lookup i inst
         , Map.lookup (i + 1) inst
         , Map.lookup (i + 2) inst
         , Map.lookup (i + 3) inst) of
        (Just 1, Just a, Just b, Just c) ->
            ( Just (i + 4)
            , Map.insert
                  c
                  (fromJust (Map.lookup a inst) + fromJust (Map.lookup b inst))
                  inst)
        (Just 2, Just a, Just b, Just c) ->
            ( Just (i + 4)
            , Map.insert
                  c
                  (fromJust (Map.lookup a inst) * fromJust (Map.lookup b inst))
                  inst)
        (Just 99, _, _, _) -> (Nothing, inst)

parse2 :: String -> _
parse2 = parse1

answer1 :: _ -> _
answer1 =
    Map.lookup 0 .
    snd .
    until ((==) Nothing . fst) step .
    (,) (Just 0) . Map.insert 2 2 . Map.insert 1 12

computer :: (Int, Int) -> Map Int Int -> Int
computer (noun, verb) =
    fromJust .
    Map.lookup 0 .
    snd .
    until ((==) Nothing . fst) step .
    (,) (Just 0) . Map.insert 2 verb . Map.insert 1 noun

answer2 :: _ -> _
answer2 inst =
    take 1 $
    filter (\x -> 19690720 == computer x inst) $
    liftA2 (,) [0 .. 100] [0 .. 100]
