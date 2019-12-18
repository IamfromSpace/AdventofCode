module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import AdventOfCode.IntCode
       (Computer, consume, initialize, parseInsts)

import AdventOfCode.Util (aStar)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import Data.List ()
import Data.List.Split ()
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

parse1 :: String -> _
parse1 = parseInsts

data Dir
    = N
    | S
    | E
    | W
    deriving (Show)

parse2 :: String -> _
parse2 = parse1

dirToCmd :: Dir -> Integer
dirToCmd N = 1
dirToCmd S = 2
dirToCmd E = 4
dirToCmd W = 3

data StepResult
    = Wall
    | Success
    | Done

step :: Dir -> Computer -> (StepResult, Computer)
step dir s =
    case consume (s, pure $ dirToCmd dir) of
        (s, (_, head . toList -> 0)) -> (Wall, s)
        (s, (_, head . toList -> 1)) -> (Success, s)
        (s, (_, head . toList -> 2)) -> (Done, s)
        _ -> error "Bad output!"

discover ::
       (Computer, (Integer, Integer), Integer)
    -> Either Integer [(Computer, (Integer, Integer), Integer)]
discover (comp, (x, y), (+ 1) -> i) =
    let steps =
            [ case step N comp of
                  (Wall, _) -> Left (Left (x, y + 1))
                  (Success, s) -> Left (Right (s, (x, y + 1), i))
                  (Done, _) -> Right (x, y + 1)
            , case step S comp of
                  (Wall, _) -> Left (Left (x, y - 1))
                  (Success, s) -> Left (Right (s, (x, y - 1), i))
                  (Done, _) -> Right (x, y - 1)
            , case step W comp of
                  (Wall, _) -> Left (Left (x - 1, y))
                  (Success, s) -> Left (Right (s, (x - 1, y), i))
                  (Done, _) -> Right (x - 1, y)
            , case step E comp of
                  (Wall, _) -> Left (Left (x + 1, y))
                  (Success, s) -> Left (Right (s, (x + 1, y), i))
                  (Done, _) -> Right (x + 1, y)
            ]
        mTarget =
            case rights steps of
                [] -> Nothing
                [p] -> Just p
                _ -> error "Two correct answers!"
    in case mTarget of
           Just x -> Left i
           Nothing -> Right $ rights $ lefts steps

search ::
       Set (Integer, Integer)
    -> [(Computer, (Integer, Integer), Integer)]
    -> Integer
search known (h:t) =
    case discover h of
        Left i -> i
        Right xs ->
            let new = filter (\(_, p, _) -> not $ Set.member p known) xs
                known' =
                    known `Set.union`
                    (Set.fromList $ fmap (\(_, p, _) -> p) new)
            in search known' (t ++ new)
search _ [] = error "queue exhausted!"

-- 52:43
answer1 :: _ -> _
answer1 insts = search mempty [(initialize insts, (0, 0), 0)]

answer2 :: _ -> _
answer2 = id
