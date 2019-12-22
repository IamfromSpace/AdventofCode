module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import AdventOfCode.IntCode
       (Computer, consume, initialize, parseInsts)

import AdventOfCode.Util (aStar)
import Control.Applicative ((<|>))
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import Data.List ()
import Data.List.Split ()
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ()
import Data.Sequence ()

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)
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

discover2 ::
       (Computer, (Integer, Integer))
    -> (Maybe (Integer, Integer), [(Computer, (Integer, Integer))])
discover2 (comp, (x, y)) =
    let steps =
            [ case step N comp of
                  (Wall, _) -> (Nothing, Nothing)
                  (Success, s) -> (Nothing, Just (s, (x, y + 1)))
                  (Done, s) -> (Just (x, y + 1), Just (s, (x, y + 1)))
            , case step S comp of
                  (Wall, _) -> (Nothing, Nothing)
                  (Success, s) -> (Nothing, Just (s, (x, y - 1)))
                  (Done, s) -> (Just (x, y - 1), Just (s, (x, y - 1)))
            , case step W comp of
                  (Wall, _) -> (Nothing, Nothing)
                  (Success, s) -> (Nothing, Just (s, (x - 1, y)))
                  (Done, s) -> (Just (x - 1, y), Just (s, (x - 1, y)))
            , case step E comp of
                  (Wall, _) -> (Nothing, Nothing)
                  (Success, s) -> (Nothing, Just (s, (x + 1, y)))
                  (Done, s) -> (Just (x + 1, y), Just (s, (x + 1, y)))
            ]
        (as, bs) = unzip steps
    in (foldr (<|>) Nothing as, catMaybes bs)

explore' ::
       (Set (Integer, Integer), Maybe (Integer, Integer))
    -> [(Computer, (Integer, Integer))]
    -> (Set (Integer, Integer), (Integer, Integer))
explore' (known, mOxygen) (h:t) =
    let (mOxygen', discovered) = discover2 h
        new = filter (not . flip Set.member known . snd) discovered
        known' = known `Set.union` Set.fromList (fmap snd new)
    in explore' (known', mOxygen <|> mOxygen') (t ++ new)
explore' r [] = fmap fromJust r

explore ::
       [(Computer, (Integer, Integer))]
    -> (Set (Integer, Integer), (Integer, Integer))
explore = explore' (mempty, Nothing)

expand ::
       (Map (Integer, Integer) Bool, Set (Integer, Integer))
    -> (Map (Integer, Integer) Bool, Set (Integer, Integer))
expand (state, perimeter) =
    foldr
        (\(x, y) (s, per) ->
             foldr
                 (\p (s', per') ->
                      case Map.lookup p state of
                          Nothing -> (s', per')
                          Just True -> (s', per')
                          Just False ->
                              (Map.insert p True s', Set.insert p per'))
                 (s, per)
                 [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)])
        (state, mempty)
        perimeter

countExpand ::
       Int -> (Map (Integer, Integer) Bool, Set (Integer, Integer)) -> Int
countExpand i (state, perimeter) =
    if Map.foldr (&&) True state
        then i
        else countExpand (i + 1) (expand (state, perimeter))

answer2 :: _ -> _
answer2 insts =
    let (reachablePoints, origin) = explore [(initialize insts, (0, 0))]
        state =
            Map.insert origin True $
            foldr (flip Map.insert False) mempty $ Set.toList reachablePoints
    in countExpand 0 (state, Set.singleton origin)
