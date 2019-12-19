module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

import Data.Set (Set)

--import Data.Sequence (Seq)
import AdventOfCode.Util (AStarStepOption(..), aStar, elmTrace)
import Control.Applicative ((<|>))
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Char (isUpper, toUpper)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.List.Split ()
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Sum)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Dungeon
    = Wall
    | Key Char
    | Door Char
    deriving (Eq, Ord, Show)

parseDun :: (Int, Int) -> Char -> (Maybe (Int, Int), Maybe Dungeon)
parseDun _ '.' = (Nothing, Nothing)
parseDun _ '#' = (Nothing, Just Wall)
parseDun p '@' = (Just p, Nothing)
parseDun _ x =
    ( Nothing
    , Just $
      (if isUpper x
           then Door
           else Key)
          (toUpper x))

parseLine :: Int -> String -> (Maybe (Int, Int), [((Int, Int), Dungeon)])
parseLine y line =
    snd $
    foldl'
        (\(x, (mStart, lst)) char ->
             let (mStart', mDun) = parseDun (x, y) char
             in ( x + 1
                , ( mStart' <|> mStart
                  , case mDun of
                        Just d -> ((x, y), d) : lst
                        Nothing -> lst)))
        (0, (Nothing, []))
        line

parseBlock :: Int -> [String] -> _
parseBlock y (h:t) = parseLine y h : parseBlock (y + 1) t
parseBlock _ [] = []

parse1 :: String -> ((Int, Int), Map (Int, Int) Dungeon)
parse1 s =
    let (as, bs) = unzip $ parseBlock 0 $ lines s
    in (fromJust $ foldl' (<|>) Nothing as, Map.fromList $ concat bs)

parse2 :: String -> _
parse2 = parse1

type Position = ((Int, Int), Set Char)

costFn ::
       Set Char
    -> Map (Int, Int) Dungeon
    -> Position
    -> [AStarStepOption Position (Sum Int)]
costFn allKeys dMap ((x, y), keys) =
    let dir (dx, dy) =
            case Map.lookup (x, y) dMap of
                Just Wall -> Nothing
                Just (Door k) ->
                    if Set.member k keys
                        then Just ((x + dx, y + dy), keys)
                        else Nothing
                Just (Key k) -> Just ((x + dx, y + dy), Set.insert k keys)
                Nothing -> Just ((x + dx, y + dy), keys)
        up = dir (0, -1)
        down = dir (0, 1)
        left = dir (-1, 0)
        right = dir (1, 0)
    in fmap
           (\x ->
                AStarStepOption
                    x
                    (pure 1)
                    (pure (50 * (Set.size allKeys - Set.size keys)))) $
       catMaybes [up, down, left, right]

answer1 :: ((Int, Int), Map (Int, Int) Dungeon) -> _
answer1 (start, dMap) =
    let allKeys =
            Map.foldr
                (\d keys ->
                     case d of
                         Key k -> Set.insert k keys
                         _ -> keys)
                mempty
                dMap
    in (\x -> x - 2) $
       fst $ fromJust $ aStar (costFn allKeys dMap) (start, mempty)

type Position2 = (((Int, Int), (Int, Int), (Int, Int), (Int, Int)), Set Char)

getKeyCostFn ::
       Map (Int, Int) Dungeon
    -> (Int, Int)
    -> Position
    -> [AStarStepOption Position (Sum Int)]
getKeyCostFn dMap (tx, ty) ((x, y), keys) =
    let dir (dx, dy) =
            let new = (dx + x, dy + y)
            in fmap ((,) new) $
               case Map.lookup new dMap of
                   Just Wall -> Nothing
                   Just (Door k) ->
                       if Set.member k keys
                           then Just (new, keys)
                           else Nothing
                   Just (Key k) -> Just (new, Set.insert k keys)
                   Nothing -> Just (new, keys)
        up = dir (0, -1)
        down = dir (0, 1)
        left = dir (-1, 0)
        right = dir (1, 0)
    in fmap
           (\((nx, ny), x) ->
                AStarStepOption
                    x
                    (pure 1)
                    (pure (abs (nx - tx) + abs (ny - ty)))) $
       catMaybes [up, down, left, right]

getAllKeysCostFn ::
       Int
    -> Map (Int, Int) Dungeon
    -> Set Char
    -> Map Char (Int, Int)
    -> (Int, Int)
    -> Position2
    -> [AStarStepOption Position2 (Sum Int)]
getAllKeysCostFn cAdj dMap allKeys keyPositions (midX, midY) ((a, b, c, d), collected) =
    let aMk target =
            fmap
                (\(cost, (p, keys):_) ->
                     AStarStepOption
                         ((p, b, c, d), keys)
                         cost
                         (pure (cAdj * (Set.size allKeys - Set.size keys)))) $
            aStar (getKeyCostFn dMap target) (a, collected)
        bMk target =
            fmap
                (\(cost, (p, keys):_) ->
                     AStarStepOption
                         ((a, p, c, d), keys)
                         cost
                         (pure (cAdj * (Set.size allKeys - Set.size keys)))) $
            aStar (getKeyCostFn dMap target) (b, collected)
        cMk target =
            fmap
                (\(cost, (p, keys):_) ->
                     AStarStepOption
                         ((a, b, p, d), keys)
                         cost
                         (pure (cAdj * (Set.size allKeys - Set.size keys)))) $
            aStar (getKeyCostFn dMap target) (c, collected)
        dMk target =
            fmap
                (\(cost, (p, keys):_) ->
                     AStarStepOption
                         ((a, b, c, p), keys)
                         cost
                         (pure (cAdj * (Set.size allKeys - Set.size keys)))) $
            aStar (getKeyCostFn dMap target) (d, collected)
        unClaimed = foldr Map.delete keyPositions collected
        (aKeys, bKeys, cKeys, dKeys) =
            foldr
                (\(x, y) (a, b, c, d) ->
                     if x < midX && y < midY
                         then ((x, y) : a, b, c, d)
                         else if x > midX && y < midY
                                  then (a, (x, y) : b, c, d)
                                  else if x < midX && y > midY
                                           then (a, b, (x, y) : c, d)
                                           else (a, b, c, (x, y) : d))
                ([], [], [], [])
                unClaimed
    in catMaybes $
       fmap aMk aKeys ++ fmap bMk bKeys ++ fmap cMk cKeys ++ fmap dMk dKeys

answer2 :: ((Int, Int), Map (Int, Int) Dungeon) -> _
answer2 ((x, y), dMap) =
    let allKeys =
            Map.foldr
                (\d keys ->
                     case d of
                         Key k -> Set.insert k keys
                         _ -> keys)
                mempty
                dMap
        correctedDMap =
            Map.insert (x - 1, y) Wall $
            Map.insert (x + 1, y) Wall $
            Map.insert (x, y - 1) Wall $
            Map.insert (x, y + 1) Wall $ Map.insert (x, y) Wall dMap
        starts =
            ((x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1))
        keyPositions =
            Map.foldrWithKey
                (\p d kps ->
                     case d of
                         Key x -> Map.insert x p kps
                         _ -> kps)
                mempty
                correctedDMap
    in fst $
       fromJust $
       aStar
           (getAllKeysCostFn 50 correctedDMap allKeys keyPositions (x, y))
           (starts, mempty)
