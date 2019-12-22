module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

import AdventOfCode.Util
       (AStarStepOption(..), aStar, elmTrace, parseGrid)
import Control.Applicative ((<|>))
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split ()
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Sum(..))
import Data.Sequence
import Data.Set (Set)
import qualified Data.Set as Set

parse1 :: String -> (Set (Int, Int), Map (Int, Int) Char)
parse1 str =
    let map =
            parseGrid
                (\p c s ->
                     case c of
                         '.' -> Set.insert p s
                         _ -> s)
                mempty
                str
        portals =
            parseGrid
                (\p c m ->
                     case c of
                         '#' -> m
                         '.' -> m
                         ' ' -> m
                         x -> Map.insert p x m)
                mempty
                str
    in (map, portals)

parse2 :: String -> _
parse2 = parse1

pointToName :: Set (Int, Int) -> Map (Int, Int) Char -> Map (Int, Int) String
pointToName s cMap =
    Map.foldrWithKey
        (\(x, y) c m ->
             let mOtherSide =
                     case Map.lookup (x, y + 1) cMap of
                         Just c2 ->
                             Just
                                 ( if Set.member (x, y + 2) s
                                       then (x, y + 2)
                                       else (x, y - 1)
                                 , [c, c2])
                         Nothing ->
                             case Map.lookup (x + 1, y) cMap of
                                 Just c2 ->
                                     Just
                                         ( if Set.member (x + 2, y) s
                                               then (x + 2, y)
                                               else (x - 1, y)
                                         , [c, c2])
                                 Nothing -> Nothing
             in case mOtherSide of
                    Nothing -> m
                    Just (p, name) -> Map.insert p name m)
        mempty
        cMap

mkHelpers :: Map (Int, Int) String -> _
mkHelpers pointToName =
    let nameToPoints =
            Map.foldrWithKey
                (\p ->
                     Map.alter
                         (\case
                              Nothing -> Just [p]
                              Just x -> Just (p : x)))
                mempty
                pointToName
        portals =
            Map.foldr
                (\ps m ->
                     case ps of
                         [a, b] -> Map.insert b a $ Map.insert a b m
                         _ -> m)
                mempty
                nameToPoints
        portalPairs =
            Map.foldr
                (\ps lst ->
                     case ps of
                         [a, b] -> (a, b) : lst
                         _ -> lst)
                mempty
                nameToPoints
        start = head $ fromJust $ Map.lookup "AA" nameToPoints
        end = head $ fromJust $ Map.lookup "ZZ" nameToPoints
    in (portals, portalPairs, start, end)

manDist :: (Int, Int) -> (Int, Int) -> Int
manDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

bestCase :: [((Int, Int), (Int, Int))] -> (Int, Int) -> (Int, Int) -> Int
bestCase portalPairs a b =
    minimum
        (manDist a b :
         concatMap
             (\(pa, pb) ->
                  [manDist a pa + manDist pb b, manDist a pb + manDist pa b])
             portalPairs)

costFn ::
       [((Int, Int), (Int, Int))]
    -> Map (Int, Int) (Int, Int)
    -> Set (Int, Int)
    -> (Int, Int)
    -> (Int, Int)
    -> [AStarStepOption (Int, Int) (Sum Int)]
costFn portalPairs portals pathway target (x, y) =
    let step p =
            let dest =
                    if p == (x, y)
                        then Map.lookup p portals
                        else if Set.member p pathway
                                 then Just p
                                 else Nothing
                minRemCost =
                    fmap
                        (\d ->
                             if d == target
                                 then Sum 0
                                 else Sum (bestCase portalPairs d target))
                        dest
            in AStarStepOption <$> dest <*> pure (Sum 1) <*> minRemCost
    in catMaybes
           [ step (x, y)
           , step (x + 1, y)
           , step (x - 1, y)
           , step (x, y + 1)
           , step (x, y - 1)
           ]

answer1 :: _ -> _
answer1 (pathways, p) =
    let (portals, portalPairs, start, end) = mkHelpers (pointToName pathways p)
    in aStar (costFn portalPairs portals pathways end) start

split :: (Int, Int) -> Map (Int, Int) String -> Map (String, Bool) (Int, Int)
split (width, height) =
    Map.foldrWithKey
        (\(x, y) str m ->
             Map.insert
                 ( str
                 , x == 2 || y == 2 || x == (width + 2) || y == (height + 2))
                 (x, y)
                 m)
        mempty

costFn2 ::
       Set (Int, Int)
    -> (Int, Int)
    -> (Int, Int)
    -> [AStarStepOption (Int, Int) (Sum Int)]
costFn2 pathway target (x, y) =
    let step p =
            let dest =
                    if Set.member p pathway
                        then Just p
                        else Nothing
                minRemCost =
                    fmap
                        (\p' ->
                             if p' == target
                                 then Sum 0
                                 else Sum (manDist target p'))
                        dest
            in AStarStepOption <$> dest <*> pure (Sum 1) <*> minRemCost
    in catMaybes
           [step (x + 1, y), step (x - 1, y), step (x, y + 1), step (x, y - 1)]

innerOuterDist ::
       Set (Int, Int)
    -> Map (String, Bool) (Int, Int)
    -> Map (String, Bool) (Map (String, Bool) (Sum Int))
innerOuterDist pathway splitMap =
    Map.foldrWithKey
        (\fromK fromP m ->
             Map.insert
                 fromK
                 (Map.foldrWithKey
                      (\toK toP m' ->
                           if fromP /= toP
                               then case fst <$>
                                         aStar (costFn2 pathway fromP) toP of
                                        Nothing -> m'
                                        Just d -> Map.insert toK d m'
                               else m')
                      mempty
                      splitMap)
                 m)
        mempty
        splitMap

costFn3 ::
       Int
    -> Map (String, Bool) (Map (String, Bool) (Sum Int))
    -> ((String, Bool), Int)
    -> [AStarStepOption ((String, Bool), Int) (Sum Int)]
costFn3 costAdj hopCosts ((portalName, isOuter), d) =
    catMaybes $
    fmap
        (\((portalName', isOuter'), cost) ->
             if isOuter' && d == 0
                 then if portalName' == "ZZ"
                          then Just
                                   (AStarStepOption
                                        ((portalName', isOuter'), d)
                                        cost
                                        (pure 0))
                          else Nothing
                 else if portalName' == "AA" || portalName' == "ZZ"
                          then Nothing
                          else let d' =
                                       d +
                                       if isOuter'
                                           then (-1)
                                           else 1
                               in Just $
                                  AStarStepOption
                                      ((portalName', not isOuter'), d')
                                      (cost <> pure 1)
                                      (pure
                                           ((d' +
                                             if isOuter
                                                 then 0
                                                 else 1) *
                                            costAdj +
                                            1))) $
    Map.toList $ fromJust $ Map.lookup (portalName, isOuter) hopCosts

answer2 :: _ -> _
answer2 (pathways, p) =
    let p2n = pointToName pathways p
        (portals, _, start, end) = mkHelpers p2n
        splitMap = split (124, 118) p2n
        hopCosts =
            innerOuterDist
                pathways
                (Map.insert ("ZZ", True) end $
                 Map.insert ("AA", True) start splitMap)
    in aStar (costFn3 32 hopCosts) (("AA", True), 0)
