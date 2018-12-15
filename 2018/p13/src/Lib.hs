module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Debug.Trace (traceShow)

import Data.List (foldl')
import qualified Data.Map as M
       (Map, delete, fromList, insert, lookup, toList, union)
import qualified Data.Set as S
       (Set, delete, findMin, fromList, insert, map, member, size, toList,
        toList, union)

data Turn
    = L
    | Straight
    | R
    deriving (Show, Eq, Ord)

data Direction
    = North
    | South
    | East
    | West
    deriving (Show, Eq, Ord)

data TrackSection
    = Intersection
    | NorthToEast
    | NorthToWest
    | EastWest
    | NorthSouth
    deriving (Show, Eq, Ord)

type Point = (Int, Int)

type Cart = (Point, (Turn, Direction))

type Track = M.Map Point TrackSection

type Parsed1 = (S.Set Cart, Track)

parseChar :: Char -> Maybe (Either (Turn, Direction) TrackSection)
parseChar '^' = Just $ Left (L, North)
parseChar '>' = Just $ Left (L, East)
parseChar 'v' = Just $ Left (L, South)
parseChar '<' = Just $ Left (L, West)
parseChar '/' = Just $ Right NorthToEast
parseChar '\\' = Just $ Right NorthToWest
parseChar '|' = Just $ Right NorthSouth
parseChar '-' = Just $ Right EastWest
parseChar '+' = Just $ Right Intersection
parseChar ' ' = Nothing

parseLine :: Int -> String -> Parsed1
parseLine y =
    (\(a, b, _) -> (a, b)) .
    foldl'
        (\(carts, track, x) c ->
             case parseChar c of
                 Just (Left cart@(_, d)) ->
                     ( S.insert ((x, y), cart) carts
                     , case d of
                           North -> M.insert (x, y) NorthSouth track
                           South -> M.insert (x, y) NorthSouth track
                           _ -> M.insert (x, y) EastWest track
                     , x + 1)
                 Just (Right trackSection) ->
                     (carts, M.insert (x, y) trackSection track, x + 1)
                 Nothing -> (carts, track, x + 1))
        (mempty, mempty, 0)

parse1 :: String -> Parsed1
parse1 =
    (\(a, b, _) -> (a, b)) .
    foldl'
        (\(carts, track, y) line ->
             let (carts', track') = parseLine y line
             in (carts' `S.union` carts, track' `M.union` track, y + 1))
        (mempty, mempty, 0) .
    lines

parse2 :: String -> Parsed1
parse2 = parse1

turn :: Turn -> Direction -> (Turn, Direction)
turn L North = (Straight, West)
turn L East = (Straight, North)
turn L South = (Straight, East)
turn L West = (Straight, South)
turn R North = (L, East)
turn R East = (L, South)
turn R South = (L, West)
turn R West = (L, North)
turn Straight x = (R, x)

cornerNorthToEast :: Direction -> Direction
cornerNorthToEast North = East
cornerNorthToEast East = North
cornerNorthToEast South = West
cornerNorthToEast West = South

cornerNorthToWest :: Direction -> Direction
cornerNorthToWest North = West
cornerNorthToWest East = South
cornerNorthToWest South = East
cornerNorthToWest West = North

tickCart :: Track -> Cart -> Cart
tickCart track ((x, y), (t, d)) =
    let p' =
            case d of
                North -> (x, y - 1)
                South -> (x, y + 1)
                East -> (x + 1, y)
                West -> (x - 1, y)
        state =
            case M.lookup p' track of
                Just Intersection -> turn t d
                Just NorthToEast -> (t, cornerNorthToEast d)
                Just NorthToWest -> (t, cornerNorthToWest d)
                Just _ -> (t, d)
                Nothing ->
                    error
                        ("A cart fell off the track! from " ++
                         show (x, y) ++ " to " ++ show p')
    in (p', state)

tickCarts :: Track -> S.Set Cart -> Either (Int, Int) (S.Set Cart)
tickCarts track carts =
    fmap snd $
    foldl'
        (\e cart ->
             e >>= \(unmoved, s) ->
                 let cart' = tickCart track cart
                     unmoved' = S.delete cart' unmoved
                     s' = S.insert cart' s
                 in if S.member (fst cart') $
                       S.map fst s `S.union` S.map fst unmoved'
                        then Left $ fst cart'
                        else Right (unmoved', s'))
        (Right (carts, mempty)) $
    S.toList carts

tickUntilCollision :: Int -> Track -> S.Set Cart -> (Int, Int)
tickUntilCollision i track carts =
    if i > 10000
        then error "too many iterations!"
        else let cartsOrPoint = tickCarts track carts
             in case cartsOrPoint of
                    Left p -> traceShow i p
                    Right carts' -> tickUntilCollision (i + 1) track carts'

{-
-- MUST be ordered!
findCollision' :: Cart -> [Cart] -> Maybe (Int, Int)
findCollision' last@((x, y), _) (this@((x', y'), _):t) =
    if x == x' && y == y'
        then Just (x, y)
        else findCollision' this t
findCollision' _ [] = Nothing

findCollision :: S.Set Cart -> Maybe (Int, Int)
findCollision s =
    let (h:t) = S.toList s
    in findCollision' h t

tickUntilCollision :: Int -> Track -> S.Set Cart -> (Int, Int)
tickUntilCollision i track carts =
    if i > 10000
        then error "too many iterations!"
        else let carts' = tickCarts track carts
             in case findCollision carts' of
                    Just p -> p
                    Nothing -> tickUntilCollision (i + 1) track carts'

tickCartsN :: Int -> Track -> S.Set Cart -> S.Set Cart
tickCartsN 0 track carts = carts
tickCartsN i track carts = tickCartsN (i - 1) track $ tickCarts track carts
-}
answer1 :: Parsed1 -> String
answer1 = show . uncurry (flip (tickUntilCollision 0))

tickCarts2 :: Track -> S.Set Cart -> S.Set Cart
tickCarts2 track carts =
    S.fromList $
    M.toList $
    (\(_, x, _) -> x) $
    foldl'
        (\(unmoved, m, deleted) cart ->
             if S.member (fst cart) deleted
                 then (unmoved, m, deleted)
                 else let (p', c) = tickCart track cart
                          unmoved' = M.delete (fst cart) unmoved
                      in case M.lookup p' (m `M.union` unmoved') of
                             Just _ ->
                                 (unmoved', M.delete p' m, S.insert p' deleted)
                             Nothing -> (unmoved', M.insert p' c m, deleted))
        (M.fromList $ S.toList carts, mempty, mempty) $
    S.toList carts

finalCart :: Int -> Track -> S.Set Cart -> Cart
finalCart i track carts =
    if i > 10000000
        then error "too many iterations!"
        else let carts' = tickCarts2 track carts
             in if S.size carts' <= 1
                    then traceShow i $ S.findMin carts'
                    else finalCart (i + 1) track carts'

answer2 :: Parsed1 -> String
answer2 = show . fst . uncurry (flip (finalCart 0))
