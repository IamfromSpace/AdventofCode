{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , getNextStep
    , findInRangeOfPoints
    , findPointsOfType
    , Unit(..)
    , Vec
    , getTurnOrder
    , identify
    , moveOnlyGame
    , prettyPrint
    , attackingGame
    , countHealth
    , battle
    , countCompletedTurns
    ) where

import Debug.Trace (traceShow)

import Control.Applicative ((<|>))
import Control.Lens
       ((%~), (&), (.~), (?~), _1, _2, _3, _4, _5, preview, set, view)
import Control.Lens.At (at, sans)
import Control.Lens.Prism (_Just)
import Control.Monad.Loops (iterateUntil, iterateWhile)
import Control.Monad.State.Lazy (State, evalState, get, put, state)
import Data.List (sort)
import Data.Map
       (Map, delete, findMax, findMin, foldlWithKey, foldrWithKey,
        fromList, insert, insertWith, keys, lookup, minView)
import Data.Maybe
       (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Monoid (Sum(Sum), (<>))
import Data.Semigroup (Min(Min), getMin)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (lookup)

parse1' :: ElfMap () -> Vec -> String -> ElfMap ()
parse1' m p ('.':t) = parse1' m ((Sum 0, Sum 1) <> p) t
parse1' m p ('#':t) = parse1' (insert p Wall m) ((Sum 0, Sum 1) <> p) t
parse1' m p ('G':t) =
    parse1' (insert p (Occupied (Goblin ())) m) ((Sum 0, Sum 1) <> p) t
parse1' m p ('E':t) =
    parse1' (insert p (Occupied (Elf ())) m) ((Sum 0, Sum 1) <> p) t
parse1' m (y, _) ('\n':t) = parse1' m (y <> Sum 1, Sum 0) t
parse1' m _ [] = m

parse1 :: String -> ElfMap ()
parse1 = parse1' mempty mempty

data Unit a
    = Goblin a
    | Elf a
    deriving (Eq, Ord, Show)

instance Functor Unit where
    fmap f (Goblin a) = Goblin (f a)
    fmap f (Elf a) = Elf (f a)

data MapPoint a
    = Wall
    | Occupied (Unit a)
    deriving (Eq, Ord, Show)

instance Functor MapPoint where
    fmap f Wall = Wall
    fmap f (Occupied a) = Occupied (fmap f a)

type Vec = (Sum Int, Sum Int) -- Y is FIRST

type ElfMap a = Map Vec (MapPoint a)

type Game a
     = ( ElfMap Int -- Points to walls/unit ids
       , Map (Unit Int) (Vec, a) -- elf/goblin stats and position by id
       , ([Unit Int], Int) -- turn stack (isElf, id), and how many turns have passed
        )

move' :: Unit Int -> Vec -> Game a -> Game a
move' i by game =
    case preview (_2 . at i . _Just . _1) game of
        Nothing -> game
        Just p ->
            game & (_1 . at p .~ Nothing) & -- delete the old position on the elfmap
            (_1 . at (p <> by) .~ Just (Occupied i)) & -- add the new position on the elfmap
            (_2 . at i . _Just . _1 %~ (<>) by) -- update the position on the unit map

move :: Unit Int -> Vec -> Game a -> Game a
move i by game@(elfMap, unitMap, turnStack) =
    case lookup i unitMap of
        Just (p, a) ->
            ( delete p $ insert (p <> by) (Occupied i) elfMap
            , insert i (p <> by, a) unitMap
            , turnStack)
        Nothing -> game

findPointsOfType :: Unit b -> ElfMap a -> Set Vec
findPointsOfType unit =
    foldrWithKey
        (\k v s ->
             case (unit, v) of
                 (Goblin _, Occupied (Goblin _)) -> Set.insert k s
                 (Elf _, Occupied (Elf _)) -> Set.insert k s
                 _ -> s)
        mempty

getSurrounding :: Vec -> Set Vec
getSurrounding p =
    Set.mapMonotonic (<> p) $
    Set.fromList
        [(Sum 0, Sum (-1)), (Sum 0, Sum 1), (Sum (-1), Sum 0), (Sum 1, Sum 0)]

isOpenSpace :: ElfMap a -> Vec -> Bool
isOpenSpace elfMap = isNothing . flip lookup elfMap

findInRangeOfPoints :: ElfMap a -> Set Vec -> Set Vec
findInRangeOfPoints elfMap =
    foldMap (Set.filter (isOpenSpace elfMap) . getSurrounding)

findGoalPoints :: Unit a -> ElfMap b -> Set Vec
findGoalPoints unit elfMap =
    findInRangeOfPoints elfMap $ findPointsOfType unit elfMap

-- Our PriorityQueue is a set of, well, just that, priorities!
-- in priority order, then finally the thing of interest.
-- if we always pull the minimum element, as soon as a goal
-- state is found, we know we're done, because nothing else
-- had a higher priority.
type PriorityQueue
     = Set ( Int -- expected distance from origin to nearest goal
           , Vec -- nearest goal point
           , Maybe Vec -- initial step on this path (the very first point doesn't have one)
           , Int -- _negative_ steps already taken
           , Vec -- current point
            )

dist :: Vec -> Vec -> Int
dist (Sum y0, Sum x0) (Sum y1, Sum x1) = abs (y1 - y0) + abs (x1 - x0)

invert :: Vec -> Vec
invert (Sum y, Sum x) = (Sum (-y), Sum (-x))

-- O(n) which is probably the weakest point of this algorithm
nearest :: Vec -> Set Vec -> (Int, Vec)
nearest p = getMin . foldMap (\gp -> Min (dist p gp, gp))

-- State _ Nothing means the search is still running
-- State _ (Just Nothing) means it is impossibe to reach any goals
-- State _ (Just (Just Vec)) means the first step in the optimal path is the Vec
searchStep ::
       ElfMap a
    -> Set Vec
    -> State (Map Vec (Int, Vec), PriorityQueue) (Maybe (Maybe Vec))
searchStep elfMap goalPs = do
    (seen, priorityQueue) <- get
    case Set.minView priorityQueue of
        Just (next, priorityQueue') -> do
            let getInitialStep p =
                    fromJust
                        (view _3 next <|> Just (p <> invert (view _5 next)))
            let stepsTaken = view _4 next - 1 -- remember, these are negative
            let toExplore =
                    Set.filter
                        (\p ->
                             case lookup p seen of
                                 Nothing -> True
                                 Just x -> (-stepsTaken, getInitialStep p) < x) $
                    Set.filter (isOpenSpace elfMap) $
                    getSurrounding (view _5 next)
            let seen' =
                    Set.foldr
                        (\p -> insertWith min p (-stepsTaken, getInitialStep p))
                        seen
                        toExplore
            let nextPriorities =
                    Set.map
                        (\p ->
                             let (d, gp) = nearest p goalPs
                             in ( d - stepsTaken
                                , gp
                                , Just (getInitialStep p)
                                , stepsTaken
                                , p))
                        toExplore
            case Set.minView $
                 Set.filter
                     (\(expected, _, _, taken, _) -> expected == -taken)
                     nextPriorities of
                Just ((_, _, Just step, _, _), _) -> return $ Just (Just step)
                Nothing -> do
                    put (seen', priorityQueue' <> nextPriorities)
                    return Nothing
                _ -> error "The initial step was Nothing at termination"
        Nothing -> return $ Just Nothing

getNextStep :: ElfMap a -> Set Vec -> Vec -> Maybe Vec
getNextStep elfMap goalPs pInit =
    if Set.member pInit goalPs
        then Nothing
        else fromJust $
             evalState
                 (iterateUntil isJust (searchStep elfMap goalPs))
                 (mempty, Set.singleton (0, pInit, Nothing, 0, pInit))

identify' :: a -> ElfMap b -> Game a
identify' baseStats =
    fst .
    foldrWithKey
        (\p v s@((elfMap', unitMap, turnOrder), (gId, eId)) ->
             case v of
                 Wall -> s & _1 . _1 . at p ?~ Wall
                 Occupied (Goblin _) ->
                     let gId = view (_2 . _1) s
                     in s &
                        -- insert unit on map
                        _1 .
                        _1 .
                        at p ?~
                        Occupied (Goblin gId) &
                         -- insert stats into units
                        _1 .
                        _2 .
                        at (Goblin gId) ?~
                        (p, baseStats) &
                        -- increment id
                        _2 .
                        _1 %~
                        (+ 1)
                 Occupied (Elf _) ->
                     let eId = view (_2 . _2) s
                     in s &
                        -- insert unit on map
                        _1 .
                        _1 .
                        at p ?~
                        Occupied (Elf eId) &
                         -- insert stats into units
                        _1 .
                        _2 .
                        at (Elf eId) ?~
                        (p, baseStats) &
                        -- increment id
                        _2 .
                        _2 %~
                        (+ 1))
        ((mempty, mempty, (mempty, 0)), (0, 0))

identify :: a -> ElfMap b -> Game a
identify baseStats =
    fst .
    foldlWithKey
        (\((elfMap', unitMap, turnOrder), (gId, eId)) p v ->
             case v of
                 Wall ->
                     ((insert p Wall elfMap', unitMap, turnOrder), (gId, eId))
                 Occupied (Goblin _) ->
                     ( ( insert p (Occupied (Goblin gId)) elfMap'
                       , insert (Goblin gId) (p, baseStats) unitMap
                       , turnOrder)
                     , (gId + 1, eId))
                 Occupied (Elf _) ->
                     ( ( insert p (Occupied (Elf eId)) elfMap'
                       , insert (Elf eId) (p, baseStats) unitMap
                       , turnOrder)
                     , (gId, eId + 1)))
        ((mempty, mempty, (mempty, -1)), (0, 0))

getTurnOrder :: ElfMap a -> [Unit a]
getTurnOrder =
    foldMap
        (\case
             Occupied x -> [x]
             _ -> [])

switchSides :: Unit a -> Unit a
switchSides (Goblin x) = Elf x
switchSides (Elf x) = Goblin x

getEnemy :: ElfMap a -> Unit a -> Vec -> Maybe (Unit a)
getEnemy elfMap unit p =
    case (unit, lookup p elfMap) of
        (Elf _, Just (Occupied (Goblin x))) -> Just $ Goblin x
        (Goblin _, Just (Occupied (Elf x))) -> Just $ Elf x
        _ -> Nothing

getStats :: Unit a -> a
getStats (Goblin x) = x
getStats (Elf x) = x

takeDamage :: Unit Int -> Game Int -> Game Int
takeDamage unitId (elfMap, unitMap, turnOrder) =
    let (p, hp) =
            fromMaybe (error "Damaging a unit that does not exist!") $
            lookup unitId unitMap
        d =
            case unitId of
                Elf _ -> 3
                Goblin _ -> 12
    in if hp - d <= 0
           then (delete p elfMap, delete unitId unitMap, turnOrder)
           else (elfMap, insert unitId (p, hp - d) unitMap, turnOrder)

-- TODO: If needed, this could be generalized for 'Game a'
-- with a 'getPriority' function (Ord b => a -> b)
getAttackee :: Unit Int -> Game Int -> Maybe (Unit Int)
getAttackee unitId (elfMap, unitMap, turnOrder) =
    let p =
            fst $
            fromMaybe (error "cannot attack with a unitId that does not exist!") $
            lookup unitId unitMap
        idToPriority e =
            let (p, hp) = fromJust $ lookup e unitMap
            in (hp, p)
        potentialAttakees =
            fromList $ catMaybes $
            fmap (fmap (\e -> (idToPriority e, e)) . getEnemy elfMap unitId) $
            Set.toList $
            getSurrounding p
    in fst <$> minView potentialAttakees

-- TODO: is this just a function...?
moveOnlyGame :: State (Game a) (Game a)
moveOnlyGame = do
    (elfMap, unitMap, turnOrder) <- get
    case turnOrder of
        ([], i) -> do
            let game = (elfMap, unitMap, (getTurnOrder elfMap, i + 1))
            put game
            return game
        ((unit:t), i) -> do
            let p =
                    fst $
                    fromMaybe
                        (error
                             -- TODO: this may signal that the unit has died
                             "Invariant Error: Turn taking units must always be in the unitMap!") $
                    lookup unit unitMap
            let goalPoints =
                    findGoalPoints
                        (switchSides unit) -- around the opposite type
                        (delete p elfMap) -- consider this unit's square empty
            case getNextStep elfMap goalPoints p of
                Just nextStep -> do
                    let game = move unit nextStep (elfMap, unitMap, (t, i))
                    put game
                    return game
                Nothing -> do
                    let game = (elfMap, unitMap, (t, i))
                    put game
                    return game

attackingGame :: Game Int -> Game Int
attackingGame (elfMap, unitMap, turnOrder) =
    case turnOrder of
        ([], i) -> (elfMap, unitMap, (getTurnOrder elfMap, i + 1))
        ((unitId:t), i) ->
            case lookup unitId unitMap of
                Nothing ->
                    (elfMap, unitMap, (t, i)) -- the unit is dead, and cannot act
                Just (p, _) ->
                    let goalPoints =
                            findGoalPoints
                                (switchSides unitId) -- around the opposite type
                                (delete p elfMap) -- consider this unitId's square empty
                        game' =
                            case getNextStep elfMap goalPoints p of
                                Just nextStep ->
                                    move
                                        unitId
                                        nextStep
                                        (elfMap, unitMap, (t, i))
                                -- the unit will not move, but may still attack
                                Nothing -> (elfMap, unitMap, (t, i))
                    in maybe game' (`takeDamage` game') $
                       getAttackee unitId game'

countHealth :: Map (Unit Int) (a, Int) -> (Int, Int)
countHealth =
    foldrWithKey
        (\u (_, hp) (e, g) ->
             case u of
                 Goblin _ -> (e, g + hp)
                 Elf _ -> (e + hp, g))
        (0, 0)

basicState :: (a -> a) -> State a a
basicState f =
    let f' a' =
            let a = f a'
            in (a, a)
    in state f'

battle :: Game Int -> Game Int
battle =
    evalState
        (iterateWhile
             (\(_, unitMap, _) ->
                  let (e, g) = countHealth unitMap
                  in e > 0 && g > 0)
             (basicState attackingGame))

prettyPrint :: ElfMap a -> String
prettyPrint elfMap =
    let ((Sum miny, Sum minx), _) = findMin elfMap
        ((Sum maxy, Sum maxx), _) = findMax elfMap
    in unlines $
       fmap
           (\y ->
                fmap
                    (\x ->
                         case lookup (Sum y, Sum x) elfMap of
                             Just Wall -> '#'
                             Just (Occupied (Goblin _)) -> 'G'
                             Just (Occupied (Elf _)) -> 'E'
                             Nothing -> '.')
                    [minx .. maxx])
           [miny .. maxy]

isSameTeam :: Unit a -> Unit b -> Bool
isSameTeam (Goblin _) (Goblin _) = True
isSameTeam (Elf _) (Elf _) = True
isSameTeam _ _ = False

countCompletedTurns :: Unit a -> ([Unit b], Int) -> Int
countCompletedTurns unit (turns, completedTurns) =
    if all (isSameTeam unit) turns
        then completedTurns + 1
        else completedTurns

answer1 :: ElfMap () -> Int
answer1 elfMap =
    let (_, countHealth -> (e, g), turnData) = battle $ identify 200 elfMap
        completedTurns =
            countCompletedTurns
                (if e > g
                     then Goblin ()
                     else Elf ())
                turnData
    in (e + g) * completedTurns

parse2 :: String -> ElfMap ()
parse2 = parse1

-- lazy, did this with a by hand binary search
answer2 :: ElfMap () -> (Int, Int, Int)
answer2 elfMap =
    let game@(_, unitMap, _) = identify 200 elfMap
        (elfMap', unitMap', turnData) = battle $ game
        (e, g) = countHealth unitMap'
        completedTurns =
            countCompletedTurns
                (if e > g
                     then Goblin ()
                     else Elf ())
                turnData
        countElves = length . filter (isSameTeam (Elf ())) . keys
    in (countElves unitMap, countElves unitMap', (e + g) * completedTurns)
