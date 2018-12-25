{-# LANGUAGE BinaryLiterals, ViewPatterns #-}

module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , parseArmy
    , parseDamageTypeStr
    , DamageType
    ) where

import Control.Monad.Loops (iterateUntil)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.List as List (filter, reverse, sortOn)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as Map (insert, lookup, toList)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Sequence as Seq
       (Seq((:<|)), adjust, filter, findIndicesL, fromList, index, lookup,
        mapWithIndex, reverse, sortOn)
import Data.Set as Set (Set, fromList, member, notMember, union)
import Debug.Trace (traceShow)

data DamageType
    = Radiation
    | Bludgeoning
    | Fire
    | Cold
    | Slashing
    deriving (Show, Read, Eq, Ord)

type Army
     = ( Int
       , ( Int -- hp
         , Set DamageType --immune
         , Set DamageType --weak
         , Int -- damage dealt
         , DamageType --inflicts
         , Int -- Initiative
          ))

type Parsed1 = (Seq Army, Seq Army)

type Parsed2 = Parsed1

capFirst :: String -> String
capFirst [] = []
capFirst (h:t) = toUpper h : t

parseDamageTypeStr :: String -> Set DamageType
parseDamageTypeStr s =
    let readable = capFirst <$> splitOn ", " s
    in Set.fromList $ fmap read readable

parseImmWeakStr :: String -> (Set DamageType, Set DamageType)
parseImmWeakStr s =
    case splitOn " to " <$> splitOn "; " s of
        [["immune", immStr]] -> (parseDamageTypeStr immStr, mempty)
        [["immune", immStr], ["weak", weakStr]] ->
            (parseDamageTypeStr immStr, parseDamageTypeStr weakStr)
        [["weak", weakStr], ["immune", immStr]] ->
            (parseDamageTypeStr immStr, parseDamageTypeStr weakStr)
        [["weak", weakStr]] -> (mempty, parseDamageTypeStr weakStr)
        _ -> error s

parseArmy :: String -> Army
parseArmy s =
    case splitOneOf "()" s of
        [left, middle, right] ->
            let (imm, weak) = parseImmWeakStr middle
                [units, _, _, _, hp, _, _] = words left
                [_, _, _, _, _, d, dt, _, _, _, init] = words right
            in ( read units
               , (read hp, imm, weak, read d, read $ capFirst dt, read init))
        [x] ->
            let [units, _, _, _, hp, _, _, _, _, _, _, _, d, dt, _, _, _, init] =
                    words x
            in ( read units
               , ( read hp
                 , mempty
                 , mempty
                 , read d
                 , read $ capFirst dt
                 , read init))

parse1 :: String -> Parsed1
parse1 s =
    let (immStrs, infStrs) = fmap (drop 2) $ span (/= "") $ drop 1 $ lines s
    in ( Seq.fromList $ fmap parseArmy immStrs
       , Seq.fromList $ fmap parseArmy infStrs)

-- Get properties of an Army
getInit :: Army -> Int
getInit (_, (_, _, _, _, _, i)) = i

hasUnits :: Army -> Bool
hasUnits = (> 0) . fst

effectivePower :: Army -> Int
effectivePower (units, (_, _, _, damage, _, _)) = units * damage

-- Get army Interactions
damage :: Army -> Army -> Int
damage a@(units, (_, _, _, _, dt, _)) (_, (_, immune, weak, _, _, _)) =
    let ep = effectivePower a
    in if member dt immune
           then 0
           else if member dt weak
                    then ep * 2
                    else ep

-- Targetting Phase
target :: Army -> Set Int -> Seq Army -> Maybe Int
target a untargetable bs =
    if hasUnits a
        then fmap snd $
             Seq.lookup 0 $
             Seq.reverse $
             Seq.sortOn fst $
             Seq.filter (flip notMember untargetable . snd) $
             Seq.filter ((\(d, _, _) -> d > 0) . fst) $ -- cannot target if you can't deal damage
             mapWithIndex
                 (\i b -> ((damage a b, effectivePower b, getInit b), i))
                 bs
        else Nothing

targetSeq' :: Map Int Int -> [Int] -> Seq Army -> Seq Army -> Map Int Int
targetSeq' selectedTargets [] _ _ = selectedTargets
targetSeq' selectedTargets (ai:ais) as bs =
    let alreadyTargetted = Set.fromList $ snd <$> Map.toList selectedTargets
        dead =
            Set.fromList $
            toList $
            fmap fst $ Seq.filter (not . hasUnits . snd) $ mapWithIndex (,) bs
        mNewTargetIndex =
            target
                (fromJust $ Seq.lookup ai as)
                (alreadyTargetted `Set.union` dead)
                bs
        selectedTargets' =
            case mNewTargetIndex of
                Just i -> Map.insert ai i selectedTargets
                nothing -> selectedTargets
    in targetSeq' selectedTargets' ais as bs

targetSeq :: Seq Army -> Seq Army -> Map Int Int
targetSeq as bs =
    let order =
            toList $
            fmap snd $
            Seq.reverse $
            Seq.sortOn fst $
            mapWithIndex (\i a -> ((effectivePower a, getInit a), i)) as
    in targetSeq' mempty order as bs

selectTargets :: (Seq Army, Seq Army) -> (Map Int Int, Map Int Int)
selectTargets (as, bs) = (targetSeq as bs, targetSeq bs as)

-- Attacking Phase
getOrder :: (Seq Army, Seq Army) -> [(Bool, Int)]
getOrder (as, bs) =
    let as' = toList $ mapWithIndex (\i a -> (getInit a, (True, i))) as
        bs' = toList $ mapWithIndex (\i b -> (getInit b, (False, i))) bs
    in snd <$> Prelude.reverse (List.sortOn fst (as' ++ bs'))

attack :: Army -> Army -> Army
attack a b@(units, info@(hp, _, _, _, _, _)) =
    let d = damage a b
    in (max 0 (units - (d `div` hp)), info)

-- Step through a battle
step ::
       ([(Bool, Int)], (Map Int Int, Map Int Int), (Seq Army, Seq Army))
    -> ([(Bool, Int)], (Map Int Int, Map Int Int), (Seq Army, Seq Army))
step ([], _, armies) = (getOrder armies, selectTargets armies, armies)
step ((isImmuneSystem, i):t, targets@(aTargets, bTargets), (as, bs)) =
    let attacker =
            if isImmuneSystem
                then index as i
                else index bs i
        mAttackeeIndex =
            if isImmuneSystem
                then Map.lookup i aTargets
                else Map.lookup i bTargets
    in case mAttackeeIndex of
           Just attackeeIndex ->
               if isImmuneSystem
                   then ( t
                        , targets
                        , (as, adjust (attack attacker) attackeeIndex bs))
                   else ( t
                        , targets
                        , (adjust (attack attacker) attackeeIndex as, bs))
           Nothing -> (t, targets, (as, bs))

stepN' ::
       Int
    -> ([(Bool, Int)], (Map Int Int, Map Int Int), (Seq Army, Seq Army))
    -> ([(Bool, Int)], (Map Int Int, Map Int Int), (Seq Army, Seq Army))
stepN' 0 x = x
stepN' i x = stepN' (i - 1) $ step x

--stepN :: Int -> (Seq Army, Seq Army) -> (Seq Army, Seq Army)
stepN n armies --(\(_, _, c) -> c) $ 
 = stepN' n ([], (mempty, mempty), armies)

battle ::
       State ( (Seq Army, Seq Army)
             , ([(Bool, Int)], (Map Int Int, Map Int Int), (Seq Army, Seq Army))) ( Bool
                                                                                  , ( Seq Army
                                                                                    , Seq Army))
battle = do
    (armiesAtBeginning, state) <- get
    let state'@(turns, _, armies') = step state
    if null turns && armiesAtBeginning == armies'
        then return (True, armies')
        else if null turns
                 then do
                     put (armies', state')
                     return (False, armies')
                 else do
                     put (armiesAtBeginning, state')
                     return (False, armies')

runBattle :: (Seq Army, Seq Army) -> (Bool, (Seq Army, Seq Army))
runBattle armies =
    evalState
        (iterateUntil pred battle)
        (armies, ([], (mempty, mempty), armies))
  where
    pred (isStalemate, getRemainingUnits -> (ac, bc)) =
        isStalemate || ac == 0 || bc == 0

getRemainingUnits :: (Seq Army, Seq Army) -> (Int, Int)
getRemainingUnits (as, bs) = (sum $ fst <$> as, sum $ fst <$> bs)

answer1 :: Parsed1 -> String
answer1 = show . fmap getRemainingUnits . runBattle

parse2 :: String -> Parsed2
parse2 = parse1

boostArmy :: Int -> Army -> Army
boostArmy i (units, (a, b, c, d, e, f)) = (units, (a, b, c, d + i, e, f))

boostArmies :: Int -> Seq Army -> Seq Army
boostArmies i = fmap (boostArmy i)

boostImmune :: Int -> (Seq Army, Seq Army) -> (Seq Army, Seq Army)
boostImmune i (imm, inf) = (boostArmies i imm, inf)

-- lazy manual binary search
answer2 :: Parsed2 -> String
answer2 = show . fmap getRemainingUnits . runBattle . boostImmune 59
