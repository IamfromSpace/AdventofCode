module Lib where

import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding ((++), init, lookup, map)

type Floor = (Set String, Set String)

type Building = (Floor, Floor, Floor, Floor)

data Thing
    = Chip String
    | Gen String
    deriving (Show, Eq)

toThings :: Floor -> [Thing]
toThings (chips, gens) =
    fmap Chip (Set.toList chips) <> fmap Gen (Set.toList gens)

fromThings :: [Thing] -> Floor
fromThings =
    List.foldl'
        (\(chips, gens) x ->
             case x of
                 Chip s -> (Set.insert s chips, gens)
                 Gen s -> (chips, Set.insert s gens))
        (mempty, mempty)

myInput :: Building
myInput =
    ( (Set.fromList ["str", "plu"], Set.fromList ["str", "plu"])
    , (Set.fromList ["rut", "cur"], Set.fromList ["rut", "cur", "thu"])
    , (Set.fromList ["thu"], Set.fromList [])
    , (Set.fromList [], Set.fromList []))

simpler :: Building
simpler =
    ( (Set.fromList ["str"], Set.fromList ["str"])
    , (Set.fromList ["ele"], Set.fromList ["ele", "thu"])
    , (Set.fromList ["thu"], Set.fromList [])
    , (Set.fromList [], Set.fromList []))

exampleInput :: Building
exampleInput =
    ( (Set.fromList ["hy", "li"], Set.fromList [])
    , (Set.fromList [], Set.fromList ["hy"])
    , (Set.fromList [], Set.fromList ["li"])
    , (Set.fromList [], Set.fromList []))

parse1 :: String -> Building
parse1 = const simpler

parse2 :: String -> _
parse2 = parse1

safe' :: Floor -> Bool
safe' (chips, gens) = Set.null gens || (chips `Set.isSubsetOf` gens)

safe :: Building -> Bool
safe (a, b, c, d) = safe' a && safe' b && safe' c && safe' d

done' :: Floor -> Bool
done' (a, b) = Set.null a && Set.null b

done :: Building -> Bool
done (a, b, c, _) = done' a && done' b && done' c

type State = (Int, Building)

moveItem :: Ord a => a -> (Set a, Set a) -> Maybe (Set a, Set a)
moveItem x (a, b) =
    if Set.member x a
        then Just (Set.delete x a, Set.insert x b)
        else Nothing

removeAFromB :: Floor -> Floor -> Floor
removeAFromB (a1, a2) (b1, b2) =
    ( List.foldl' (flip Set.delete) b1 (Set.toList a1)
    , List.foldl' (flip Set.delete) b2 (Set.toList a2))

union :: Floor -> Floor -> Floor
union (a1, a2) (b1, b2) = (a1 `Set.union` b1, a2 `Set.union` b2)

twoCombos :: [a] -> [[a]]
twoCombos (h:t) = fmap (\x -> [h, x]) t <> twoCombos t
twoCombos [] = []

oneTwoCombos :: [a] -> [[a]]
oneTwoCombos xs = fmap pure xs <> twoCombos xs

options' :: Floor -> [(Floor, Floor)]
options' f =
    let inElevatorPerms = fmap fromThings $ oneTwoCombos $ toThings f
    in fmap
           (\inElevator -> (inElevator, removeAFromB inElevator f))
           inElevatorPerms

options :: State -> [State]
options (0, (a, b, c, d)) =
    fmap (\(el, a') -> (1, (a', union el b, c, d))) (options' a)
options (1, (a, b, c, d)) =
    fmap (\(el, b') -> (2, (a, b', union el c, d))) (options' b) <>
    fmap (\(el, b') -> (0, (union el a, b', c, d))) (options' b)
options (2, (a, b, c, d)) =
    fmap (\(el, c') -> (3, (a, b, c', union el d))) (options' c) <>
    fmap (\(el, c') -> (1, (a, union el b, c', d))) (options' c)
options (3, (a, b, c, d)) =
    fmap (\(el, d') -> (2, (a, b, union el c, d'))) (options' d)
options _ = error "bad floor!"

find :: State -> Int
find =
    getSum .
    fst .
    Maybe.fromJust .
    Util.aStar2
        -- Smarter lower bounds don't seem to make things much faster
        (\x ->
             if done (snd x)
                 then Sum 0
                 else Sum 1)
        (fmap (\o -> Util.AStarStepOption2 o (Sum 1)) .
         List.filter (safe . snd) . options)

-- 1:19:57 (Would have been 77 on the leaderboard)
answer1 :: _ -> _
answer1 = find . (,) 0

-- 1:23:40 (Would have been 47 on the leaderboard)
answer2 :: _ -> String
answer2 =
    const
        "Add 24 to pt1.  We know this because we just start repeating a cycle.  If we remove all the middle row parts and then start adding pairs to the bottom, each pair costs us an additional 12."

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
