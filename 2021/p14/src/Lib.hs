module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow (arr, returnA, (&&&), (***), (<+>), (<<<), (>>>), (>>^), (|||))
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (fold, toList)
import Data.Group (invert, (~~))
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum (..))
import Data.Semigroup (Semigroup (..), stimes)
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, (++))

line = ((AP.many1 (AP.anyOf ['A' .. 'Z'])) >>! AP.string " -> ") &&& AP.anyToken

parse1 :: String -> _
parse1 s =
  let [a, b] = Split.splitOn "\n\n" s
   in (a, Map.fromList $ AP.parseImpure (AP.linesOf line) b)

parse2 :: String -> _
parse2 = parse1

step :: Map String Char -> String -> String
step rules (a : b : t) = case Map.lookup [a, b] rules of
  Nothing -> a : (step rules (b : t))
  Just c -> a : c : (step rules (b : t))
step rules x = x

freqs :: Ord a => a -> Map a Int -> Map a Int
freqs c m =
  Map.alter
    ( \case
        Nothing -> Just 1
        Just x -> Just (x + 1)
    )
    c
    m

mostLeast :: Map Char Int -> Int
mostLeast m =
  let list = List.sort $ fmap snd $ Map.toList m
   in last list - head list

answer1 :: (String, Map String Char) -> _
answer1 (init, rules) =
  mostLeast $
    foldl (flip freqs) mempty $
      Util.applyNTimes (step rules) init 10

step2 :: Map String String -> String -> String
step2 rules (a : b : t) = case Map.lookup [a, b] rules of
  Nothing -> a : (step2 rules (b : t))
  Just c -> a : (c <> (step2 rules (b : t)))
step2 rules x = x

stepRules :: Map String String -> Map String String
stepRules m =
  Map.mapWithKey (\[a, b] v -> step2 m ((a : v) <> [b])) m

newtype F a = F (a -> a)

pairs :: String -> [String]
pairs (a : b : t) = [a, b] : pairs (b : t)
pairs (a : []) = []

toMatrix :: String -> Map String Char -> _
toMatrix init rules =
  let unique = Set.toList $ Set.fromList $ fold $ Map.keys rules
      pairCounts = foldl (\m s -> freqs s m) mempty $ pairs init
      indexes = Map.fromList $ zip [1 ..] (App.liftA2 (\a b -> [a, b]) unique unique)
      d = Map.size indexes
   in ( indexes,
        Matrix.matrix d 1 (\(x, _) -> Maybe.fromMaybe 0 $ flip Map.lookup pairCounts $ Maybe.fromJust $ Map.lookup x indexes),
        Matrix.matrix
          d
          d
          ( \(y, x) -> -- Not sure why these are flipped...
              let from@[a, b] = Maybe.fromJust $ Map.lookup x indexes
                  to = Maybe.fromJust $ Map.lookup y indexes
                  c = Maybe.fromJust $ Map.lookup from rules
               in if [a, c] == to || [c, b] == to then 1 else 0
          )
      )

fastExp :: Integer -> Matrix Int -> Matrix Int
fastExp (flip divMod 2 -> (0, 1)) !m = m
fastExp (flip divMod 2 -> (n, 1)) !m =
  Matrix.multStd2 m (fastExp n (Matrix.multStd2 m m))
fastExp (flip divMod 2 -> (n, 0)) !m =
  fastExp n (Matrix.multStd2 m m)
fastExp _ _ = error "fast exponentiation error"

answer2 :: _ -> _
answer2 (init, rules) =
  let (indexes, start, matrix) = toMatrix init rules
   in mostLeast $
        Map.map (\x -> x `div` 2) $
          Map.update (Just . (+) 1) (last init) $
            Map.update (Just . (+) 1) (head init) $
              foldl
                ( \m ([a, b], v) ->
                    Map.alter
                      ( \case
                          Nothing -> Just v
                          Just x -> Just (x + v)
                      )
                      b
                      $ Map.alter
                        ( \case
                            Nothing -> Just v
                            Just x -> Just (x + v)
                        )
                        a
                        m
                )
                mempty
                $ zip (fmap (\i -> Maybe.fromJust $ Map.lookup i indexes) [1 ..]) $ concat $ Matrix.toLists $ Matrix.multStd2 (fastExp 40 matrix) start

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ 'a' `shouldBe` 'a'
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex1.txt" 2188189693529
