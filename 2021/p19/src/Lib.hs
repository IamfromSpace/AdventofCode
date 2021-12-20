module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (Vector (..), multiLines)
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
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum (..))
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, (++))

type V3 = Vector (Integer, Integer, Integer)

scanner :: [String] -> Set V3
scanner (_ : t) = Set.fromList $ fmap (\line -> maybe (error line) Vector $ readMaybe ("(" <> line <> ")")) t
scanner [] = error "bad"

parse1 :: String -> [Set V3]
parse1 = fmap scanner . multiLines

parse2 :: String -> _
parse2 = parse1

face :: Int -> V3 -> V3
face 0 (Vector (x, y, z)) = Vector (x, y, z)
face 1 (Vector (x, y, z)) = Vector (- y, x, z)
face 2 (Vector (x, y, z)) = Vector (- x, - y, z)
face 3 (Vector (x, y, z)) = Vector (y, - x, z)
face 4 (Vector (x, y, z)) = Vector (- z, y, x)
face 5 (Vector (x, y, z)) = Vector (z, y, - x)
face _ _ = error "faced"

orient :: Int -> V3 -> V3
orient 0 (Vector (x, y, z)) = Vector (x, y, z)
orient 1 (Vector (x, y, z)) = Vector (x, z, - y)
orient 2 (Vector (x, y, z)) = Vector (x, - y, - z)
orient 3 (Vector (x, y, z)) = Vector (x, - z, y)
orient _ _ = error "oriented"

align :: Int -> Int -> V3 -> V3
align f o v = orient o $ face f v

allAlignments :: [V3 -> V3]
allAlignments = App.liftA2 align [0 .. 5] [0 .. 3]

checkMerge :: Set V3 -> Set V3 -> V3 -> Bool
checkMerge a b offset =
  let common = a `Set.intersection` Set.map ((<>) offset) b
   in if Set.size common == 0
        then error "didn't align"
        else (length $ take 12 $ Set.toList common) >= 12 -- lazier

merge :: Set V3 -> Set V3 -> Maybe V3
merge a b =
  case take 1 $ filter (checkMerge a b) $ App.liftA2 (\to from -> to ~~ from) (Set.toList a) (Set.toList b) of
    [h] -> Just h
    _ -> Nothing

findMerge' :: Set V3 -> Set V3 -> (V3, V3 -> V3)
findMerge' a b =
  Maybe.fromJust $ findMerge a b

findMerge :: Set V3 -> Set V3 -> Maybe (V3, V3 -> V3)
findMerge a b =
  case Maybe.catMaybes $ fmap (\f -> fmap (\x -> (x, f)) $ merge a $ Set.map f b) allAlignments of
    (h : _) -> Just h
    _ -> Nothing

options :: [a] -> [a] -> [(a, [a])]
options _ [] = []
options prev (h : t) = (h, prev <> t) : options (h : prev) t

allProbes :: [Set V3] -> Set V3
allProbes [a] = a
allProbes (a : t) = do
  case Maybe.catMaybes $ fmap (\(b, others) -> fmap (\x -> (x, b, others)) $ findMerge a b) $ options [] t of
    (((o, f), b, t') : _) -> allProbes ((a <> Set.map ((<>) o) (Set.map f b)) : t')

answer1 :: _ -> _
answer1 = length . allProbes

allProbesVs :: [Set V3] -> [V3]
allProbesVs [a] = [mempty]
allProbesVs (a : t) = do
  case Maybe.catMaybes $ fmap (\(b, others) -> fmap (\x -> (x, b, others)) $ findMerge a b) $ options [] t of
    (((o, f), b, t') : _) -> o : allProbesVs ((a <> Set.map ((<>) o) (Set.map f b)) : t')

answer2 :: _ -> _
answer2 pds =
  let ps = allProbesVs pds
      allP2P = App.liftA2 (~~) ps ps
   in maximum $ fmap Util.manLen allP2P

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

firstMergedEx1 = "Junk\n-618,-824,-621\n-537,-823,-458\n-447,-329,318\n404,-588,-901\n544,-627,-890\n528,-643,409\n-661,-816,-575\n390,-675,-793\n423,-701,434\n-345,-311,381\n459,-707,401\n-485,-357,347"

firstMergedEx2 = "Just\n459,-707,401\n-739,-1745,668\n-485,-357,347\n432,-2009,850\n528,-643,409\n423,-701,434\n-345,-311,381\n408,-1815,803\n534,-1912,768\n-687,-1600,576\n-447,-329,318\n-635,-1737,486"

allProbesEx1 = "Junk\n-892,524,684\n-876,649,763\n-838,591,734\n-789,900,-551\n-739,-1745,668\n-706,-3180,-659\n-697,-3072,-689\n-689,845,-530\n-687,-1600,576\n-661,-816,-575\n-654,-3158,-753\n-635,-1737,486\n-631,-672,1502\n-624,-1620,1868\n-620,-3212,371\n-618,-824,-621\n-612,-1695,1788\n-601,-1648,-643\n-584,868,-557\n-537,-823,-458\n-532,-1715,1894\n-518,-1681,-600\n-499,-1607,-770\n-485,-357,347\n-470,-3283,303\n-456,-621,1527\n-447,-329,318\n-430,-3130,366\n-413,-627,1469\n-345,-311,381\n-36,-1284,1171\n-27,-1108,-65\n7,-33,-71\n12,-2351,-103\n26,-1119,1091\n346,-2985,342\n366,-3059,397\n377,-2827,367\n390,-675,-793\n396,-1931,-563\n404,-588,-901\n408,-1815,803\n423,-701,434\n432,-2009,850\n443,580,662\n455,729,728\n456,-540,1869\n459,-707,401\n465,-695,1988\n474,580,667\n496,-1584,1900\n497,-1838,-617\n527,-524,1933\n528,-643,409\n534,-1912,768\n544,-627,-890\n553,345,-567\n564,392,-477\n568,-2007,-577\n605,-1665,1952\n612,-1593,1893\n630,319,-379\n686,-3108,-505\n776,-3184,-501\n846,-3110,-434\n1135,-1161,1235\n1243,-1093,1063\n1660,-552,429\n1693,-557,386\n1735,-437,1738\n1749,-1800,1813\n1772,-405,1572\n1776,-675,371\n1779,-442,1789\n1780,-1548,337\n1786,-1538,337\n1847,-1591,415\n1889,-1729,1762\n1994,-1805,1792"

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ 'a' `shouldBe` 'a'
  describe "merge 0/1" $ do
    let p1 =
          Util.autoFileTest
            ( ( \(a : b : _) ->
                  let (o, f) = findMerge' a b
                   in a `Set.intersection` Set.map ((<>) o) (Set.map f b)
              )
                . parse1
            )
    it "example 1" $ p1 "./ex1.txt" (head $ parse1 firstMergedEx1)
  describe "merge 1/4" $ do
    let p1 =
          Util.autoFileTest
            ( ( \(z : a : _ : _ : b : _) ->
                  let (off1, f1) = findMerge' a b
                      (off2, f2) = findMerge' z a
                   in Set.map ((<>) off2) $ Set.map f2 $ (a `Set.intersection` Set.map (<> off1) (Set.map f1 b))
              )
                . parse1
            )
    it "example 1" $ p1 "./ex1.txt" (head $ parse1 firstMergedEx2)
  describe "merge 2/3" $ do
    let p1 =
          Util.autoFileTest
            ( fst . (\(_ : a : _ : b : _) -> findMerge' a b)
                . parse1
            )
    it "example 1" $ p1 "./ex1.txt" mempty
  describe "part 1" $ do
    let p1 = Util.autoFileTest (allProbes . parse1)
    it "example 1" $ p1 "./ex1.txt" (head $ parse1 allProbesEx1)
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" 3621
