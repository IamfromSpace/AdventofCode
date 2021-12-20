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

type V2 = Vector (Integer, Integer)

parse1 :: String -> (Set Integer, (Bool, Set V2))
parse1 s =
  let [alg, img] = Split.splitOn "\n\n" s
   in ( Util.parseGrid (\(x, _) c s -> if c == '#' then Set.insert x s else s) mempty alg,
        (False, Util.parseGrid (\v c s -> if c == '#' then Set.insert (Vector v) s else s) mempty img)
      )

parse2 :: String -> _
parse2 = parse1

threes =
  [ Vector {getVector = (-1, -1)},
    Vector {getVector = (0, -1)},
    Vector {getVector = (1, -1)},
    Vector {getVector = (-1, 0)},
    Vector {getVector = (0, 0)},
    Vector {getVector = (1, 0)},
    Vector {getVector = (-1, 1)},
    Vector {getVector = (0, 1)},
    Vector {getVector = (1, 1)}
  ]

toNum :: [Bool] -> Integer
toNum =
  let go (h : t) = (if h then 1 else 0) + 2 * go t
      go [] = 0
   in go . reverse

pointToBools (isFlipped, input) p = fmap ((if isFlipped then not else id) . flip Set.member input) (fmap (<> p) threes)

pointToNum x p = toNum $ pointToBools x p

isLit :: Set Integer -> (Bool, Set V2) -> V2 -> Bool
isLit rules x p =
  flip Set.member rules $ pointToNum x p

step :: Set Integer -> (Bool, Set V2) -> (Bool, Set V2)
step rules (isFlipped, input) =
  let (xs, ys) = unzip $ fmap getVector $ Set.elems input
      (minX, minY) = (minimum xs, minimum ys)
      (maxX, maxY) = (maximum xs, maximum ys)
      allVs =
        App.liftA2
          (\x y -> Vector (x, y))
          [minX - 1 .. maxX + 1]
          [minY - 1 .. maxY + 1]
   in ( not isFlipped,
        foldl
          ( \s v ->
              if ( if isFlipped
                     then id --the _next_ on is not
                     else not
                 )
                $ isLit rules (isFlipped, input) v
                then Set.insert v s
                else s
          )
          mempty
          allVs
      )

answer1 :: _ -> _
answer1 (rules, input) = Set.size $ snd $ step rules $ step rules input

--Util.prettyPrintPointSetFlippable True '.' '#' $ Set.map getVector $ snd $

answer2 :: _ -> _
answer2 (rules, input) = Set.size $ snd $ Util.applyNTimes (step rules) input 50

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ 'a' `shouldBe` 'a'
  describe "toBools" $ do
    let p1 = Util.autoFileTest (flip pointToBools (Vector (2, 2)) . snd . parse1)
    it "example 1" $ p1 "./ex1.txt" [False, False, False, True, False, False, False, True, False]
  describe "toBools2" $ do
    let p1 = Util.autoFileTest (flip pointToBools (Vector (1, 1)) . snd . parse1)
    it "example 1" $ p1 "./ex1.txt" [True, False, False, True, False, False, True, True, False]
  describe "toBools3" $ do
    let p1 = Util.autoFileTest (flip pointToBools (Vector (1, 3)) . snd . parse1)
    it "example 1" $ p1 "./ex1.txt" [True, True, False, False, False, True, False, False, True]
  describe "toNum" $ do
    let p1 = Util.autoFileTest (flip pointToNum (Vector (2, 2)) . snd . parse1)
    it "example 1" $ p1 "./ex1.txt" 34
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1.txt" 35
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex1.txt" 3351
