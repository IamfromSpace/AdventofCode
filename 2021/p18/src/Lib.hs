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

data STree
  = P STree STree
  | R Int
  deriving (Eq, Show)

stree :: APC () STree
stree = (AP.decimal >>^ R) <+> (((AP.token '[' !>> stree >>! AP.token ',') &&& (stree >>! AP.token ']')) >>^ uncurry P)

parse1 :: String -> _
parse1 = AP.parseImpure (AP.linesOf stree)

pretty (R x) = show x
pretty (P x y) = "[" <> pretty x <> "," <> pretty y <> "]"

parse2 :: String -> _
parse2 = parse1

injectLeft :: Int -> STree -> STree
injectLeft x (R y) = R (x + y)
injectLeft x (P a b) = P (injectLeft x a) b

injectRight :: Int -> STree -> STree
injectRight x (R y) = R (x + y)
injectRight x (P a b) = P a (injectRight x b)

-- some minor cleanup here
explode :: Int -> STree -> Maybe (Maybe Int, STree, Maybe Int)
explode _ (R _) = Nothing
explode 4 (P (R a) (R b)) = Just (Just a, R 0, Just b)
explode d (P a b) =
  let onExplodeA (left, a', right) =
        (left, P a' (maybe b (flip injectLeft b) right), Nothing)
      onExplodeB (left, b', right) =
        (Nothing, P (maybe a (flip injectRight a) left) b', right)
   in fmap onExplodeA (explode (d + 1) a) <|> fmap onExplodeB (explode (d + 1) b)

split :: STree -> Maybe STree
split (R n) =
  if n >= 10
    then
      let (x, y) = n `divMod` 2
       in Just (P (R x) (R (x + y)))
    else Nothing
split (P a b) = fmap (flip P b) (split a) <|> fmap (P a) (split b)

reduce :: STree -> Maybe STree
reduce x = case explode 0 x of
  Nothing -> split x
  Just (_, reduced, _) -> Just reduced

add :: STree -> STree -> STree
add a b =
  let go x = maybe x go (reduce x)
   in go (P a b)

mag :: STree -> Int
mag (R x) = x
mag (P a b) = 3 * mag a + 2 * mag b

addAll :: _ -> _
addAll = foldl1 (\t n -> add t n)

answer1 :: _ -> _
answer1 = mag . addAll

bottom :: [STree]
bottom = App.liftA2 (\a b -> P (R a) (R b)) [0 .. 9] [0 .. 9]

levelN lower = App.liftA2 P (lower <> fmap R [0 .. 9]) (lower <> fmap R [0 .. 9])

fourth :: [STree]
fourth = levelN $ levelN $ levelN bottom

answer2 :: _ -> _
answer2 xs = maximum $ fmap mag $ App.liftA2 add xs xs

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "add" $ (add (AP.parseImpure stree ("[[[[4,3],4],4],[7,[[8,4],9]]]" :: String)) (P (R 1) (R 1))) `shouldBe` (AP.parseImpure stree ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" :: String))
    it "add step one" $
      (reduce (AP.parseImpure stree ("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" :: String)))
        `shouldBe` Just (AP.parseImpure stree ("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" :: String))
    it "add step two" $
      (reduce (AP.parseImpure stree ("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" :: String)))
        `shouldBe` Just (AP.parseImpure stree ("[[[[0,7],4],[15,[0,13]]],[1,1]]" :: String))
    it "add step three" $
      (reduce (AP.parseImpure stree ("[[[[0,7],4],[15,[0,13]]],[1,1]]" :: String)))
        `shouldBe` Just (AP.parseImpure stree ("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" :: String))
    it "add step four" $
      (reduce (AP.parseImpure stree ("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" :: String)))
        `shouldBe` Just (AP.parseImpure stree ("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]" :: String))
    it "add step five" $
      (reduce (AP.parseImpure stree ("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]" :: String)))
        `shouldBe` Just (AP.parseImpure stree ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" :: String))
    it "explode step example 1" $
      (reduce (AP.parseImpure stree ("[[[[[9,8],1],2],3],4]" :: String)))
        `shouldBe` Just (AP.parseImpure stree ("[[[[0,9],2],3],4]" :: String))
    it "mag" $
      mag (AP.parseImpure stree ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" :: String))
        `shouldBe` 3488
    it "addMore" $ (add (AP.parseImpure stree ("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" :: String)) (AP.parseImpure stree ("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]" :: String))) `shouldBe` (AP.parseImpure stree ("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" :: String))
  describe "part 1-0 fold" $ do
    let p1 = Util.autoFileTest (addAll . parse1)
    it "example 1" $ p1 "./ex0.txt" (AP.parseImpure stree ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" :: String))
  describe "part 1 fold" $ do
    let p1 = Util.autoFileTest (addAll . parse1)
    it "example 1" $ p1 "./ex1.txt" (AP.parseImpure stree ("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]" :: String))
  describe "part 1 mag" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1.txt" 4140
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
