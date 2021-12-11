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
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, (++))

parse1 :: String -> [Int]
parse1 s = read ("[" <> s <> "]") :: [Int]

parse2 :: String -> _
parse2 = parse1

toCounts :: [Int] -> Map Int Int -> Matrix Integer
toCounts [] m = Matrix.fromLists (List.transpose [fmap (fromIntegral . snd) (Map.toList m) <> [0, 0, 0, 0]])
toCounts (h : t) m = toCounts t (Map.alter (Just . (+) 1 . Maybe.fromMaybe 0) h m)

day :: Matrix Integer
day =
  Matrix.fromLists
    [ [0, 1, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 1, 0, 0],
      [1, 0, 0, 0, 0, 0, 0, 1, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 0, 0, 0, 0]
    ]

fastExp :: Int -> Matrix Integer -> Matrix Integer
fastExp (flip divMod 2 -> (0, 1)) !m = m
fastExp (flip divMod 2 -> (n, 1)) !m =
  Matrix.multStd2 m (fastExp n (Matrix.multStd2 m m))
fastExp (flip divMod 2 -> (n, 0)) !m =
  fastExp n (Matrix.multStd2 m m)
fastExp _ _ = error "fast exponentiation error"

answer :: Int -> [Int] -> Integer
answer dayCount is = sum $ Matrix.toList $ Matrix.multStd2 (fastExp (dayCount - 1) day) (toCounts is mempty)

-- These changed substantially
answer1 :: _ -> _
answer1 = answer 80

answer2 :: _ -> _
answer2 = answer 256

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "pure components" $ do
    it "should parse" $ parse1 undefined `shouldBe` undefined
