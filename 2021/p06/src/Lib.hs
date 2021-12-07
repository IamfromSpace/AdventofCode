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
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vec
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, (++))

parse1 :: String -> [Int]
parse1 s = read ("[" <> s <> "]") :: [Int]

parse2 :: String -> _
parse2 = parse1

toCounts :: [Int] -> IOVector Integer -> IO (IOVector Integer)
toCounts [] v = return v
toCounts (h : t) v = Vec.unsafeModify v ((+) 1) h *> toCounts t v

day :: IOVector Integer -> Int -> IO ()
day v !d = Vec.unsafeRead v d >>= (\x -> Vec.unsafeModify v ((+) x) ((d + 7) `mod` 9))

addUp :: IOVector Integer -> IO Integer
addUp = Vec.foldl (+) 0

answer :: _ -> _ -> _
answer dayCount is = do
  v <- Vec.replicate 9 0
  x <- toCounts is v
    *> List.foldl' (\f i -> f *> day v i) (pure ()) (take dayCount (cycle [0 .. 8]))
    *> addUp v
  return $ length $ show x

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
