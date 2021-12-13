module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow (arr, returnA, (&&&), (***), (<+>), (<<<), (>>>), (>>^), (|||))
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State.Lazy (State, evalState, get, put)
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

x = AP.many1 (AP.anyOf (['a' .. 'z'] <> ['A' .. 'Z']))

parse1 :: String -> _
parse1 = AP.parseImpure (AP.linesOf ((x >>! AP.token '-') &&& x))

parse2 :: String -> _
parse2 = parse1

toMap :: [(String, String)] -> Map String [String]
toMap xs =
  foldl
    ( \m (a, b) ->
        Map.alter
          ( \case
              Nothing -> Just [b]
              Just xs -> Just (xs <> [b])
          )
          a
          m
    )
    mempty
    (xs <> fmap (\(a, b) -> (b, a)) (filter (\(f,t) -> not (f == "start" || t == "end")) xs))


follow :: Map String [String] -> (Set String, String) -> [[String]]
follow caves =
  fmap (fmap snd)
    . Util.explorePaths
      ( \(notAllowed, p) ->
          let options = Maybe.fromMaybe [] $ Map.lookup p caves
              allowedOptions = filter (not . flip Set.member notAllowed) options
              nextAllowed o = if all Char.isLower o then Set.insert o notAllowed else notAllowed
           in fmap (\o -> (nextAllowed o, o)) allowedOptions
      )

-- Answers changed from original
answer1 :: _ -> _
answer1 xs =
  length $
    filter (\(h : _) -> h == "end") $
      fmap reverse $
        follow (toMap xs) ((Set.singleton "start"), "start")

follow2 :: Map String [String] -> (Set String, Maybe String, String) -> [[String]]
follow2 caves =
  fmap (fmap (\(_, _, x) -> x))
    . Util.explorePaths
      ( \(notAllowed, double, p) ->
          let options = Maybe.fromMaybe [] $ Map.lookup p caves
              allowedOptions = filter (\o -> not (Set.member o notAllowed && Maybe.isJust double)) options
              next o =
                if all Char.isLower o
                  then
                    if Set.member o notAllowed
                      then (notAllowed, Just o, o)
                      else (Set.insert o notAllowed, double, o)
                  else (notAllowed, double, o)
           in fmap next allowedOptions
      )

answer2 :: _ -> _
answer2 xs =
  length $
    filter (\(h : _) -> h == "end") $
      fmap reverse $
        follow2 (toMap xs) (mempty, Nothing, "start")

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex1.txt" 36
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2.txt" 3509
  describe "part 2" $ do
    let p2 = Util.autoFileTest (\xs -> follow2 (toMap (parse1 xs)) (mempty, Nothing, "start"))
    it "example 1" $ p2 "./ex1.txt" []
