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
    (xs <> fmap (\(a, b) -> (b, a)) xs)

follow :: Map String [String] -> Set String -> String -> [[String]]
follow _ _ "end" = [["end"]]
follow caves notAllowed p =
  case Map.lookup p caves of
    Nothing -> error "this can't happen???"
    Just options ->
      let nexts = filter (\o -> not (Set.member o notAllowed)) options
       in fmap (\xxx -> p : xxx) $ concatMap (\n -> follow caves (if all Char.isLower n then Set.insert n notAllowed else notAllowed) n) nexts

answer1 :: _ -> _
answer1 xs =
  length $
    filter (\(h : _) -> h == "end") $
      fmap reverse $
        follow (toMap xs) (Set.singleton "start") "start"

follow2 :: Map String [String] -> Set String -> Maybe String -> String -> [[String]]
follow2 _ _ _ "end" = [["end"]]
follow2 caves notAllowed double p =
  case Map.lookup p caves of
    Nothing -> error "this can't happen???"
    Just options ->
      let nexts = filter (\o -> not (o == "start" || (Set.member o notAllowed && Maybe.isJust double))) options
       in fmap (\xxx -> p : xxx) $
            concatMap
              ( \n ->
                  let (na, d) = if all Char.isLower n then (if Set.member n notAllowed then (notAllowed, Just n) else (Set.insert n notAllowed, double)) else (notAllowed, double)
                   in follow2 caves na d n
              )
              nexts

answer2 :: _ -> _
answer2 xs =
  length $
    filter (\(h : _) -> h == "end") $
      fmap reverse $
        follow2 (toMap xs) mempty Nothing "start"

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
    let p2 = Util.autoFileTest (\xs -> follow2 (toMap (parse1 xs)) mempty Nothing "start")
    it "example 1" $ p2 "./ex1.txt" []
