module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (toList)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Prelude hiding ((++), init, lookup, map)
import Text.ParserCombinators.PArrow (MD, (>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.Read (readMaybe)

parse1 :: String -> Seq Int
parse1 = Seq.fromList . fmap (read . pure)

parse2 :: String -> _
parse2 = parse1

move :: Int -> Seq Int -> Seq Int
move len (current :<| a :<| b :<| c :<| cups) =
    let rem = cups |> current
        findIndex off =
            Seq.findIndexL
                ((==) (((len + current - 1 - off) `mod` len) + 1))
                rem
        mDestIndex = findIndex 1 <|> findIndex 2 <|> findIndex 3 <|> findIndex 4
    in case mDestIndex of
           Just destIndex ->
               Seq.insertAt (destIndex + 1) a $
               Seq.insertAt (destIndex + 1) b $
               Seq.insertAt (destIndex + 1) c rem
           Nothing -> rem |> a |> b |> c

move2 :: Int -> (Int, IntMap Int) -> (Int, IntMap Int)
move2 len (current, cups) =
    Maybe.fromJust $ do
        a <- IM.lookup current cups
        b <- IM.lookup a cups
        c <- IM.lookup b cups
        next <- IM.lookup c cups
        let moved = Set.fromList [a, b, c]
        let offset =
                if not (Set.member (current - 1) moved)
                    then 1
                    else if not (Set.member (current - 2) moved)
                             then 2
                             else if not (Set.member (current - 3) moved)
                                      then 3
                                      else if not
                                                  (Set.member
                                                       (current - 4)
                                                       moved)
                                               then 4
                                               else undefined
        let destination = ((len + current - 1 - offset) `mod` len) + 1
        afterDestination <- IM.lookup destination cups
        let cups' =
                IM.insert destination a $
                IM.insert c afterDestination $ IM.insert current next cups
        return (next, cups')

finalize :: Seq Int -> String
finalize s =
    let i = Maybe.fromJust $ Seq.findIndexL ((==) 1) s
        (a, b) = Seq.splitAt i s
    in concatMap show $ drop 1 $ toList (b <> a)

answer1 :: _ -> _
answer1 s = finalize (iterate (move 9) s !! 100)

finalize2 :: (Int, IntMap Int) -> (Int, Int)
finalize2 (_, m) =
    Maybe.fromJust $ do
        a <- IM.lookup 1 m
        b <- IM.lookup a m
        return (a, b)

init :: Seq Int -> (Int, IntMap Int)
init (a :<| b :<| c :<| d :<| e :<| f :<| g :<| h :<| i :<| Empty) =
    ( a
    , IM.fromList
          ([ (1000000, a)
           , (a, b)
           , (b, c)
           , (c, d)
           , (d, e)
           , (e, f)
           , (f, g)
           , (g, h)
           , (h, i)
           , (i, 10)
           ] <>
           zip [10 .. 999999] [11 .. 1000000]))

answer2 :: Seq Int -> _
answer2 s = finalize2 (iterate (move2 1000000) (init s) !! 10000000)

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: String
ex1_1 = "67384529"

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: (Int, Int)
ex2_1 = (934001, 159792)

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
