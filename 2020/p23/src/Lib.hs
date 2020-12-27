module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
import Control.Monad ((>=>))
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
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as Vec
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

finalize :: Seq Int -> String
finalize s =
    let i = Maybe.fromJust $ Seq.findIndexL ((==) 1) s
        (a, b) = Seq.splitAt i s
    in concatMap show $ drop 1 $ toList (b <> a)

answer1 :: _ -> _
answer1 s = finalize (iterate (move 9) s !! 100)

move2 :: Int -> IOVector Int -> Int -> IO Int
move2 len cups current = do
    a <- Vec.unsafeRead cups current
    b <- Vec.unsafeRead cups a
    c <- Vec.unsafeRead cups b
    next <- Vec.unsafeRead cups c
    let moved = Set.fromList [a, b, c]
    let offset =
            if not (Set.member (current - 1) moved)
                then 1
                else if not (Set.member (current - 2) moved)
                         then 2
                         else if not (Set.member (current - 3) moved)
                                  then 3
                                  else if not (Set.member (current - 4) moved)
                                           then 4
                                           else undefined
    let destination = ((len + current - 1 - offset) `mod` len) + 1
    afterDestination <- Vec.unsafeRead cups destination
    Vec.unsafeWrite cups current next
    Vec.unsafeWrite cups destination a
    Vec.unsafeWrite cups c afterDestination
    return next

move2Many :: Int -> Int -> IOVector Int -> Int -> IO ()
move2Many 0 _ _ = const (return ())
move2Many n len cups = move2 len cups >=> move2Many (n - 1) len cups

finalize2 :: IOVector Int -> IO Integer
finalize2 cups = do
    a <- Vec.unsafeRead cups 1
    b <- Vec.unsafeRead cups a
    return (fromIntegral a * fromIntegral b)

init :: Seq Int -> IO (Int, IOVector Int)
init (a :<| b :<| c :<| d :<| e :<| f :<| g :<| h :<| i :<| Empty) = do
    cups <- Vec.new 1000001
    Vec.unsafeWrite cups 1000000 a
    Vec.unsafeWrite cups a b
    Vec.unsafeWrite cups b c
    Vec.unsafeWrite cups c d
    Vec.unsafeWrite cups d e
    Vec.unsafeWrite cups e f
    Vec.unsafeWrite cups f g
    Vec.unsafeWrite cups g h
    Vec.unsafeWrite cups h i
    Vec.unsafeWrite cups i 10
    _ <-
        sequence $
        List.zipWith (Vec.unsafeWrite cups) [10 .. 999999] [11 .. 1000000]
    return (a, cups)

answer2 :: Seq Int -> IO Integer
answer2 s = do
    (first, cups) <- init s
    move2Many 10000000 1000000 cups first
    finalize2 cups

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

ex2_1 :: Integer
ex2_1 = 934001 * 159792

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
