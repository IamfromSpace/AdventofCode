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

parse1 :: String -> (Seq Int, Seq Int)
parse1 s =
    let [(_:p1), (_:p2)] = multiLines s
    in (Seq.fromList $ fmap read p1, Seq.fromList $ fmap read p2)

parse2 :: String -> _
parse2 = parse1

playHand :: (Seq Int, Seq Int) -> Maybe (Seq Int, Seq Int)
playHand (h1 :<| p1, h2 :<| p2) =
    Just $
    if h1 > h2
        then (p1 |> h1 |> h2, p2)
        else (p1, p2 |> h2 |> h1)
playHand _ = Nothing

playGame :: (Seq Int, Seq Int) -> Seq Int
playGame (!p1, !p2) =
    case playHand (p1, p2) of
        Nothing ->
            if Seq.length p1 == 0
                then p2
                else p1
        Just s' -> playGame s'

score :: Seq Int -> Int
score s = sum $ List.zipWith (*) (reverse (toList s)) [1 ..]

answer1 :: (Seq Int, Seq Int) -> Int
answer1 = score . playGame

playGame2 ::
       Set (Seq Int, Seq Int)
    -> (Seq Int, Seq Int)
    -> Either (Seq Int) (Seq Int)
playGame2 seen (s@(p1Full@(h1 :<| p1), h2 :<| p2)) =
    if Set.member s seen
        then Left p1Full
        else let seen' = Set.insert s seen
             in if (h1 <= Seq.length p1 && h2 <= Seq.length p2)
                    then case playGame2 mempty (Seq.take h1 p1, Seq.take h2 p2) of
                             Left _ -> playGame2 seen' (p1 |> h1 |> h2, p2)
                             Right _ -> playGame2 seen' (p1, p2 |> h2 |> h1)
                    else case playHand s of
                             Nothing ->
                                 if Seq.length p1 == 0
                                     then Right p2
                                     else Left p1
                             Just s' -> playGame2 seen' s'
playGame2 _ (p1, Empty) = Left p1
playGame2 _ (Empty, p2) = Right p2

answer2 :: _ -> _
answer2 = score . either id id . playGame2 mempty

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 306

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: Int
ex2_1 = 291

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
