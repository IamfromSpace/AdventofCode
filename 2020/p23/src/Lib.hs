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

finalize :: Seq Int -> String
finalize s =
    let i = Maybe.fromJust $ Seq.findIndexL ((==) 1) s
        (a, b) = Seq.splitAt i s
    in concatMap show $ drop 1 $ toList (b <> a)

answer1 :: _ -> _
answer1 s = finalize (iterate (move 9) s !! 100)

type X = (IntMap (Seq Int), Seq Int)

takeX :: X -> (Int, X)
takeX (lazyInserts, h :<| t) =
    case IM.lookup h lazyInserts of
        Just s -> (h, (IM.delete h lazyInserts, s <> t))
        Nothing -> (h, (lazyInserts, t))
takeX _ = error "X's seq was empty!"

injectAfter :: Int -> Seq Int -> X -> X
injectAfter afterValue s (lazyInserts, t) =
    (IM.insert afterValue s lazyInserts, t)

pushBack :: Int -> X -> X
pushBack v (lazyInserts, seq) = (lazyInserts, seq |> v)

move2 :: Int -> X -> X
move2 len x =
    let (current, x') = takeX x
        (a, x'') = takeX x'
        (b, x''') = takeX x''
        (c, x'''') = takeX x'''
        moved = Set.fromList [a, b, c]
        offset =
            if not (Set.member (current - 1) moved)
                then 1
                else if not (Set.member (current - 2) moved)
                         then 2
                         else if not (Set.member (current - 3) moved)
                                  then 3
                                  else if not (Set.member (current - 4) moved)
                                           then 4
                                           else undefined
        destination = ((len + current - 1 - offset) `mod` len) + 1
    in pushBack current $ injectAfter destination (Seq.fromList [a, b, c]) x''''

finalize2 :: X -> Integer
finalize2 x =
    let (v, x') = takeX x
    in if v == 1
           then let (a, x'') = takeX x'
                    (b, _) = takeX x''
                in fromIntegral a * fromIntegral b
           else finalize2 x'

init :: Seq Int -> X
init s = (mempty, s <> Seq.fromList [10 .. 1000000])

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
