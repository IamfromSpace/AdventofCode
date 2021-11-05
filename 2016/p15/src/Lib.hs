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

parse1 :: String -> _
parse1 =
    const [(5 :: Int, 2 :: Int), (13, 7), (17, 10), (3, 2), (19, 9), (7, 0)]

parse2 :: String -> _
parse2 = parse1

cycle' :: [(Int, Int)] -> [(Int, Int)]
cycle' = fmap (\(a, b) -> (a, (b + 1) `mod` a))

-- Done in 15:59
answer1 :: _ -> Integer
answer1 _ =
    let preCycled =
            [(5 :: Int, 3 :: Int), (13, 9), (17, 13), (3, 0), (19, 14), (7, 6)]
    in fst $
       Util.boundedUntilWithCount 1000000 (all ((==) 0 . snd)) cycle' preCycled

-- Done in 17:19 (in theory, a bit of extra time since I didn't actually get
-- the answer into the paste buffer.  First one back in a while, so this one
-- was all pretty slow and rusty!)
answer2 :: _ -> _
answer2 _ =
    let preCycled =
            [ (5 :: Int, 3 :: Int)
            , (13, 9)
            , (17, 13)
            , (3, 0)
            , (19, 14)
            , (7, 6)
            , (11, 7)
            ]
    in fst $
       Util.boundedUntilWithCount 10000000 (all ((==) 0 . snd)) cycle' preCycled

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: _
ex1_1 = undefined

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: _
ex2_1 = undefined

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
