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

parseChar :: Char -> Bool
parseChar '1' = True
parseChar '0' = False

parse1 :: String -> _
parse1 = Seq.fromList . fmap parseChar

parse2 :: String -> _
parse2 = parse1

show' True = '1'
show' False = '0'

-- 17:03
answer1 :: _ -> _
answer1 = answer 272

dragon1 :: Seq Bool -> Seq Bool
dragon1 s = s <> pure False <> Seq.reverse (fmap not s)

dragon :: (Int, Seq Bool) -> Seq Bool
dragon (len, s) =
    if length s >= len
        then Seq.take len s
        else dragon (len, dragon1 s)

checksum s =
    if length s `mod` 2 /= 0
        then s
        else checksum $ fmap ((\[a, b] -> a == b) . toList) $ Seq.chunksOf 2 s

answer len = toList . fmap show' . checksum . dragon . (,) len

-- 26:47
answer2 :: _ -> _
answer2 = answer 35651584

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: _
ex1_1 = "100"

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
