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

isTrap '^' = True
isTrap '.' = False
isTrap _ = error "bad input"

parse1 :: String -> Seq Bool -- isTrapped
parse1 = Seq.fromList . fmap isTrap

next1 :: Int -> Seq Bool -> Bool
next1 i s =
    let left = Maybe.fromMaybe False $ Seq.lookup (i - 1) s
        center = Maybe.fromMaybe False $ Seq.lookup i s
        right = Maybe.fromMaybe False $ Seq.lookup (i + 1) s
    in case (left, center, right) of
           (True, True, False) -> True
           (False, True, True) -> True
           (True, False, False) -> True
           (False, False, True) -> True
           _ -> False

next :: Seq Bool -> Seq Bool
next s = Seq.fromList $ fmap (\i -> next1 i s) [0 .. (length s - 1)]

parse2 :: String -> _
parse2 = parse1

-- 11:26 (Would be 96 on the leaderboard!)
answer1 :: _ -> _
answer1 = length . filter not . concat . fmap toList . take 40 . iterate next

-- 12:22 (Would be 86 on the leaderboard!)  Also Haskell's lazy streaming FTW here.
answer2 :: _ -> _
answer2 =
    length . filter not . concat . fmap toList . take 400000 . iterate next

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
