module Lib where

import AdventOfCode.Util ()
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Monad (guard)
import qualified Control.Monad.State.Lazy as Stae

-- cryptonite is unfortunately slower!
-- long term should just hide hash details in a `String -> String` utility
import qualified Crypto.Hash as Hash
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import Data.Foldable (foldMap, toList)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Word (Word8)
import Prelude hiding ((++), init, lookup, map)

parse1 :: String -> String
parse1 = id

parse2 :: String -> _
parse2 = parse1

hashIndex :: String -> Int -> String
hashIndex bs i =
    show $ (Hash.hash (BS8.pack (bs <> show i)) :: Hash.Digest Hash.MD5)

repeats3 :: Eq a => [a] -> Maybe a
repeats3 (a:b:c:t) =
    if (a == b && a == c)
        then Just a
        else repeats3 (b : c : t)
repeats3 _ = Nothing

repeats5 :: Eq a => [a] -> [a]
repeats5 (a:b:c:d:e:t) =
    (if (a == b && a == c && a == d && a == e)
         then (:) a
         else id)
        (repeats5 (b : c : d : e : t))
repeats5 _ = []

bufferEntry :: (Int -> String) -> Int -> (Set Char, String, Int)
bufferEntry h i =
    let asHash = h i
    in (Set.fromList (repeats5 asHash), asHash, i)

isKeyStrict' ::
       Int -> Seq (Set Char, String, Int) -> [Int] -> (Int -> String) -> Int
isKeyStrict' !n !fives !(j:t) !h =
    let ((_, iHash, i) :<| back) = fives
        fives' = back |> bufferEntry h j
    in if maybe
              False
              (\threeChar -> any (\(s, _, _) -> Set.member threeChar s) fives')
              (repeats3 iHash)
           then if n == 63
                    then i
                    else isKeyStrict' (n + 1) fives' t h
           else isKeyStrict' n fives' t h

isKeyStrict :: _ -> Int
isKeyStrict h =
    isKeyStrict'
        0
        (Seq.fromList $ fmap (bufferEntry h) $ [0 .. 999])
        [1000 ..]
        h

-- 01:14:34.8
answer1 :: _ -> _
answer1 = isKeyStrict . hashIndex

stretchIndex :: String -> Int -> String
stretchIndex bs i =
    (iterate
         (show . (Hash.hash :: ByteString -> Hash.Digest Hash.MD5) . BS8.pack)
         (bs <> show i)) !!
    2017

-- 01:38:34.267
answer2 :: _ -> _
answer2 = isKeyStrict . stretchIndex

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
