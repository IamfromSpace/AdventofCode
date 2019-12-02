module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util (asCounted)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString, pack, unpack)
import Data.ByteString.Builder (byteStringHex)
import Data.ByteString.UTF8 (fromString)
import Data.List (sort)
import Data.List.Split ()
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()
import GHC.Word (Word8)
import Numeric (showHex)

parse1 :: String -> String
parse1 = id

parse2 :: String -> _
parse2 = id

is5Zeros :: ByteString -> Bool
is5Zeros x =
    case unpack x of
        0:0:x:_ -> x < 16
        _ -> False

value :: ByteString -> Word8
value x =
    case unpack x of
        0:0:x:_ -> x

findIndexes :: String -> [Integer]
findIndexes x =
    take 8 $ filter (is5Zeros . hash . fromString . (++) x . show) [0 ..]

getValue :: String -> Integer -> Word8
getValue str i = value $ hash $ fromString $ (++) str $ show i

-- completion time: 42:59.53, not a strong showing, tripped up by bytestrings!
answer1 :: String -> [Word8]
answer1 x = fmap (getValue x) $ findIndexes x

findIndexes2 :: String -> [Integer]
findIndexes2 x =
    take 48 $ filter (is5Zeros . hash . fromString . (++) x . show) [0 ..]

values :: Integer -> ByteString -> (Word8, Integer, Word8)
values i x =
    case unpack x of
        0:0:x:y:_ -> (x, i, y `quot` 16)

getValues :: String -> Integer -> (Word8, Integer, Word8)
getValues str i = values i $ hash $ fromString $ (++) str $ show i

-- completion time: 56:15.69
answer2 :: _ -> _
answer2 x = sort $ fmap (getValues x) $ findIndexes2 x
