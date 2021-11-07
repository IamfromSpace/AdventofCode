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
import Data.ByteString.Internal (c2w)
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

-- If needed, really this is just (0,0)-(3,3) inclusive where (3,4) and (4,3) win
mazeStr :: String
mazeStr =
    "#########\n#S| | | #\n#-#-#-#-#\n# | | | #\n#-#-#-#-#\n# | | | #\n#-#-#-#-#\n# | | |  \n####### V"

-- Meta-Position
data MP =
    MP String --Current Passcode
       (Util.Vector (Integer, Integer)) -- Grid Position
    deriving (Show, Eq, Ord)

parse1 :: String -> _
parse1 = id

parse2 :: String -> _
parse2 = parse1

isOpen :: Char -> Bool
isOpen 'b' = True
isOpen 'c' = True
isOpen 'd' = True
isOpen 'e' = True
isOpen 'f' = True
isOpen _ = False

getUnlocks :: String -> [(Char, Util.Vector (Integer, Integer))]
getUnlocks passcode =
    let (up:down:left:right:_) =
            take 4 $
            Util.byteStringToHex $ MD5.hash (BS.pack (fmap c2w passcode))
    in Maybe.catMaybes
           [ if isOpen up
                 then Just ('U', Util.Vector (0, -1))
                 else Nothing
           , if isOpen down
                 then Just ('D', Util.Vector (0, 1))
                 else Nothing
           , if isOpen left
                 then Just ('L', Util.Vector (-1, 0))
                 else Nothing
           , if isOpen right
                 then Just ('R', Util.Vector (1, 0))
                 else Nothing
           ]

isValid (Util.Vector (x, y)) =
    (x == 4 && y == 3) ||
    (x == 3 && y == 4) || (x >= 0 && y >= 0 && x < 4 && y < 4)

getOptions :: MP -> [Util.AStarStepOption2 MP (Sum Integer)]
getOptions (MP passcode v) =
    fmap (\x -> Util.AStarStepOption2 x (-1)) $
    filter (\(MP _ v) -> isValid v) $
    fmap (\(c, dv) -> MP (passcode <> pure c) (v <> dv)) $ getUnlocks passcode

guessLowestCost1 :: MP -> Sum Integer
guessLowestCost1 (MP _ (Util.Vector (x, y))) = pure (3 - x + 3 - y)

guessLowestCost :: MP -> Sum Integer
guessLowestCost (MP _ (Util.Vector (x, y))) =
    if x == 3 && y == 3
        then 0
        else -10000000

-- 45:40 (broken by solving part 2, this should use guessLowestCost1 and the cost of a step should be 1 not -1)
answer1 :: _ -> _
answer1 passcode =
    case Util.aStar2 guessLowestCost getOptions (MP passcode mempty) of
        Nothing -> error "no valid path!"
        Just (_, (MP passcode _):_) ->
            filter (\c -> c == 'U' || c == 'D' || c == 'L' || c == 'R') passcode

-- 52:33
answer2 :: _ -> _
answer2 = length . answer1

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
