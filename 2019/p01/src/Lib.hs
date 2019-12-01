module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Data.List ()
import Data.List.Split ()
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

type Parsed2 = [Double]

parse1 :: String -> [Double]
parse1 = fmap read . lines

parse2 :: String -> Parsed2
parse2 = parse1

answer1 :: [Double] -> String
answer1 = show . sum . fmap naiveFuel

naiveFuel :: Double -> Double
naiveFuel x = realToFrac $ max 0 $ floor (x / 3) - 2

realFuel :: Double -> Double
realFuel 0 = 0
realFuel x =
    let n = naiveFuel x
    in n + realFuel n

answer2 :: Parsed2 -> String
answer2 = show . sum . fmap realFuel
