module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import AdventOfCode.IntCode
       (Computer, Insts, WaitState(..), consume, initialize, parseInsts)

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.List ()
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

parse1 :: String -> _
parse1 = parseInsts

parse2 :: String -> _
parse2 = parse1

answer1 :: Insts -> String
answer1 insts =
    fmap (chr . fromIntegral) $
    toList $ snd $ snd $ consume (initialize insts, mempty)

prog =
    Seq.fromList $
    fmap (fromIntegral . ord) $
    unlines
        [ "A,B,A,C,A,C,B,C,C,B"
        , "L,4,L,4,L,10,R,4"
        , "R,4,L,4,L,4,R,8,R,10"
        , "R,4,L,10,R,10"
        , "n"
        ]

answer2 :: _ -> _
answer2 insts =
    last $
    toList $ snd $ snd $ consume (initialize $ Map.insert 0 2 insts, prog)
