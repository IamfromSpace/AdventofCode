module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import AdventOfCode.IntCode
       (Computer, Insts, WaitState(..), consume, initialize, parseInsts,
        step)
import AdventOfCode.Util (elmTrace)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.List ()
import Data.List.Split ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ()
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

parse1 :: String -> _
parse1 = parseInsts

parse2 :: String -> _
parse2 = parse1

toInput :: [String] -> Seq Integer
toInput strs = Seq.fromList $ fmap (fromIntegral . ord) $ unlines strs

answer1 :: _ -> _
answer1 insts =
    fmap (chr . fromIntegral) $
    toList $
    snd $
    snd $
    consume
        ( initialize insts
        , toInput
              [ "south"
              --, "take space law space brochure"
              , "south"
              --, "take mouse"
              , "south"
              , "take astrolabe" -- always need
              , "south"
              , "take mug" -- always need
              , "north"
              , "north"
              , "west"
              , "north"
              , "west"
              , "east"
              , "north"
              , "take wreath" -- need at least mouse OR wreath
              , "south"
              , "south"
              , "east"
              , "north"
              , "west"
              , "take sand" -- need at least mouse OR sand
              , "north"
              --, "take manifold"
              , "south"
              , "west"
              --never take, "take monolith"
              , "west"
              , "west"
              {-
              , "east"
              , "east"
              , "north"
              , "west"
              , "east"
              , "south"
              , "east"
              , "north"
              , "west"
              , "east"
              , "north"
              , "east"
              , "west"
              , "south"
              -}
              ])

answer2 :: _ -> _
answer2 = id
