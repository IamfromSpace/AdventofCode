module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , run
    , SpInst(..)
    , Reg(..)
    ) where

import AdventOfCode.IntCode
       (Computer, Insts, WaitState(..), consume, initialize, parseInsts)
import AdventOfCode.Util (elmTrace)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Char (chr, ord)
import Data.List ()
import Data.List.Split ()

import Data.Foldable (toList)

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

parse1 :: String -> Insts
parse1 = parseInsts

parse2 :: String -> _
parse2 = parse1

data Reg
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | T
    | J
    deriving (Show)

data SpInst
    = And Reg
          Reg
    | Or Reg
         Reg
    | Not Reg
          Reg
    deriving (Show)

cmdToStr :: [SpInst] -> Bool -> Seq Integer
cmdToStr sinsts walk =
    Seq.fromList $
    fmap (fromIntegral . ord) $
    unlines $
    fmap
        (\case
             And a b -> "AND " ++ show a ++ " " ++ show b
             Or a b -> "OR " ++ show a ++ " " ++ show b
             Not a b -> "NOT " ++ show a ++ " " ++ show b)
        sinsts ++
    [ if walk
          then "WALK"
          else "RUN"
    ]

display :: Seq Integer -> String
display = fmap (chr . fromIntegral) . toList

run :: Insts -> [SpInst] -> Bool -> _
run insts cmd walk =
    let (s, (_, display -> x)) = consume (initialize insts, mempty)
    in display $ snd $ snd $ consume (s, cmdToStr cmd walk)

answer1 :: Insts -> _
answer1 insts =
    run insts [Not A J, Not B T, And D T, Or T J, Not C T, And D T, Or T J] True

answer2 :: _ -> _
answer2 insts =
    run
        insts
        [ Not C J
        , And D J
        , Or E T
        , Or H T
        , And T J
        , Not B T
        , And D T
        , Or T J
        , Not A T
        , Or T J
        ]
        False
