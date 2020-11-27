module Lib where

import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding ((++), init, lookup, map)

data X
    = Reg Char
    | Val Int
    deriving (Show, Eq)

data Inst
    = Copy X
           Char
    | Inc Char
    | Dec Char
    | JumpNotZero X
                  Int
    deriving (Show, Eq)

parse1 :: String -> _
parse1 =
    const $
    Seq.fromList
        [ Copy (Val 1) 'a'
        , Copy (Val 1) 'b'
        , Copy (Val 26) 'd'
        , JumpNotZero (Reg 'c') 2
        , JumpNotZero (Val 1) 5
        , Copy (Val 7) 'c'
        , Inc 'd'
        , Dec 'c'
        , JumpNotZero (Reg 'c') (-2)
        , Copy (Reg 'a') 'c'
        , Inc 'a'
        , Dec 'b'
        , JumpNotZero (Reg 'b') (-2)
        , Copy (Reg 'c') 'b'
        , Dec 'd'
        , JumpNotZero (Reg 'd') (-6)
        , Copy (Val 16) 'c'
        , Copy (Val 17) 'd'
        , Inc 'a'
        , Dec 'd'
        , JumpNotZero (Reg 'd') (-2)
        , Dec 'c'
        , JumpNotZero (Reg 'c') (-5)
        ]

parse2 :: String -> _
parse2 = parse1

type State = (Int, Map Char Int)

step :: Inst -> State -> State
step i (s@(ic, regs)) =
    case i of
        Copy (Reg from) to ->
            step (Copy (Val (Map.findWithDefault 0 from regs)) to) s
        Copy (Val val) to -> (ic + 1, Map.insert to val regs)
        Inc r -> (ic + 1, Map.alter (pure . (+) 1 . Maybe.fromMaybe 0) r regs)
        Dec r ->
            ( ic + 1
            , Map.alter (pure . (\x -> x - 1) . Maybe.fromMaybe 0) r regs)
        JumpNotZero (Reg r) offset ->
            step (JumpNotZero (Val (Map.findWithDefault 0 r regs)) offset) s
        JumpNotZero (Val 0) _ -> (ic + 1, regs)
        JumpNotZero (Val _) offset -> (ic + offset, regs)

run :: _ -> (Int, Map Char Int) -> Map Char Int
run insts (s@(ic, regs)) =
    case Seq.lookup ic insts of
        Nothing -> regs
        Just i -> run insts (step i s)

-- 00:24:38.667 (1.517x 100th place)
answer1 :: _ -> _
answer1 = Maybe.fromJust . Map.lookup 'a' . (flip run) (0, mempty)

-- 00:25:26.400 (1.4x 100th place)
answer2 :: _ -> _
answer2 =
    Maybe.fromJust . Map.lookup 'a' . (flip run) (0, Map.insert 'c' 1 $ mempty)

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
