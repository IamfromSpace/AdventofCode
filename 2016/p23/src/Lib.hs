module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow
  ( arr,
    (&&&),
    (***),
    (<+>),
    (<<<),
    (>>>),
    (>>^),
    (|||),
  )
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
import Data.Monoid (Sum (..))
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (init, lookup, map, (++))

data X
  = Reg Char
  | Val Int
  deriving (Show, Eq)

data Inst
  = Copy
      X
      X -- ONLY REG IS VALID
  | Inc Char
  | Dec Char
  | JumpNotZero
      X
      X
  | Toggle Char
  deriving (Show, Eq)

parse1 :: String -> _
parse1 =
  const $
    Seq.fromList
      [ Copy (Reg 'a') (Reg 'b'),
        Dec 'b',
        Copy (Reg 'a') (Reg 'd'),
        Copy (Val 0) (Reg 'a'),
        Copy (Reg 'b') (Reg 'c'),
        Inc 'a',
        Dec 'c',
        JumpNotZero (Reg 'c') (Val (-2)),
        Dec 'd',
        JumpNotZero (Reg 'd') (Val (-5)),
        Dec 'b',
        Copy (Reg 'b') (Reg 'c'),
        Copy (Reg 'c') (Reg 'd'),
        Dec 'd',
        Inc 'c',
        JumpNotZero (Reg 'd') (Val (-2)),
        Toggle 'c',
        Copy (Val (-16)) (Reg 'c'),
        JumpNotZero (Val 1) (Reg 'c'),
        Copy (Val 90) (Reg 'c'),
        JumpNotZero (Val 73) (Reg 'd'),
        Inc 'a',
        Inc 'd',
        JumpNotZero (Reg 'd') (Val (-2)),
        Inc 'c',
        JumpNotZero (Reg 'c') (Val (-5))
      ]

parse2 :: String -> _
parse2 = parse1

type State = (Int, Map Char Int, Seq Inst)

step :: Inst -> State -> State
step i s@(ic, regs, insts) =
  case i of
    Copy (Reg from) to ->
      step (Copy (Val (Map.findWithDefault 0 from regs)) to) s
    Copy _ (Val _) -> (ic + 1, regs, insts) -- ignored
    Copy (Val val) (Reg to) -> (ic + 1, Map.insert to val regs, insts)
    Inc r ->
      (ic + 1, Map.alter (pure . (+) 1 . Maybe.fromMaybe 0) r regs, insts)
    Dec r ->
      ( ic + 1,
        Map.alter (pure . (\x -> x - 1) . Maybe.fromMaybe 0) r regs,
        insts
      )
    JumpNotZero (Reg r) offset ->
      step (JumpNotZero (Val (Map.findWithDefault 0 r regs)) offset) s
    JumpNotZero (Val 0) _ -> (ic + 1, regs, insts)
    JumpNotZero (Val _) (Val offset) -> (ic + offset, regs, insts)
    -- TODO: Some chance that this is just an invalid instruction...
    JumpNotZero x (Reg r) ->
      step (JumpNotZero x (Val (Map.findWithDefault 0 r regs))) s
    Toggle r ->
      let i = ic + Map.findWithDefault 0 r regs
       in ( ic + 1,
            regs,
            case Seq.lookup i insts of
              Nothing -> insts
              Just (Copy a b) -> Seq.update i (JumpNotZero a b) insts
              Just (Inc a) -> Seq.update i (Dec a) insts
              Just (Dec a) -> Seq.update i (Inc a) insts
              Just (JumpNotZero a b) -> Seq.update i (Copy a b) insts
              Just (Toggle a) -> Seq.update i (Inc a) insts
          )

run :: State -> Map Char Int
run s@(ic, regs, insts) =
  case Seq.lookup ic insts of
    Nothing -> regs
    Just i -> run (step i s)

run1 :: State -> _
run1 s@(ic, _, insts) =
  case Seq.lookup ic insts of
    Nothing -> undefined
    Just i -> step i s

-- 44:40 (1.64x 100th place)
answer1 :: _ -> _
answer1 insts =
  Maybe.fromJust $ Map.lookup 'a' $ run (0, Map.singleton 'a' 7, insts)

-- 1000000:00:00, had a break between solving pt1 and pt2
-- 7290
-- 11610 (7)
-- 46890
-- 369450
-- 3635370
-- Manually simplifying the instructions yields....
answer2 :: a -> Integer
answer2 _ = product [1 .. 12] + 90 * 73

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex :: Seq Inst
ex =
  Seq.fromList
    [ Copy (Val 2) (Reg 'a'),
      Toggle 'a',
      Toggle 'a',
      Toggle 'a',
      Copy (Val 1) (Reg 'a'),
      Dec 'a',
      Dec 'a'
    ]

tests :: _
tests = do
  describe "pure components" $ do it "should run" $ answer1 ex `shouldBe` 3
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 5" $ p2 "./ex2_5.txt" undefined
