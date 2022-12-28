{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Prelude (Bit, BitVector, Clock, DomainPeriod, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, mealy, register, repeat, replicate, resetGen, truncateB, unbundle, vName, vPeriod, vResetPolarity)
import Clash.Prelude.BlockRam (ResetStrategy (NoClearOnReset), blockRamU, readNew)
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (!!), (++))
import qualified Clash.Sized.Vector as Vector
import Clash.WaveDrom (BitsWave (BitsWave), ShowWave (ShowWave), ToWave, render, wavedromWithClock)
import Clash.XException (NFDataX, ShowX)
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Data.ByteString.UTF8 ()
import Data.Char (chr, ord)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Ice40.Pll.Pad (pllPadPrim)
import System.Hclip (setClipboard)
import Util (adapt, awaitBothT, charToUartRx, delayBufferVecT)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="Alchitry3", vResetPolarity=ActiveLow, vPeriod=30000}

-- part 1 approach:
-- We need 8 stacks, all of which _could_ stack all items (almost 256), and
-- have identifiers at least 5 bits long.  This precludes our register count,
-- so we have to use RAM.  If the stacks were shorter, we could possibly
-- combine many blocks to read and write the whole stack in one cycle, but
-- we're beyond that limit too.  So we use 8 blocks of 256 items each.
--
-- Since we can't pack for more efficient access, we need a full clock cycle to
-- move an item.  In theory, we could move 256 repeatedly.  Lucky for us it
-- takes 3040 clock cycles to parse a single line.  So our bit rate has get
-- within 60% of our clockspeed before this is a concern.  At which point, we
-- could add a buffer, considering our average case should be well below 256.
--
-- At the parse phase, we pass an add or move instruction, which then executes
-- against a mealy machine connected to each RAM block.
--
-- Annoyingly, we get the _top_ of the stack first, so we need to reverse the
-- pointer (or worse, reverse everything).  When we write our first items,
-- we'll start at 0 and move bacwkards (255, 254...) and then reset the pointer
-- to 0 on our first move instruction.  Essentially, this means the true start
-- of our stack is somewhere randomly in the middle of our RAM.  An interesting
-- case here is when the stack starts as empty.  Nothing is added, and then
-- when reset the pointer stays at 0, which is undefined.  But the stack is
-- empty, so we know this value can't be used anyway.  When we add an item, it
-- gets written to address 1, so this stack just happens to have its bottom
-- value at 1, but everything is correct.

data Inst
  = Add (Unsigned 3) (BitVector 8)
  | InitialCratesDone
  | Move (Unsigned 8) (Unsigned 3) (Unsigned 3)
  | CrateMovesDone
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data StackParseProgress
  = Waiting
  | IsPresent
  | IsNotPresent
  | Closing
  | End
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data MoveParseProgress
  = PreCount
  | Count (Unsigned 8) -- TODO: May need to do this as BCD
  | PreFromId (Unsigned 8)
  | PreToId (Unsigned 8) (Unsigned 3)
  | NeedNewLine
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data ParseProgress
  = SPP (Unsigned 3) StackParseProgress
  | NoMoreStacks
  | LineBreak
  | MPP MoveParseProgress
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

parseDigit :: KnownNat n => BitVector 8 -> Maybe (Unsigned n)
parseDigit 48 = Just 0
parseDigit 49 = Just 1
parseDigit 50 = Just 2
parseDigit 51 = Just 3
parseDigit 52 = Just 4
parseDigit 53 = Just 5
parseDigit 54 = Just 6
parseDigit 55 = Just 7
parseDigit 56 = Just 8
parseDigit 57 = Just 9
parseDigit _ = Nothing

parseStackId :: BitVector 8 -> Maybe (Unsigned 3)
parseStackId x =
  let mOneIndexed = parseDigit x :: Maybe (Unsigned 4)
   in (\x -> truncateB (x - 1)) <$> mOneIndexed

parseT :: ParseProgress -> BitVector 8 -> (ParseProgress, Maybe Inst)
parseT (SPP n Waiting) 32 = (SPP n IsNotPresent, Nothing)
parseT (SPP n Waiting) 91 = (SPP n IsPresent, Nothing)
parseT (SPP n IsPresent) x = (SPP n Closing, Just (Add n x))
parseT (SPP n IsNotPresent) 32 = (SPP n Closing, Nothing)
parseT (SPP 0 IsNotPresent) _ = (NoMoreStacks, Just InitialCratesDone)
parseT (SPP n Closing) _ = (SPP n End, Nothing)
parseT (SPP n End) 32 = (SPP (n + 1) Waiting, Nothing)
parseT (SPP _ End) 10 = (SPP 0 Waiting, Nothing)
parseT NoMoreStacks 10 = (LineBreak, Nothing)
parseT NoMoreStacks _ = (NoMoreStacks, Nothing)
parseT LineBreak 10 = (MPP PreCount, Nothing)
parseT LineBreak _ = (LineBreak, Nothing)
parseT (MPP PreCount) 4 = (SPP 0 Waiting, Just CrateMovesDone)
parseT (MPP PreCount) x =
  case parseDigit x of
    Nothing -> (MPP PreCount, Nothing)
    Just digit -> (MPP (Count digit), Nothing)
parseT (MPP (Count acc)) x =
  case parseDigit x of
    Nothing -> (MPP (PreFromId acc), Nothing)
    Just digit -> (MPP (Count (acc * 10 + digit)), Nothing)
parseT (MPP (PreFromId count)) x =
  case parseStackId x of
    Nothing -> (MPP (PreFromId count), Nothing)
    Just stackId -> (MPP (PreToId count stackId), Nothing)
parseT (MPP (PreToId count fromId)) x =
  case parseStackId x of
    Nothing -> (MPP (PreToId count fromId), Nothing)
    Just stackId -> (MPP NeedNewLine, Just (Move count fromId stackId))
parseT (MPP NeedNewLine) 10 = (MPP PreCount, Nothing)
parseT s x = error ("Unexpected input while parsing.\n" <> show s <> "\n" <> show x)

data CraneState = CraneState
  { pointers :: Vec 8 (Unsigned 8),
    job :: Maybe (Unsigned 8, Unsigned 3, Unsigned 3),
    isDone :: Bool
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

craneMoveHelper :: CraneState -> Vec 8 (BitVector 8) -> (CraneState, (Maybe (Vec 8 (BitVector 8)), Vec 8 (Unsigned 8), Vec 8 (Maybe (Unsigned 8, BitVector 8))))
craneMoveHelper s@CraneState {pointers, job, isDone} readValues =
  case job of
    Nothing ->
      if isDone
        then
          ( CraneState (repeat 0) Nothing False,
            (Just readValues, repeat 0, repeat Nothing)
          )
        else (s, (Nothing, pointers, repeat Nothing))
    Just (count, from, to) ->
      let fromPointer' = (pointers !! from) - 1
          toPointer' = (pointers !! to) + 1
          pointers' = Vector.replace from fromPointer' $ Vector.replace to toPointer' pointers
          writeValues = Vector.replace to (Just (toPointer', readValues !! from)) $ repeat Nothing
          s' =
            s
              { pointers = pointers',
                job = if count == 1 then Nothing else Just (count - 1, from, to)
              }
       in (s', (Nothing, pointers', writeValues))

craneT :: CraneState -> (Maybe Inst, Vec 8 (BitVector 8)) -> (CraneState, (Maybe (Vec 8 (BitVector 8)), Vec 8 (Unsigned 8), Vec 8 (Maybe (Unsigned 8, BitVector 8))))
craneT s (Just (Add n id), _) =
  let idx = pointers s !! n
      idx' = idx - 1
      s' = s {pointers = Vector.replace n idx' (pointers s)}
   in (s', (Nothing, pointers s', Vector.replace n (Just (idx, id)) (repeat Nothing)))
craneT s (Just InitialCratesDone, _) =
  let s' = s {pointers = repeat 0}
   in (s', (Nothing, pointers s', repeat Nothing))
craneT s (Just (Move count from to), readValues) =
  craneMoveHelper (s {job = Just (count, from, to)}) readValues
craneT s (Just CrateMovesDone, readValues) =
  craneMoveHelper (s {isDone = True}) readValues
craneT s (Nothing, readValues) =
  craneMoveHelper s readValues

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Vec 8 (BitVector 8)))
runCore1 mByteSig =
  let mParsed = join <$> mealy (adapt parseT) (SPP 0 Waiting) mByteSig
      ramValues = bundle $ Vector.map (\i -> readNew (blockRamU NoClearOnReset (SNat :: SNat 256) undefined) (unbundle readAddrs !! i) (unbundle writes !! i)) $ Vector.iterate (SNat :: SNat 8) ((+) 1) (0 :: Unsigned 3)
      (out, readAddrs, writes) = unbundle $ mealy craneT (CraneState (repeat 0) Nothing False) $ bundle (mParsed, ramValues)
   in out

runCore2 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore2 =
  pure (pure (pure 0))

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Vec 8 (BitVector 8), Unsigned 64))
runCore maybeByteSig =
  let part1 = runCore1 maybeByteSig
      part2 = runCore2 maybeByteSig
      out = mealy awaitBothT (Nothing, Nothing) (bundle (part1, part2))
   in out

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom Bit -> Signal dom Bit
run =
  fst
    . uartTx (SNat :: SNat 2083333)
    . register Nothing
    . mealy (delayBufferVecT (SNat :: SNat 160) (SNat :: SNat 10)) Nothing
    . register Nothing
    . fmap (fmap ((\x -> x ++ (10 :> 4 :> Nil)) . fst))
    . runCore
    . register Nothing
    . uartRx (SNat :: SNat 2083333)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal Alchitry3 Bit -> Signal Alchitry3 Bit
topEntity clk rst _ input =
  let (clk', _, _) = pllPadPrim 0 0 2 "SIMPLE" 1 "GENCLK" "FIXED" "FIXED" 0 0 0 clk (pure 0) (pure 1) (pure 0)
   in exposeClockResetEnable run clk' (convertReset clk clk' rst) enableGen input

testOutput1 :: Vec _ Char
testOutput1 = $(listToVecTH "CMZAAAAA") -- Hove to cheat a bit

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "    [D]\n[N] [C]\n[Z] [M] [P] [A] [A] [A] [A] [A]\n 1   2   3\n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2\n")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "    [C]\n[A] [B]\n 1 2\n\nmove 1 from 1 to 2\nmove 2 from 2 to 1\n")
   in ( (Nothing :> Nil)
          ++ fmap (Just . fromIntegral . ord) raw
          ++ (Just 4 :> Nothing :> Nil)
      )

data InOut a b = InOut
  { input :: a,
    output :: b
  }
  deriving stock (Generic, Eq)
  deriving anyclass (NFData, NFDataX, ShowX, ToWave)

copyWavedrom :: IO ()
copyWavedrom =
  let en = enableGen
      clk = tbClockGen (False <$ out)
      rst = resetGen :: Reset System
      inputSignal = stimuliGenerator clk rst testInputCore
      out = InOut <$> fmap BitsWave inputSignal <*> fmap ShowWave (exposeClockResetEnable runCore clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 75 "" out

testBench :: Signal Alchitry3 Bool
testBench =
  let en = enableGen
      clk = tbClockGen (not <$> done)
      rst = resetGen
      inputSignal = stimuliGenerator clk rst testInput
      expectOutput =
        outputVerifier'
          clk
          rst
          ( replicate (SNat :: SNat 22087) 1
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
