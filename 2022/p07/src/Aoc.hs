{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Prelude (Bit, BitVector, Clock, DomainPeriod, Enable, HiddenClockResetEnable, Index, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, mealy, register, replicate, resetGen, resize, unbundle, unpack, vName, vPeriod, vResetPolarity)
import Clash.Prelude.BlockRam (ResetStrategy (NoClearOnReset), blockRamU, readNew)
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (++))
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
import Util (BcdOrControl (..), Mealy, awaitBothT, bcd64T, bcdOrControlToAscii, blankLeadingZeros, charToUartRx, delayBufferVecT)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="Alchitry3", vResetPolarity=ActiveLow, vPeriod=30000}

-- TODO: Also might need to flush, it could make sense to have the parser emit
-- "missing" Pops for us.  That way we don't have to mix our stack
-- manipulations and our flushing logic.
-- Could also drop the first Push...
data Event
  = Push
  | Pop
  | -- The largest file size is 20bits, this gives us substantial headroom
    Add (Unsigned 32)
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

fakeParse :: BitVector 8 -> Event
fakeParse 111 = Pop
fakeParse 117 = Push
fakeParse x = Add (resize (unpack x :: Unsigned 8))

-- Emits:
--   - the possible write address/value
--   - the read address
--   - _Every_ directory's size
traverseT :: KnownNat n => Mealy (Index n, Unsigned 64) (Maybe Event, Unsigned 64) (Maybe (Index n, Unsigned 64), Index n, Maybe (Unsigned 64))
traverseT (ptr, acc) (Just Push, _) =
  let ptr' = ptr + 1
   in ((ptr', 0), (Just (ptr', acc), ptr', Nothing))
traverseT (ptr, acc) (Just (Add x), _) =
  let acc' = acc + resize x
   in ((ptr, acc'), (Nothing, ptr, Nothing))
traverseT (ptr, acc) (Just Pop, v) =
  let ptr' = ptr - 1
   in ((ptr', acc + v), (Nothing, ptr', Just acc))
traverseT s@(ptr, _) (Nothing, _) = (s, (Nothing, ptr, Nothing))

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe Event) -> Signal dom (Maybe (Unsigned 64))
runCore1 mEventSig =
  let (mWrite, readAddr, out) =
        unbundle $ mealy traverseT (0 :: Index 256, 0) (bundle (mEventSig, ramOut))
      ramOut = readNew (blockRamU NoClearOnReset (SNat :: SNat 256) undefined) readAddr mWrite
   in out

runCore2 :: HiddenClockResetEnable dom => Signal dom () -> Signal dom (Maybe (Unsigned 64))
runCore2 =
  pure (pure (pure 0))

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64, Unsigned 64))
runCore mEventSig =
  let part1 = runCore1 (fmap fakeParse <$> mEventSig)
      part2 = runCore2 (pure ())
      out = mealy awaitBothT (Nothing, Nothing) (bundle (part1, part2))
   in out

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom Bit -> Signal dom Bit
run =
  fst
    . uartTx (SNat :: SNat 2083333)
    . register Nothing
    . fmap (fmap bcdOrControlToAscii)
    . register Nothing
    . fmap join
    . mealy (delayBufferVecT (SNat :: SNat 160) (SNat :: SNat 43)) Nothing
    . register Nothing
    . fmap (fmap (\(a, b) -> blankLeadingZeros a ++ (Just Return :> Nil) ++ blankLeadingZeros b ++ (Just Return :> Just Eot :> Nil)))
    . register Nothing
    . mealy bcd64T Nothing
    . register Nothing
    . runCore
    . register Nothing
    . uartRx (SNat :: SNat 2083333)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal Alchitry3 Bit -> Signal Alchitry3 Bit
topEntity clk rst _ input =
  let (clk', _, _) = pllPadPrim 0 0 2 "SIMPLE" 1 "GENCLK" "FIXED" "FIXED" 0 0 0 clk (pure 0) (pure 1) (pure 0)
   in exposeClockResetEnable run clk' (convertReset clk clk' rst) enableGen input

testOutput1 :: Vec _ Char
testOutput1 = $(listToVecTH "32")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "0")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH " o")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH " u u ooo")
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

copyWavedrom1 :: IO ()
copyWavedrom1 =
  let en = enableGen
      clk = tbClockGen (False <$ out)
      rst = resetGen :: Reset System
      inputSignal = stimuliGenerator clk rst (fmap Just (Add 10 :> Add 12 :> Push :> Add 9 :> Push :> Push :> Add 8 :> Pop :> Add 6 :> Pop :> Add 4 :> Pop :> Pop :> Nil) ++ (Nothing :> Nil))
      out = fmap ShowWave (exposeClockResetEnable runCore1 clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 75 "" out

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
          ( replicate (SNat :: SNat 554) 1
              ++ replicate (SNat :: SNat 2880) 1
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ replicate (SNat :: SNat 3040) 1
              ++ Vector.concatMap charToUartRx testOutput2
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
