{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Prelude (Bit, BitVector, Clock, DomainPeriod, Enable, HiddenClockResetEnable, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, mealy, register, replicate, resetGen, vName, vPeriod, vResetPolarity, Index, KnownNat)
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
import GHC.TypeNats (type (+))
import Ice40.Pll.Pad (pllPadPrim)
import System.Hclip (setClipboard)
import Util (BcdOrControl (..), awaitBothT, bcd64T, bcdOrControlToAscii, blankLeadingZeros, charToUartRx, delayBufferVecT)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="Alchitry3", vResetPolarity=ActiveLow, vPeriod=30000}

-- part 1: A naive approach would have us buffer four items, and then
-- combinatonally compare everything.  This yields O(n^2) comparisons.
-- However, we can instead buffer the items _and_ the number of distinct items
-- behind it.  So each current item shifts and increments if a) all previous
-- items were distinct b) it's distinct from the current item.  We feed out the
-- oldest value and it's distinction count.  This is then fed into a new
-- accumulator that counts when it sees at least 3, then at least 2, then at
-- least 1.  This only does O(n) comparisons.

data DistinctStatus
  = Running
  | Flushing
  deriving stock (Generic, Eq, Show, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

data DistinctState n m = DistinctState
  { status :: DistinctStatus
  , buffer :: Vec n (Maybe (BitVector 8, Index m))
  }
  deriving stock (Generic, Eq, Show, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

distinctNT :: KnownNat n => DistinctState n (n + 1) -> Maybe (BitVector 8) -> (DistinctState n (n + 1), Maybe (BitVector 8, Index (n + 1)))
distinctNT s@DistinctState { status = Running } Nothing =
  (s, Nothing)
distinctNT s@DistinctState { status = Running } (Just 4) =
  (s { status = Flushing }, Nothing)
distinctNT s@DistinctState { status = Running, buffer } (Just new) =
  let
    withNewCounts = Vector.imap (\i -> fmap (\(x, xn) -> (x, if x /= new && xn == fromIntegral i then xn + 1 else xn))) buffer
    (newBuffer, out) = Vector.shiftInAt0 withNewCounts (Just (new, 0) :> Nil)
  in
    (s { buffer = newBuffer }, Vector.head out)
distinctNT s@DistinctState { status = Flushing, buffer } _ =
  let
    (newBuffer, out) = Vector.shiftInAt0 buffer (Nothing :> Nil)
  in
    (s { buffer = newBuffer }, Vector.head out)

detectorT :: KnownNat n => Maybe (Unsigned 64, Index n) -> Maybe (BitVector 8, Index n) -> (Maybe (Unsigned 64, Index n), Maybe (Unsigned 64))
detectorT Nothing _ =
  -- Dormant once finished
  (Nothing, Nothing)
detectorT s Nothing = (s, Nothing)
detectorT (Just (n, distinctCharCount)) (Just (_, next)) =
  let
    distinctCharCount' = if next >= maxBound - distinctCharCount then distinctCharCount + 1 else 0
  in
    if distinctCharCount' == maxBound then
      -- We detect one early (we don't have to see the four one, we know it's
      -- distinct, and we're zero indexed, so we add 2 to account for these
      -- factors.
      (Nothing, Just (n + 2))
    else
      (Just (n + 1, distinctCharCount'), Nothing)

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore1 =
  mealy detectorT (Just (0, 0)) . mealy distinctNT (DistinctState Running (Vector.replicate (SNat :: SNat 3) Nothing))

runCore2 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore2 =
  mealy detectorT (Just (0, 0)) . mealy distinctNT (DistinctState Running (Vector.replicate (SNat :: SNat 13) Nothing))


runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64, Unsigned 64))
runCore mByteSig =
  let part1 = runCore1 mByteSig
      part2 = runCore2 mByteSig
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
testOutput1 = $(listToVecTH "10")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "29")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "zcfzfwzzqfraaaaabcdefghijklmn")
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
          ( replicate (SNat :: SNat 5682) 1
              ++ replicate (SNat :: SNat 2880) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ replicate (SNat :: SNat 2880) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput2
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
