{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Class.Counter (countSucc)
import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Prelude (Bit, BitVector, Clock, DomainPeriod, Enable, HiddenClockResetEnable, Index, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, bundle, bv2i, createDomain, enableGen, exposeClockResetEnable, knownVDomain, mealy, pack, register, replicate, resetGen, resize, unbundle, vName, vPeriod, vResetPolarity)
import Clash.Prelude.BlockRam (ResetStrategy (NoClearOnReset), blockRamU)
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
import GHC.TypeNats (type (<=))
import Ice40.Pll.Pad (pllPadPrim)
import System.Hclip (setClipboard)
import Util (BcdOrControl (..), Mealy, MoreOrDone (..), adapt, awaitBothT, bcd64T, bcdOrControlToAscii, blankLeadingZeros, charToUartRx, delayBufferVecT)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="Alchitry3", vResetPolarity=ActiveLow, vPeriod=30000}

-- part 1 approach:
-- A streaming solution is incredibly challenging here.  The biggest difficulty
-- is that we don't know what the bottom row will be. This means that even the
-- top most tree that is hidden from top/left/right, might still be visible
-- from the bottom, even many many lines later.
--
-- A simple example of this:
--
-- 99999
-- 99199
-- 99099
-- 99099
-- 99099
--
-- The 1 at the top middle is eventually visible, but we're not sure until we
-- get all the way to the end.  If the very bottom middle was a 9, then the 1
-- would be a candidate for the third and fourth line, and then eliminated by
-- the 5th.
--
-- Even more more complex, we could get up to _nine_ trees visible this way, as
-- new candidates don't necessarily block previous candidates.
--
-- Example:
--
-- 999
-- 999
-- 919
-- 989
-- 919
-- 979
-- 919
-- 969
-- 919
-- 959
-- 919
-- 949
-- 919
-- 939
-- 919
-- 929
-- 919
-- 919
-- 909
--
-- We see here that all trees above 1 and the final 1 are visible.  Every 1
-- except the last is a candidate for one row, until we see the tree in front
-- of it.
--
-- This means as we stream, we need a history of upto the previous 9
-- candidates. And they could be spread across multiple rows.  At this size, we
-- absolutely need to use block RAM, as this requires no fewer than 3600k
-- registers just for our buffer.
--
-- We also still have to ensure we never double count a candidate that was
-- already visible from the left, right, or top.
--
-- The resulting algorithm is just quite complex.
--
-- Since we don't even know what part 2 brings, we'll just solve part 1
-- niavely, by loading everything into memory and then checking each cell in
-- each direction one at a time.
--
-- But wait!  A final thought.  Perhaps we track uncounted candidates in a
-- BitVector 9.  Since a tie has no real effect, each height can have at most
-- one vesible tree.  So 000100010 would mean that a tree of height 8 and 4 are
-- candidates.  We don't log anything that's already been recorded as visible,
-- since those trees have no effect any more.
--
-- When we encounter a new bottom row tree, we blank all positions smaller than
-- it.  So if we saw a 5, we'd get 000000010.  If that tree isn't visible from
-- the left, right, or top, then it gets added (for 000010010 in this case).
--
-- If we do this, we can theoretically do the same thing for viewing from the
-- right side.  Basically we get a zipper effect, with n+1 trailing BitVectors,
-- n for the top, and 1 for the left.  Only other data we need is the tallest
-- tree "behind" the BitVector, since the first tallest will be visible, and
-- therefore not present in the BitVector.
--
-- Example of the zipper walking:
--
--            872
--
--            000
--            100
--            000
--        ... 000  ...
--            000
--            010
--            010
--            011
--            000
--
-- 8 001000100 X
--
-- This requires (n + 1) * (9 + 4) registers.  For 100, that's 1313.  So maybe
-- manage-able?
--
-- For performance, we need to ensure that we're always shifting our top vector
-- to put the operated on BitVector in the same position to minimize our logic.
--
-- SHOOT!  This doesn't work, because we can't dedup candidates from the right
-- and the bottom.
--
-- Back to the naive approach

parse :: BitVector 8 -> Maybe (MoreOrDone (Index 10))
parse 4 = Just Done
parse 10 =
  -- A right-most edge
  Nothing
parse x =
  Just (More (resize (bv2i (x - 48))))

data ScanDir
  = L
  | R
  | U
  | D
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

data ScanMode n
  = Receiving
  | GetNextHeight
  | Scanning (Index 10) ScanDir (Index n, Index n) Bool (Index n) (Unsigned 64)
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

data MoreOrLast a
  = MoreL a
  | Last a
  deriving stock (Generic, Show, Eq, Ord, Functor)
  deriving anyclass (NFData, NFDataX, ShowX)

-- OH GOD IT'S SO AWFUL
scanT ::
  KnownNat n =>
  (1 <= n) =>
  Mealy
    ((Index n, Index n), ScanMode n)
    (Maybe (MoreOrDone (Index 10)), Index 10)
    (Maybe (MoreOrLast (Unsigned 64, Bool)), (Index n, Index n), Maybe ((Index n, Index n), Index 10))
scanT s@(ptr, Receiving) (Nothing, _) =
  (s, (Nothing, ptr, Nothing))
scanT (ptr, Receiving) (Just (More height), _) =
  ((countSucc ptr, Receiving), (Nothing, ptr, Just (ptr, height)))
scanT (_, Receiving) (Just Done, _) =
  (((0, 0), GetNextHeight), (Nothing, (0, 0), Nothing))
scanT (target, GetNextHeight) (_, height) =
  ((target, Scanning height L target False 0 1), (Nothing, target, Nothing))
scanT (target, Scanning height L ptr@(col, row) prevVisible viewCount score) (_, nextHeight) =
  let knownVisible = row == minBound && (ptr == target || height > nextHeight)
      knownHidden = ptr /= target && height <= nextHeight
   in if knownVisible || knownHidden
        then ((target, Scanning height R target (knownVisible || prevVisible) 0 (fromIntegral viewCount * score)), (Nothing, target, Nothing))
        else ((target, Scanning height L (col, pred row) prevVisible (viewCount + 1) score), (Nothing, (col, pred row), Nothing))
scanT (target, Scanning height R ptr@(col, row) prevVisible viewCount score) (_, nextHeight) =
  let knownVisible = row == maxBound && (ptr == target || height > nextHeight)
      knownHidden = ptr /= target && height <= nextHeight
   in if knownVisible || knownHidden
        then ((target, Scanning height U target (knownVisible || prevVisible) 0 (fromIntegral viewCount * score)), (Nothing, target, Nothing))
        else ((target, Scanning height R (col, succ row) prevVisible (viewCount + 1) score), (Nothing, (col, succ row), Nothing))
scanT (target, Scanning height U ptr@(col, row) prevVisible viewCount score) (_, nextHeight) =
  let knownVisible = col == minBound && (ptr == target || height > nextHeight)
      knownHidden = ptr /= target && height <= nextHeight
   in if knownVisible || knownHidden
        then ((target, Scanning height D target (knownVisible || prevVisible) 0 (fromIntegral viewCount * score)), (Nothing, target, Nothing))
        else ((target, Scanning height U (pred col, row) prevVisible (viewCount + 1) score), (Nothing, (pred col, row), Nothing))
scanT (target@(tc, tr), Scanning height D ptr@(col, row) prevVisible viewCount score) (_, nextHeight) =
  let knownVisible = col == maxBound && (ptr == target || height > nextHeight)
      knownHidden = ptr /= target && height <= nextHeight
   in if knownVisible || knownHidden
        then
          let out = (fromIntegral viewCount * score, knownVisible || prevVisible)
           in if tr == maxBound && tc == maxBound
                then (((0, 0), Receiving), (Just (Last out), (0, 0), Nothing))
                else
                  let target' = countSucc target
                   in ((target', GetNextHeight), (Just (MoreL out), target', Nothing))
        else ((target, Scanning height D (succ col, row) prevVisible (viewCount + 1) score), (Nothing, (succ col, row), Nothing))

sumT :: Unsigned 64 -> MoreOrLast Bool -> (Unsigned 64, Maybe (Unsigned 64))
sumT acc (MoreL False) = (acc, Nothing)
sumT acc (MoreL True) = (succ acc, Nothing)
sumT acc (Last False) = (0, Just acc)
sumT acc (Last True) = (0, Just (succ acc))

maxT :: Unsigned 64 -> MoreOrLast (Unsigned 64) -> (Unsigned 64, Maybe (Unsigned 64))
maxT acc (MoreL x) = (max acc x, Nothing)
maxT acc (Last x) = (0, Just (max acc x))

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64, Unsigned 64))
runCore mByteSig =
  let parsed = (=<<) parse <$> mByteSig
      (mMolScoreAndVisibility, readAddrUnpacked, mWriteUnpacked) = unbundle $ mealy scanT ((0 :: Index 99, 0), Receiving) $ bundle (parsed, ramOut)
      readAddr = pack <$> readAddrUnpacked
      mWrite = fmap (fmap (\(a, b) -> (pack a, b))) mWriteUnpacked
      ramOut = blockRamU NoClearOnReset (SNat :: SNat 16384) undefined readAddr mWrite
      part1 = join <$> mealy (adapt sumT) 0 (fmap (fmap (fmap snd)) mMolScoreAndVisibility)
      part2 = join <$> mealy (adapt maxT) 0 (fmap (fmap (fmap fst)) mMolScoreAndVisibility)
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
testOutput1 = $(listToVecTH "21")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "0")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "")
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
          ( replicate (SNat :: SNat 165) 1
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ Vector.concatMap charToUartRx testOutput2
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
