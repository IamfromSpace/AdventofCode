{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Prelude (Bit, BitPack, BitVector, Clock, DomainPeriod, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, Signed, System, Unsigned, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, mealy, pack, register, replicate, resetGen, resize, unbundle, vName, vPeriod, vResetPolarity)
import Clash.Prelude.BlockRam (ResetStrategy (ClearOnReset), blockRam1, readNew)
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (++))
import qualified Clash.Sized.Vector as Vector
import Clash.WaveDrom (BitsWave (BitsWave), ShowWave (ShowWave), ToWave, render, wavedromWithClock)
import Clash.XException (NFDataX, ShowX)
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Data.ByteString.UTF8 ()
import Data.Char (chr, ord)
import Data.Group (Group (invert))
import Data.Monoid (Sum (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Ice40.Pll.Pad (pllPadPrim)
import System.Hclip (setClipboard)
import Util (BcdOrControl (..), Mealy, MoreOrDone (..), Vector (..), adapt, awaitBothT, bcd64T, bcdOrControlToAscii, blankLeadingZeros, charToUartRx, delayBufferVecT)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="Alchitry3", vResetPolarity=ActiveLow, vPeriod=30000}

instance BitPack a => BitPack (Sum a)

type V2 n = Vector (Vec 2 (Sum (Signed n)))

data ParseState
  = StartOfLine
  | Digits (V2 2) (Unsigned 7)
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX)

parseT :: Mealy ParseState (BitVector 8) (Maybe (MoreOrDone (V2 2, Unsigned 7)))
parseT StartOfLine 82 =
  -- R
  (Digits (Vector (1 :> 0 :> Nil)) 0, Nothing)
parseT StartOfLine 76 =
  -- L
  (Digits (Vector (-1 :> 0 :> Nil)) 0, Nothing)
parseT StartOfLine 85 =
  -- U
  (Digits (Vector (0 :> 1 :> Nil)) 0, Nothing)
parseT StartOfLine 68 =
  -- D
  (Digits (Vector (0 :> -1 :> Nil)) 0, Nothing)
parseT StartOfLine 4 = (StartOfLine, Just Done)
parseT StartOfLine _ = error "bad direction!"
parseT s@(Digits _ _) 32 = (s, Nothing)
parseT (Digits v acc) 10 = (StartOfLine, Just (More (v, acc)))
parseT (Digits v acc) digit =
  (Digits v (acc * 10 + fromIntegral (digit - 48)), Nothing)

expandT ::
  Mealy
    (Maybe (V2 2, Unsigned 7))
    (Maybe (MoreOrDone (V2 2, Unsigned 7)))
    (Maybe (MoreOrDone (V2 2)))
expandT (Just (v, n)) _ = (if n == 1 then Nothing else Just (v, n - 1), Just (More v))
expandT Nothing (Just Done) = (Nothing, Just Done)
expandT Nothing (Just (More (v, n))) = (if n == 1 then Nothing else Just (v, n - 1), Just (More v))
expandT Nothing Nothing = (Nothing, Nothing)

followingVector :: V2 3 -> V2 3
followingVector (Vector (2 :> 0 :> Nil)) =
  Vector (1 :> 0 :> Nil)
followingVector (Vector (2 :> x :> Nil)) =
  if x > 0
    then Vector (1 :> 1 :> Nil)
    else Vector (1 :> -1 :> Nil)
followingVector (Vector (-2 :> 0 :> Nil)) =
  Vector (-1 :> 0 :> Nil)
followingVector (Vector (-2 :> x :> Nil)) =
  if x > 0
    then Vector (-1 :> 1 :> Nil)
    else Vector (-1 :> -1 :> Nil)
followingVector (Vector (-1 :> 2 :> Nil)) =
  Vector (-1 :> 1 :> Nil)
followingVector (Vector (0 :> 2 :> Nil)) =
  Vector (0 :> 1 :> Nil)
followingVector (Vector (1 :> 2 :> Nil)) =
  Vector (1 :> 1 :> Nil)
followingVector (Vector (-1 :> -2 :> Nil)) =
  Vector (-1 :> -1 :> Nil)
followingVector (Vector (0 :> -2 :> Nil)) =
  Vector (0 :> -1 :> Nil)
followingVector (Vector (1 :> -2 :> Nil)) =
  Vector (1 :> -1 :> Nil)
followingVector _ = mempty

data FollowState n = FollowState
  { originToTail :: V2 n,
    tailToHead :: V2 3
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX)

followT ::
  KnownNat n =>
  Mealy
    (FollowState n)
    (MoreOrDone (V2 2))
    (Maybe (MoreOrDone (V2 n)))
followT _ Done = (FollowState {originToTail = mempty, tailToHead = mempty}, Just Done)
followT FollowState {originToTail, tailToHead} (More v) =
  let headMoved = tailToHead <> fmap (fmap (fmap resize)) v
      toFollow = followingVector headMoved
      originToTail' = originToTail <> fmap (fmap (fmap resize)) toFollow
      tailToHead' = invert toFollow <> headMoved
   in (FollowState {originToTail = originToTail', tailToHead = tailToHead'}, Just (More originToTail'))

-- TODO: There is definitely not enough RAM for this
-- TODO: This only works exactly once!
-- Receipt of a Done needs to reset all positions
deduplicateT :: Eq a => a -> Mealy (Maybe (MoreOrDone a)) (Maybe (MoreOrDone a), Bool) (Maybe (MoreOrDone a), a, Maybe (a, Bool))
deduplicateT defaultRead prev (next, prevWasPresent) =
  ( next,
    ( prev >>= (\x -> if not prevWasPresent || x == Done then Just x else Nothing),
      case next of
        Just (More x) -> x
        _ -> defaultRead,
      case prev of
        Just (More x) -> Just (x, True)
        _ -> Nothing
    )
  )

countEventsT :: Mealy (Unsigned 64) (MoreOrDone a) (Maybe (Unsigned 64))
countEventsT acc Done = (0, Just acc)
countEventsT acc (More _) = (acc + 1, Nothing)

deduplicate :: Eq a => HiddenClockResetEnable dom => BitPack a => NFDataX a => a -> Signal dom (Maybe (MoreOrDone a)) -> Signal dom (Maybe (MoreOrDone a))
deduplicate defaultRead mA =
  let (out, readAddr, write) = unbundle $ mealy (deduplicateT defaultRead) Nothing (bundle (mA, ramOut))
      -- this is everything we got and absolutely may not be enough RAM
      -- Almost certainly too small because it means we can only go 128 to the
      -- left and right :(
      ramOut = readNew (blockRam1 ClearOnReset (SNat :: SNat 64000) False) (pack <$> readAddr) (fmap (fmap (\(a, b) -> (pack a, b))) write)
   in out

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore1 mBytes =
  let parsed = join <$> mealy (adapt parseT) StartOfLine mBytes
      singleInstructions = mealy expandT Nothing parsed
      tailPositions = join <$> mealy (adapt followT) (FollowState {originToTail = mempty :: V2 8, tailToHead = mempty}) singleInstructions
      uniqueTailPositions = deduplicate (Vector (0 :> 0 :> Nil)) tailPositions
      out = join <$> mealy (adapt countEventsT) 0 uniqueTailPositions
   in out

runCore2 :: HiddenClockResetEnable dom => Signal dom () -> Signal dom (Maybe (Unsigned 64))
runCore2 =
  pure (pure (pure 0))

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64, Unsigned 64))
runCore mBytes =
  let part1 = runCore1 mBytes
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
testOutput1 = $(listToVecTH "")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  -- A couple hacks here since we can't actually accept data this quickly, need
  -- to end on 1 instruction or we miss the Done signal, and we add spaces to
  -- not read to fast since they act as a no-op
  let raw = $(listToVecTH "R 4\nU     4\nL     3\nD 1\nR 4\nD     1\nL 5\nR        1\nR 1\n")
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
