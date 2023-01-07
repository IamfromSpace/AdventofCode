{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Prelude (Bit, BitPack, BitVector, Clock, DomainPeriod, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, Signed, System, Unsigned, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, mealy, pack, register, replicate, resetGen, resize, unbundle, vName, vPeriod, vResetPolarity, (!), replaceBit)
import Clash.Prelude.BlockRam (ResetStrategy (ClearOnReset), blockRam1, readNew)
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (++))
import qualified Clash.Sized.Vector as Vector
import Clash.WaveDrom (BitsWave (BitsWave), ShowWave (ShowWave), ToWave, render, wavedromWithClock)
import Clash.XException (NFDataX, ShowX)
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Data.Bits (shiftR)
import Data.ByteString.UTF8 ()
import Data.Char (chr, ord)
import Data.Group (Group (invert))
import qualified Data.List as List
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
  Mealy
    (V2 3)
    (MoreOrDone (V2 3))
    (MoreOrDone (V2 3))
followT _ Done = (mempty, Done)
followT tailToHead (More v) =
  let headMoved = tailToHead <> v
      toFollow = followingVector headMoved
      tailToHead' = invert toFollow <> headMoved
   in (tailToHead', More toFollow)

trackTailT ::
  KnownNat n =>
  Mealy
    (V2 n)
    (MoreOrDone (V2 3))
    (MoreOrDone (V2 n))
trackTailT _ Done = (mempty, Done)
trackTailT originToTail (More toFollow) =
  let originToTail' = originToTail <> fmap (fmap (fmap resize)) toFollow
   in (originToTail', More originToTail')

-- TODO: There is definitely not enough RAM for this.
--
-- Something like a HashSet/trie requires us to store the deduped value itelf,
-- plus pointers, with 5-11k items of 18-bits and pointers of ~13-bits, this
-- just isn't possible.
--
-- Using randomness gets killed by the birthday problem right out of the gate.
-- For our maximum RAM we can't even go above 52 items before we have a 1%
-- chance of collision.  By 430 we're at a 50/50 of getting a correct answer.
-- There's just no way to slice this (such as a combo of randomeness and
-- non-randomness) that doesn't yield collisions with a high probability.  We
-- have ~3000k raw elements that exceed our naive bounds.
--
-- Since we've got 1/2 the needed ram (and only a 1/4 utilizable by synthesis
-- without extra work), we could consider just counting by quadrant and running
-- the input through 4 times.
--
-- It is possible to buffer the entire input, replay it 4 times, and check a
-- different quadrant each time.  The input is about ~22kb if you consider 7b
-- for the iteration count and 2b per vector dimension.  That 7b could even
-- shrink to 5b since we don't ever actually see both digits utilized (no
-- iteration count is over 20).  That's as low as 18kb.
--
-- TODO: This only works exactly once!
-- Receipt of a Done needs to reset all positions
--
-- TODO: Type params for this for arbitrary depth and width
deduplicateT :: BitPack a => a -> Mealy (Maybe (MoreOrDone a)) (Maybe (MoreOrDone a), BitVector 256) (Maybe (MoreOrDone a), BitVector 8, Maybe (BitVector 8, BitVector 256))
deduplicateT defaultRead prev (next, prevWasPresentBv) =
  let output =
        case prev of
          Just (More x) ->
            -- take the bottom bits to get the BV address
            let index = resize (pack x) :: BitVector 8
            in if (prevWasPresentBv ! index) == 1 then
              Nothing
            else
              prev
          _ -> prev
      readAddr =
        let v = case next of
              Just (More x) -> x
              _ -> defaultRead
        -- take the top bits to get the RAM address
        in resize (shiftR (pack v) 8) :: BitVector 8
      mWrite =
        case prev of
          Just (More x) ->
            -- TODO: BitPack.split is probably the way to go and should do some
            -- type wizardry for us.
            let packed = pack x
                index = resize packed :: BitVector 8
                bv' = replaceBit index 1 prevWasPresentBv
                ramAddr = resize (shiftR packed 8) :: BitVector 8
             in Just (ramAddr, bv')
          _ -> Nothing
  in
  ( next,
    ( output,
      readAddr,
      mWrite
    )
  )

countEventsT :: Mealy (Unsigned 64) (MoreOrDone a) (Maybe (Unsigned 64))
countEventsT acc Done = (0, Just acc)
countEventsT acc (More _) = (acc + 1, Nothing)

deduplicate :: HiddenClockResetEnable dom => BitPack a => NFDataX a => a -> Signal dom (Maybe (MoreOrDone a)) -> Signal dom (Maybe (MoreOrDone a))
deduplicate defaultRead mA =
  let (out, readAddr, write) = unbundle $ mealy (deduplicateT defaultRead) Nothing (bundle (mA, ramOut))
      ramOut = readNew (blockRam1 ClearOnReset (SNat :: SNat 256) 0) readAddr write
   in out

biggestBoundT ::
  KnownNat n =>
  Mealy
    (Unsigned 64)
    (Maybe (MoreOrDone (V2 n)))
    (Maybe (Unsigned 64))
biggestBoundT acc Nothing = (acc, Nothing)
biggestBoundT acc (Just (Done)) = (0, Just acc)
biggestBoundT acc (Just (More (Vector (Sum {getSum = x} :> Sum {getSum = y} :> Nil)))) =
  (max acc (fromIntegral (max (abs x) (abs y))), Nothing)

isOutsideOf8Bit :: KnownNat n => (MoreOrDone (V2 n)) -> Maybe (MoreOrDone ())
isOutsideOf8Bit Done = Just Done
isOutsideOf8Bit (More (Vector (Sum {getSum = x} :> Sum {getSum = y} :> Nil))) = if x > 126 || x < -127 || y > 126 || y < -127 then Just (More ()) else Nothing

isOutsideOf7Bit :: KnownNat n => (MoreOrDone (V2 n)) -> Maybe (MoreOrDone ())
isOutsideOf7Bit Done = Just Done
isOutsideOf7Bit (More (Vector (Sum {getSum = x} :> Sum {getSum = y} :> Nil))) = if x > 62 || x < -63 || y > 62 || y < -63 then Just (More ()) else Nothing

-- Pre-filter out items base on their top bits
-- Note: technically this could work for _any_ division of 4^n
selectQuadrant :: BitVector 2 -> MoreOrDone (V2 9) -> Maybe (MoreOrDone (BitVector 16))
selectQuadrant _ Done = Just Done
selectQuadrant quad (More x) =
  if quad == resize (shiftR (pack x :: BitVector 18) 16)
    then Just (More (resize (pack x)))
    else Nothing

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore1 mBytes =
  let -- Hack to manually run 4 times, testing a different quadrant each time
      selectedQuadrant = 3

      -- Hack because since we don't even have enough RAM to solve part 1, we
      -- have to recompile specifically for part 2 as well.
      ropeLength = 9

      parsed = join <$> mealy (adapt parseT) StartOfLine mBytes
      singleInstructions = fmap (fmap (fmap (fmap (fmap (fmap resize))))) $ mealy expandT Nothing parsed
      tailMoves =
        List.foldl1
          (.)
          ( List.replicate
              ropeLength
              (register Nothing . mealy (adapt followT) mempty)
          )
          singleInstructions
      tailPositions = mealy (adapt trackTailT) mempty tailMoves
      tailPositionsInQuadrant = fmap ((=<<) (selectQuadrant selectedQuadrant)) tailPositions
      uniqueTailPositions = deduplicate 0 tailPositionsInQuadrant
      out = join <$> mealy (adapt countEventsT) 0 uniqueTailPositions
      -- Figure out how many (raw) events exceed our 1/2 max RAM
      -- tooBig = fmap ((=<<) isOutsideOf8Bit) tailPositions
      -- out = join <$> mealy (adapt countEventsT) 0 tooBig

      -- Figure out how many (raw) events exceed 1/4 our max RAM
      -- tooBig = fmap ((=<<) isOutsideOf7Bit) tailPositions
      -- out = join <$> mealy (adapt countEventsT) 0 tooBig

      -- Figure out how many bits are needed
      -- out = mealy biggestBoundT 0 tailPositions

      -- Figure out how many raw events there are
      -- out = join <$> mealy (adapt countEventsT) 0 tailPositions
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

testInputCore2 :: Vec _ (Maybe (BitVector 8))
testInputCore2 =
  -- A couple hacks here since we can't actually accept data this quickly, need
  -- to end on 1 instruction or we miss the Done signal, and we add spaces to
  -- not read to fast since they act as a no-op
  let raw = $(listToVecTH "R 5\nU   8\nL      8\nD      3\nR 17\nD                10\nL        25\nU                       19\nU                 1\n")
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

copyWavedromDedup :: IO ()
copyWavedromDedup =
  let en = enableGen
      clk = tbClockGen (False <$ out)
      rst = resetGen :: Reset System
      inputSignal = stimuliGenerator clk rst (Just (More (0 :: Unsigned 16)) :>  Just (More 518) :> Just (More 518) :>  Just (More 1) :> Just (More 0) :> Just Done :> Nothing :> Nil)
      out = fmap ShowWave (exposeClockResetEnable (deduplicate (0 :: Unsigned 16)) clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 75 "" out


copyWavedrom :: IO ()
copyWavedrom =
  let en = enableGen
      clk = tbClockGen (False <$ out)
      rst = resetGen :: Reset System
      inputSignal = stimuliGenerator clk rst testInputCore
      out = InOut <$> fmap BitsWave inputSignal <*> fmap ShowWave (exposeClockResetEnable runCore clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 75 "" out

copyWavedrom2 :: IO ()
copyWavedrom2 =
  let en = enableGen
      clk = tbClockGen (False <$ out)
      rst = resetGen :: Reset System
      inputSignal = stimuliGenerator clk rst testInputCore2
      out = InOut <$> fmap BitsWave inputSignal <*> fmap ShowWave (exposeClockResetEnable runCore clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 150 "" out

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
