{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Num.Wrapping (Wrapping)
import Clash.Prelude (Bit, BitVector, Clock, DomainPeriod, Enable, HiddenClockResetEnable, Index, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, Signed, System, Unsigned, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, mealy, register, replicate, resetGen, resize, vName, vPeriod, vResetPolarity)
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
import Util (BcdOrControl (..), Mealy, adapt, awaitBothT, bcd1SignedN, bcdOrControlToAscii, blankLeadingZeros, charToUartRx, delayBufferVecT)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="Alchitry3", vResetPolarity=ActiveLow, vPeriod=30000}

data ParseProgress
  = StartOfLine
  | IsAddx
  | AddxReady
  | AddxGo Bool (Unsigned 7)
  | AwaitNewLine
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

parseT :: Mealy ParseProgress (BitVector 8) (Maybe Inst)
parseT AwaitNewLine 10 = (StartOfLine, Nothing)
parseT AwaitNewLine _ = (AwaitNewLine, Nothing)
parseT StartOfLine 110 = (AwaitNewLine, Just Noop)
parseT StartOfLine 97 = (IsAddx, Nothing)
parseT IsAddx 32 = (AddxReady, Nothing)
parseT IsAddx _ = (IsAddx, Nothing)
parseT AddxReady 45 = (AddxGo True 0, Nothing)
parseT AddxReady n = (AddxGo False (fromIntegral (n - 48)), Nothing)
parseT (AddxGo isNegative x) 10 =
  (StartOfLine, Just (Addx (fromIntegral x * if isNegative then -1 else 1)))
parseT (AddxGo isNegative x) y =
  (AddxGo isNegative (10 * x + fromIntegral (y - 48)), Nothing)
parseT StartOfLine 10 = (StartOfLine, Nothing)
parseT StartOfLine 4 = (StartOfLine, Nothing)
parseT StartOfLine c = error ("Unexpected character " <> show c <> " at start of line")

data Inst
  = Noop
  | Addx (Signed 8)
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

-- Need to know when done
executeT :: Mealy (Unsigned 32, Signed 32, Wrapping (Index 40)) Inst (Maybe (Signed 32))
executeT (cycleCount, x, n) Noop =
  ( (cycleCount + 1, x, n + 1),
    if n == maxBound
      then Just (fromIntegral cycleCount * x)
      else Nothing
  )
executeT (cycleCount, x, n) (Addx offset) =
  ( (cycleCount + 2, x + resize offset, n + 2),
    if n == maxBound
      then Just (fromIntegral cycleCount * x)
      else
        if n + 1 == maxBound
          then Just (fromIntegral (cycleCount + 1) * x)
          else Nothing
  )

sum6T :: Mealy (Index 6, Signed 32) (Signed 32) (Maybe (Signed 32))
sum6T (n, acc) next =
  let acc' = acc + next
   in if n == maxBound
        then ((0, 0), Just acc')
        else ((n + 1, acc'), Nothing)

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Signed 32))
runCore1 mBytes =
  let mInsts = join <$> mealy (adapt parseT) StartOfLine mBytes
      mInsts' = register Nothing mInsts
      mSigStrengths = join <$> mealy (adapt executeT) (1, 1, 20) mInsts'
      mSigStrengths' = register Nothing mSigStrengths
      out = join <$> mealy (adapt sum6T) (0, 0) mSigStrengths'
   in out

runCore2 :: HiddenClockResetEnable dom => Signal dom () -> Signal dom (Maybe (Vec _ (Maybe BcdOrControl)))
runCore2 =
  pure (pure (pure Nil))

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Vec _ (Maybe BcdOrControl)))
runCore mBytes =
  let part1 = register Nothing $ runCore1 mBytes
      part1Bcd = register Nothing $ bcd1SignedN part1
      part1BcdOrControl = register Nothing $ fmap (fmap (\(isNegative, bcds) -> (if isNegative then Just Negate else Nothing) :> blankLeadingZeros bcds)) part1Bcd
      part2 = runCore2 (pure ())
      texts = register Nothing $ mealy awaitBothT (Nothing, Nothing) (bundle (part1BcdOrControl, part2))
      out = register Nothing $ fmap (fmap (\(a, b) -> a ++ (Just Return :> Nil) ++ b ++ (Just Return :> Just Eot :> Nil))) texts
   in out

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom Bit -> Signal dom Bit
run =
  fst
    . uartTx (SNat :: SNat 2083333)
    . register Nothing
    . fmap (fmap bcdOrControlToAscii)
    . register Nothing
    . fmap join
    . register Nothing
    . mealy (delayBufferVecT (SNat :: SNat 160) (SNat :: SNat 14)) Nothing
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

copyWavedromExecute :: IO ()
copyWavedromExecute =
  let en = enableGen
      clk = tbClockGen (False <$ out)
      rst = resetGen :: Reset System
      inputSignal = stimuliGenerator clk rst (Addx 15 :> Addx (-11) :> Addx 6 :> Addx (-3) :> Addx 5 :> Addx (-1) :> Addx (-8) :> Addx 13 :> Addx 4 :> Noop :> Addx (-1) :> Nil)
      out = InOut <$> fmap ShowWave inputSignal <*> fmap ShowWave (exposeClockResetEnable (mealy executeT (1, 1, 20)) clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 75 "" out

copyWavedrom1 :: IO ()
copyWavedrom1 =
  let en = enableGen
      clk = tbClockGen (False <$ out)
      rst = resetGen :: Reset System
      inputSignal = stimuliGenerator clk rst (fmap (Just . fromIntegral . ord) $(listToVecTH "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"))
      out = InOut <$> fmap (ShowWave . maybe "" ((\x -> [x]) . Data.Char.chr . fromIntegral)) inputSignal <*> fmap ShowWave (exposeClockResetEnable runCore1 clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 1000 "" out

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
