{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Num.Saturating (Saturating)
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

spriteCollision :: Signed 32 -> Wrapping (Index 40) -> Bool
spriteCollision x n =
  let n' = fromIntegral n
   in n' == x || n' - 1 == x || n' + 1 == x

drawT :: Mealy (Signed 32, Wrapping (Index 40)) Inst (Bool, Maybe Bool)
drawT (x, n) Noop =
  ( (x, n + 1),
    (spriteCollision x n, Nothing)
  )
drawT (x, n) (Addx offset) =
  ( (x + resize offset, n + 2),
    (spriteCollision x n, Just (spriteCollision x (n + 1)))
  )

collect240T ::
  Mealy
    (Saturating (Index 240), Vec 240 Bool)
    (Bool, Maybe Bool)
    (Maybe (Vec 240 Bool))
collect240T (n, v) (x0, mx1) =
  let (n', v') = case mx1 of
        Just x1 -> (n + 2, fst $ Vector.shiftInAtN v (x0 :> x1 :> Nil))
        Nothing -> (n + 1, fst $ Vector.shiftInAtN v (x0 :> Nil))
   in ((n', v'), if n' == maxBound then Just v' else Nothing)

data BcdOrControlOrCrt
  = Boc BcdOrControl
  | Crt Bool
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

bcdOrControlOrCrtToAscii :: BcdOrControlOrCrt -> BitVector 8
bcdOrControlOrCrtToAscii = \case
  Boc b -> bcdOrControlToAscii b
  Crt True -> 35
  Crt False -> 32

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe Inst) -> Signal dom (Maybe (Signed 32))
runCore1 mInsts =
  let mSigStrengths = join <$> mealy (adapt executeT) (1, 1, 20) mInsts
      mSigStrengths' = register Nothing mSigStrengths
      out = join <$> mealy (adapt sum6T) (0, 0) mSigStrengths'
   in out

bool240ToBcc :: Vec 240 Bool -> Vec 246 BcdOrControlOrCrt
bool240ToBcc =
  Vector.concatMap (\v' -> fmap Crt v' ++ (Boc Return :> Nil))
    . Vector.unconcat (SNat :: SNat 40)

runCore2 :: HiddenClockResetEnable dom => Signal dom (Maybe Inst) -> Signal dom (Maybe (Vec 246 BcdOrControlOrCrt))
runCore2 mInsts =
  let pixels = register Nothing $ mealy (adapt drawT) (1, 0) mInsts
      collected = join <$> mealy (adapt collect240T) (0, Vector.repeat False) pixels
      out = fmap (fmap bool240ToBcc) collected
   in out

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Vec _ (Maybe BcdOrControlOrCrt)))
runCore mBytes =
  let mInsts = join <$> mealy (adapt parseT) StartOfLine mBytes
      mInsts' = register Nothing mInsts
      part1 = register Nothing $ runCore1 mInsts'
      part1Bcd = register Nothing $ bcd1SignedN part1
      part1BcdOrControl = register Nothing $ fmap (fmap (\(isNegative, bcds) -> (if isNegative then Just Negate else Nothing) :> blankLeadingZeros bcds)) part1Bcd
      part2 = runCore2 mInsts'
      texts = register Nothing $ mealy awaitBothT (Nothing, Nothing) (bundle (part1BcdOrControl, part2))
      out = register Nothing $ fmap (fmap (\(a, b) -> fmap (fmap Boc) a ++ (Just (Boc Return) :> Nil) ++ fmap Just b ++ (Just (Boc Eot) :> Nil))) texts
   in out

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom Bit -> Signal dom Bit
run =
  fst
    . uartTx (SNat :: SNat 2083333)
    . register Nothing
    . fmap (fmap bcdOrControlOrCrtToAscii)
    . register Nothing
    . fmap join
    . register Nothing
    . mealy (delayBufferVecT (SNat :: SNat 160) (SNat :: SNat 259)) Nothing
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
