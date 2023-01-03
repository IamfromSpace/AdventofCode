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
import Util (BcdOrControl (..), Mealy, adapt, awaitBothT, bcd64T, bcdOrControlToAscii, blankLeadingZeros, charToUartRx, delayBufferVecT)
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

data ParseState
  = StartOfLine
  | AwaitingNewLine
  | Adding (Unsigned 32)
  | CdOrLs
  | COfCd
  | Cd
  | CdOneDot
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data MoreOrDone a
  = More a
  | Done
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

parseT :: Mealy ParseState (BitVector 8) (Maybe (MoreOrDone Event))
parseT _ 4 = (StartOfLine, Just Done)
parseT AwaitingNewLine 10 = (StartOfLine, Nothing)
parseT AwaitingNewLine _ = (AwaitingNewLine, Nothing)
parseT StartOfLine 36 = (CdOrLs, Nothing)
parseT CdOrLs 32 = (CdOrLs, Nothing)
parseT CdOrLs 99 = (COfCd, Nothing)
parseT CdOrLs _ = (AwaitingNewLine, Nothing)
parseT COfCd 100 = (Cd, Nothing)
parseT COfCd _ = error "Got a command starting with c that was not cd!"
parseT Cd 32 = (Cd, Nothing)
parseT Cd 46 = (CdOneDot, Nothing)
parseT CdOneDot 46 = (AwaitingNewLine, Just (More Pop))
parseT Cd _ = (CdOneDot, Nothing)
parseT CdOneDot _ = (AwaitingNewLine, Just (More Push))
parseT StartOfLine 100 = (AwaitingNewLine, Nothing)
parseT StartOfLine x = (Adding (resize ((unpack x :: Unsigned 8) - 48)), Nothing)
parseT (Adding acc) 32 = (AwaitingNewLine, Just (More (Add acc)))
parseT (Adding acc) x = (Adding (10 * acc + resize ((unpack x :: Unsigned 8) - 48)), Nothing)

data PopHelperState
  = Running
  | Popping
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

-- Upon seeing a done, this sends remaining pops
popHelperT :: Mealy (PopHelperState, Unsigned 8) (Maybe (MoreOrDone Event)) (Maybe (MoreOrDone Event))
popHelperT s@(Running, _) Nothing = (s, Nothing)
popHelperT (Running, n) (Just Done) = ((Popping, n), Nothing)
popHelperT (Running, n) (Just (More Push)) = ((Running, n + 1), Just (More Push))
popHelperT (Running, n) (Just (More Pop)) = ((Running, n - 1), Just (More Pop))
popHelperT s@(Running, _) (Just (More x)) = (s, Just (More x))
popHelperT (Popping, 0) Nothing = ((Running, 0), Just Done)
popHelperT (Popping, n) Nothing = ((Popping, n - 1), Just (More Pop))
popHelperT (Popping, _) (Just _) = error "popHelper buffer overrun!"

-- Emits:
--   - the possible write address/value
--   - the read address
--   - _Every_ directory's size
traverseT :: KnownNat n => Mealy (Index n, Unsigned 64) (Maybe (MoreOrDone Event), Unsigned 64) (Maybe (Index n, Unsigned 64), Index n, Maybe (MoreOrDone (Unsigned 64)))
traverseT _ (Just Done, _) = ((0, 0), (Nothing, 0, Just Done))
traverseT (ptr, acc) (Just (More Push), _) =
  let ptr' = ptr + 1
   in ((ptr', 0), (Just (ptr', acc), ptr', Nothing))
traverseT (ptr, acc) (Just (More (Add x)), _) =
  let acc' = acc + resize x
   in ((ptr, acc'), (Nothing, ptr, Nothing))
traverseT (ptr, acc) (Just (More Pop), v) =
  let ptr' = ptr - 1
   in ((ptr', acc + v), (Nothing, ptr', Just (More acc)))
traverseT s@(ptr, _) (Nothing, _) = (s, (Nothing, ptr, Nothing))

sumT :: Mealy (Unsigned 64) (MoreOrDone (Unsigned 64)) (Maybe (Unsigned 64))
sumT acc (More new) = (acc + new, Nothing)
sumT acc Done = (0, Just acc)

data ChooseDirMode
  = Ingesting (Unsigned 64) -- Final/largest (one implies the other)
  | Selecting
      (Unsigned 64) -- Space needed
      (Unsigned 64) -- Best candidate's size
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data ChooseDirState n = ChooseDirState
  { mode :: ChooseDirMode,
    pointer :: Index n
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

chooseDirT ::
  KnownNat n =>
  Mealy
    (ChooseDirState n)
    (Maybe (MoreOrDone (Unsigned 64)), Unsigned 64)
    (Maybe (Unsigned 64), Index n, Maybe (Index n, Unsigned 64))
chooseDirT s@ChooseDirState {mode = Ingesting _, pointer} (Nothing, _) =
  (s, (Nothing, pointer, Nothing))
chooseDirT s@ChooseDirState {mode = Ingesting largestDirSeen, pointer} (Just Done, _) =
  (s {mode = Selecting (largestDirSeen - 40000000) maxBound}, (Nothing, pointer, Nothing))
chooseDirT s@ChooseDirState {mode = Ingesting _, pointer} (Just (More newerSize), _) =
  let pointer' = pointer + 1
   in ( s
          { mode = Ingesting newerSize,
            pointer = pointer'
          },
        (Nothing, pointer', Just (pointer', newerSize))
      )
chooseDirT s@ChooseDirState {mode = Selecting spaceNeeded currCandidateSize, pointer} (_, altCandidateSize) =
  let currCandidateSize' =
        if altCandidateSize >= spaceNeeded
          && altCandidateSize < currCandidateSize
          then altCandidateSize
          else currCandidateSize
      pointer' = pointer - 1
   in if pointer' == 0
        then
          ( ChooseDirState (Ingesting 0) 0,
            (Just currCandidateSize', 0, Nothing)
          )
        else
          ( s
              { mode = Selecting spaceNeeded currCandidateSize',
                pointer = pointer'
              },
            (Nothing, pointer', Nothing)
          )

emitAllDirs :: HiddenClockResetEnable dom => Signal dom (Maybe (MoreOrDone Event)) -> Signal dom (Maybe (MoreOrDone (Unsigned 64)))
emitAllDirs mEventSig =
  let (mWrite, readAddr, out) =
        unbundle $ mealy traverseT (0 :: Index 256, 0) (bundle (mEventSig, ramOut))
      ramOut = readNew (blockRamU NoClearOnReset (SNat :: SNat 256) undefined) readAddr mWrite
   in out

sumDirsAtMost100000 :: HiddenClockResetEnable dom => Signal dom (Maybe (MoreOrDone (Unsigned 64))) -> Signal dom (Maybe (Unsigned 64))
sumDirsAtMost100000 allDirs =
  let filteredDirs =
        (=<<)
          ( \case
              More x -> if x > 100000 then Nothing else Just (More x)
              Done -> Just Done
          )
          <$> allDirs
      out = join <$> mealy (adapt sumT) 0 filteredDirs
   in out

chooseDir :: HiddenClockResetEnable dom => Signal dom (Maybe (MoreOrDone (Unsigned 64))) -> Signal dom (Maybe (Unsigned 64))
chooseDir mEventSig =
  let (out, readAddr, mWrite) =
        unbundle $ mealy chooseDirT (ChooseDirState (Ingesting 0) (0 :: Index 256)) (bundle (mEventSig, ramOut))
      ramOut = readNew (blockRamU NoClearOnReset (SNat :: SNat 256) undefined) readAddr mWrite
   in out

-- Now just used for testing
runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe (MoreOrDone Event)) -> Signal dom (Maybe (Unsigned 64))
runCore1 =
  sumDirsAtMost100000 . emitAllDirs

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64, Unsigned 64))
runCore mByteSig =
  let parsed = join <$> mealy (adapt parseT) StartOfLine mByteSig
      helpedParsed = mealy popHelperT (Running, 0) parsed
      allDirs = emitAllDirs helpedParsed
      part1 = sumDirsAtMost100000 allDirs
      part2 = chooseDir allDirs
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
testOutput1 = $(listToVecTH "95437")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "24933642")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "$ cd /\ndir a\n40000000 x\n$ cd a\ndir b\n456 y\n$ cd ..")
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
      inputSignal = stimuliGenerator clk rst (fmap (Just . More) (Add 10 :> Add 12 :> Push :> Add 9 :> Push :> Push :> Add 8 :> Pop :> Add 6 :> Pop :> Add 4 :> Pop :> Pop :> Nil) ++ (Just Done :> Nothing :> Nil))
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
          ( replicate (SNat :: SNat 30961) 1
              ++ replicate (SNat :: SNat 2400) 1
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ replicate (SNat :: SNat 1920) 1
              ++ Vector.concatMap charToUartRx testOutput2
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
