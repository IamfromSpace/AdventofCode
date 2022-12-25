{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Class.BitPack (BitPack (pack, unpack), (!))
import Clash.Class.Resize (resize)
import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen, tbSystemClockGen)
import Clash.Prelude (Bit, BitVector, CLog, Clock, DomainPeriod, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, addSNat, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, lengthS, mealy, register, repeat, replaceBit, replicate, resetGen, snatToNum, subSNat, unbundle, vName, vPeriod, vResetPolarity, Index)
import Clash.Prelude.BlockRam (trueDualPortBlockRam, RamOp(..))
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (!!), (++))
import qualified Clash.Sized.Vector as Vector
import Clash.WaveDrom (ToWave, WithBits (WithBits), render, wavedromWithClock)
import Clash.XException (NFDataX, ShowX)
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow (arr, returnA, (&&&), (***), (<+>), (<<<), (>>>), (>>^), (|||))
import Control.DeepSeq (NFData)
import Control.Monad (join)
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Char (chr, ord)
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (fold, toList)
import Data.Group (invert, (~~))
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import GHC.TypeNats (type (+))
import GHC.TypeLits.Extra (CLog)
import Ice40.Pll.Pad (pllPadPrim)
import System.Hclip (setClipboard)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="Alchitry8", vResetPolarity=ActiveLow, vPeriod=80000}

data Feed
  = Done
  | NewLine
  | Char (Unsigned 6)
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

parse :: BitVector 8 -> Feed
parse 4 = Done
parse 10 = NewLine
parse x =
  Char (fromIntegral (if x >= 97 then x - 97 else x - 65 + 26))

data StoreTState addr =
  StoreTState
    { sLinePointer :: addr,
      sCharPointer :: addr,
      len :: Unsigned 6
    }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

storeT ::
  Num addr =>
  Bounded addr =>
  Eq addr =>
  StoreTState addr ->
  Maybe Feed ->
  ( StoreTState addr
  , ( Maybe (addr, (Bool, Unsigned 6))
    , Maybe (addr, Unsigned 6)
    , addr
    )
  )
storeT s@StoreTState { sLinePointer = slp } Nothing =
  (s, (Nothing, Nothing, slp))
storeT s@StoreTState { sLinePointer = slp, sCharPointer = scp, len } (Just (Char c)) =
  -- A new character was received, advance the character pointer and write it
  ( s { sCharPointer = if scp == maxBound then 0 else scp + 1, len = len + 1 }
  , (Nothing, Just (scp, c), slp)
  )
storeT s@StoreTState { sLinePointer = slp, len } (Just x) =
  -- We're at the end of the line (which might be the last).  We update our
  -- line queue entry and advance our pointer and reset our line length
  -- counter.
  let slp' = if slp == maxBound then 0 else slp + 1
  in  ( s { sLinePointer = slp', len = 0 }
      , (Just (slp, (x == Done, len `div` 2)), Nothing, slp')
      )

data RetrieveTState addr =
  RetrieveTState
    { rLinePointer :: addr,
      rCharPointer :: addr,
      charCount :: Unsigned 6,
      isLeft :: Bool,
      bv :: BitVector 52
    }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

-- Note our queue can't actually be completely full (can't distinguish full and
-- empty here)
retrieveT :: Bounded addr => Eq addr => Num addr => RetrieveTState addr -> ((Bool, Unsigned 6), Unsigned 6, addr) -> (RetrieveTState addr, (addr, addr, Maybe (Bool, BitVector 52)))
retrieveT s@RetrieveTState { rLinePointer = rlp, rCharPointer = rcp, charCount, bv, isLeft } ((isFinalLine, halfLineLen), charEntry, producerLinePointer) =
  if producerLinePointer /= rlp then
    let
      thisHalfDone = charCount + 1 == halfLineLen
      charCount' = if thisHalfDone then 0 else charCount + 1
      isLeft' = if thisHalfDone then not isLeft else isLeft
      isLineDone = not isLeft && thisHalfDone
      rlp' = if isLineDone then (if rlp == maxBound then 0 else rlp + 1) else rlp
      rcp' = if rcp == maxBound then 0 else rcp + 1
      -- NOTE: This makes up a giant muxer, which is really inefficient creates
      -- our bottleneck.  It's possibly faster to write into yet _another_
      -- block RAM, essentially using it as a very fast muxer.  The main issue
      -- is that there's a lot of latency to then walk over the completed
      -- BitVector (right?) as it's spread across 52 addresses, and we have to
      -- zero out those address when we're done.  So this is a substantial
      -- number of cycles.
      bvOut = replaceBit charEntry 1 bv
      bvStored = if thisHalfDone then 0 else bvOut
    in
      (s
        { rLinePointer = rlp'
        , rCharPointer = rcp'
        , charCount = charCount'
        , bv = bvStored
        , isLeft = isLeft'
        }
      , ( rlp'
        , rcp'
        , if thisHalfDone then Just (isFinalLine, bvOut) else Nothing
        )
      )
  else
    (s, (rlp, rcp, Nothing))

priorityT :: Maybe (BitVector 52) -> Maybe (Bool, BitVector 52) -> (Maybe (BitVector 52), Maybe (Bool, Unsigned 6))
priorityT Nothing x = (fmap snd x, Nothing)
priorityT (Just a) (Just (isDone, b)) = (Nothing, Just (isDone, (+) 1 $ fromIntegral $ Bits.countTrailingZeros (a .&. b)))
priorityT x Nothing = (x, Nothing)

sumT :: Unsigned 64 -> (Bool, Unsigned 6) -> (Unsigned 64, Maybe (Unsigned 64))
sumT acc (True, new) = (0, Just (acc + fromIntegral new))
sumT acc (False, new) = (acc + fromIntegral new, Nothing)

bcd64T ::
  Maybe (Unsigned 64, Unsigned 6, Vec 20 (Unsigned 4)) ->
  Maybe (Unsigned 64) ->
  ( Maybe (Unsigned 64, Unsigned 6, Vec 20 (Unsigned 4)),
    Maybe (Vec 20 (Unsigned 4))
  )
bcd64T Nothing Nothing =
  -- Nothing in progress, no new values to convert
  (Nothing, Nothing)
bcd64T Nothing (Just x) =
  -- New values to convert
  (Just (x, 63, repeat 0), Nothing)
bcd64T (Just (x, n, xBcd)) _ =
  -- Convert values in progress
  let xBcd' = convertStep (x ! n) xBcd
      done = n == 0
   in if done
        then (Nothing, Just xBcd')
        else (Just (x, n - 1, xBcd'), Nothing)

delayBufferVecT ::
  KnownNat (CLog 2 (n + 1)) =>
  KnownNat (CLog 2 (m + 1)) =>
  KnownNat m =>
  SNat (n + 1) ->
  -- ^ How many clocks per output, 1 means no delay
  SNat (m + 1) ->
  -- ^ Vector length
  Maybe (Unsigned (CLog 2 (n + 1)), Unsigned (CLog 2 (m + 1)), Vec (m + 1) a) ->
  Maybe (Vec (m + 1) a) ->
  (Maybe (Unsigned (CLog 2 (n + 1)), Unsigned (CLog 2 (m + 1)), Vec (m + 1) a), Maybe a)
delayBufferVecT _ _ Nothing Nothing =
  -- Nothing to do, waiting for another input
  (Nothing, Nothing)
delayBufferVecT _ _ Nothing (Just v) =
  -- output just finished, grab it
  (Just (0, 0, v), Nothing)
delayBufferVecT clocksPerOutput depth (Just (d, n, v)) _ =
  -- Need to send characters
  let next = d == snatToNum (subSNat clocksPerOutput (SNat :: SNat 1))
      done = n == snatToNum (subSNat depth (SNat :: SNat 1)) && next
   in ( if done
          then Nothing
          else
            if next
              then Just (0, n + 1, v)
              else Just (d + 1, n, v),
        if next then Just (v !! n) else Nothing
      )

data BcdOrControl
  = Bcd (Unsigned 4)
  | Return
  | Eot
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

blankLeadingZeros :: KnownNat n => Vec n (Unsigned 4) -> Vec n (Maybe BcdOrControl)
blankLeadingZeros v =
  case Vector.findIndex ((/=) 0) v of
    Nothing -> Vector.replace (0 :: Int) (Just (Bcd 0)) $ repeat Nothing
    Just i -> Vector.imap (\i' x -> if i' < i then Nothing else Just (Bcd x)) v

bcdOrControlToAscii :: BcdOrControl -> BitVector 8
bcdOrControlToAscii x =
  case x of
    Bcd bcd -> bcdToAscii $ pack bcd
    Return -> 10
    Eot -> 4

-- Let's us arbitrarily group the number of times the state machine executes in
-- combinational logic.  Possibly none!
adapt :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
adapt m b ta =
  let (bs, ta') = traverse (fmap (\(b, a) -> ([b], a)) $ m b) ta
   in (last (b : bs), ta')

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore maybeByteSig =
  let
    maybeToWriteOp = maybe RamNoOp (uncurry RamWrite)
    parsed = register Nothing $ fmap (fmap parse) maybeByteSig
    (mLineWrite, mCharWrite, lineWriteAddr) = unbundle $ mealy storeT (StoreTState (0 :: Index 256) 0 0) parsed
    lineWriteAddr' = register 0 $ register 0 $ register 0 lineWriteAddr
    (lineRead, charRead, bvs) = unbundle $ mealy retrieveT (RetrieveTState 0 0 0 True 0) $ bundle (lineRamOut, charRamOut, lineWriteAddr')
    (_, lineRamOut) = trueDualPortBlockRam (maybeToWriteOp <$> mLineWrite) (RamRead <$> lineRead)
    (_, charRamOut) = trueDualPortBlockRam (maybeToWriteOp <$> mCharWrite) (RamRead <$> charRead)
    priorities = mealy priorityT Nothing bvs
    out = join <$> mealy (adapt sumT) 0 priorities
  in
    out

runOutputU64 :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 80000) => Signal dom (Maybe (Unsigned 64)) -> Signal dom Bit
runOutputU64 =
  fst
    . uartTx (SNat :: SNat 781250)
    . register Nothing
    . fmap (fmap bcdOrControlToAscii)
    . register Nothing
    . fmap join
    -- TODO: uartTx signals when ready for the next
    . mealy (delayBufferVecT (SNat :: SNat 160) (SNat :: SNat 22)) Nothing
    . register Nothing
    . fmap (fmap (\x -> blankLeadingZeros x ++ (Just Return :> Just Eot :> Nil)))
    . register Nothing
    . mealy bcd64T Nothing

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 80000) => Signal dom Bit -> Signal dom Bit
run =
  runOutputU64
    . register Nothing
    . runCore
    . register Nothing
    . uartRx (SNat :: SNat 781250)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal Alchitry8 Bit -> Signal Alchitry8 Bit
topEntity clk rst _ input =
  let (clk', _, _) = pllPadPrim 0 0 7 "SIMPLE" 1 "GENCLK" "FIXED" "FIXED" 0 0 0 clk (pure 0) (pure 1) (pure 0)
   in exposeClockResetEnable run clk' (convertReset clk clk' rst) enableGen input

charToBit :: Char -> Int -> Bit
charToBit char lsbIndex =
  (fromIntegral (ord char) :: Unsigned 8) ! lsbIndex

charToUartRx :: Char -> Vec 160 Bit
charToUartRx char =
  replicate (SNat :: SNat 16) 0
    ++ replicate (SNat :: SNat 16) (charToBit char 0)
    ++ replicate (SNat :: SNat 16) (charToBit char 1)
    ++ replicate (SNat :: SNat 16) (charToBit char 2)
    ++ replicate (SNat :: SNat 16) (charToBit char 3)
    ++ replicate (SNat :: SNat 16) (charToBit char 4)
    ++ replicate (SNat :: SNat 16) (charToBit char 5)
    ++ replicate (SNat :: SNat 16) (charToBit char 6)
    ++ replicate (SNat :: SNat 16) (charToBit char 7)
    ++ replicate (SNat :: SNat 16) 1

testOutput1 :: Vec _ Char
testOutput1 = $(listToVecTH "157")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "abdefghiZjklmnopqrstuvwxABDEFGHIJKLMNOPQRSZTUVWX\naa")
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
      out = InOut <$> fmap (Maybe.fromMaybe 0) inputSignal <*> fmap (Maybe.fromMaybe 0) (exposeClockResetEnable runCore clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 200 "" out

testBench :: Signal Alchitry8 Bool
testBench =
  let en = enableGen
      clk = tbClockGen (not <$> done)
      rst = resetGen
      inputSignal = stimuliGenerator clk rst testInput
      expectOutput =
        outputVerifier'
          clk
          rst
          ( replicate (SNat :: SNat 24261) 1
              ++ replicate (SNat :: SNat 2720) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
