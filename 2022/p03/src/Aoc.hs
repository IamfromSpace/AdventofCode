{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Class.BitPack (BitPack (pack, unpack), (!))
import Clash.Class.Resize (resize)
import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen, tbSystemClockGen)
import Clash.Prelude (Bit, BitVector, CLog, Clock, DomainPeriod, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, addSNat, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, lengthS, mealy, register, repeat, replaceBit, replicate, resetGen, snatToNum, subSNat, unbundle, vName, vPeriod, vResetPolarity)
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

createDomain (knownVDomain @System){vName="AlchitryThird", vResetPolarity=ActiveLow, vPeriod=30000}

data Feed
  = Done
  | NewLine
  | Char (Unsigned 6)

parse :: BitVector 8 -> Feed
parse 4 = Done
parse 10 = NewLine
parse x =
  Char (fromIntegral (if x >= 97 then x - 97 else x - 65 + 26))

-- Timing is a bit funny here...  We might get a full length one that we feed to the next step, but then immediately send a bunch of short ones before the next processing is done.
-- Do we _need_ to queue this?
-- We know that there are at least two elements and a new line (we know it
-- can't be empty, because there must a match), so we have to get everything
-- done in three clock cycles before the one behind could show up.
buildT ::
  (Unsigned 6, Vec 48 (Maybe (Unsigned 6))) ->
  Feed ->
  ((Unsigned 6, Vec 48 (Maybe (Unsigned 6))), Maybe (Bool, Unsigned 6, Vec 48 (Maybe (Unsigned 6))))
buildT (len, v) NewLine = ((0, repeat Nothing), Just (False, len `div` 2, v))
buildT (len, v) Done = ((0, repeat Nothing), Just (True, len `div` 2, v))
buildT (len, v) (Char x) = ((len + 1, fst $ Vector.shiftInAt0 v (pure x :> Nil)), Nothing)

toBitHistState :: (Bool, Unsigned 6, Vec 48 (Maybe (Unsigned 6))) -> BitHistState 24
toBitHistState (isDone, halfLen, v) =
  let left = Vector.imap (\i x -> if fromIntegral i >= halfLen then Nothing else x) (Vector.take (SNat :: SNat 24) v)
      right = Vector.take (SNat :: SNat 24) (Vector.rotateLeft v halfLen)
  in BitHistState isDone 0 0 left right

data BitHistState n = BitHistState
  { isDone :: Bool,
    leftHist :: BitVector 52,
    rightHist :: BitVector 52,
    left :: Vec n (Maybe (Unsigned 6)),
    right :: Vec n (Maybe (Unsigned 6))
  }
  deriving stock (Generic)
  deriving anyclass (NFData, NFDataX, ShowX)

stepHelper :: KnownNat n => Unsigned 8 -> BitHistState n -> BitHistState n
stepHelper i (BitHistState d lh rh l r) =
  BitHistState
     d
     (maybe lh (\x -> replaceBit x 1 lh) (l !! i))
     (maybe rh (\x -> replaceBit x 1 rh) (r !! i))
     l
     r

threeStepT :: KnownNat n => Maybe (Unsigned 1, BitHistState (n + 3)) -> Maybe (BitHistState (n + 3)) -> (Maybe (Unsigned 1, BitHistState (n + 3)), Maybe (BitHistState n))
threeStepT Nothing Nothing =
  (Nothing, Nothing)
threeStepT Nothing (Just bhs) =
  (Just (0, stepHelper 0 bhs), Nothing)
threeStepT (Just (0, bhs)) _ =
  (Just (1, stepHelper 1 bhs), Nothing)
threeStepT (Just (1, bhs)) _ =
  let (BitHistState d lh rh l r) = stepHelper 2 bhs
   in (Nothing, Just (BitHistState d lh rh (Vector.drop (SNat :: SNat 3) l) (Vector.drop (SNat :: SNat 3) r)))

twentyFourStep :: HiddenClockResetEnable dom => Signal dom (Maybe (BitHistState 24)) -> Signal dom (Maybe (BitHistState 0))
twentyFourStep =
  mealy threeStepT Nothing
    . mealy threeStepT Nothing
    . mealy threeStepT Nothing
    . mealy threeStepT Nothing
    . mealy threeStepT Nothing
    . mealy threeStepT Nothing
    . mealy threeStepT Nothing
    . mealy threeStepT Nothing

histCompare :: BitHistState 0 -> (Bool, BitVector 52)
histCompare (BitHistState d l r _ _ ) = (d, l .&. r)

getPriority :: BitVector 52 -> Unsigned 64
getPriority = (+) 1 . fromIntegral . Bits.countTrailingZeros

sumT :: Unsigned 64 -> (Bool, Unsigned 64) -> (Unsigned 64, Maybe (Unsigned 64))
sumT acc (True, new) = (0, Just (acc + new))
sumT acc (False, new) = (acc + new, Nothing)

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

parAsync :: (Signal dom a -> Signal dom b) -> (Signal dom c -> Signal dom d) -> Signal dom (a, c) -> Signal dom (b, d)
parAsync f g s =
  let (sa, sb) = unbundle s
      sa' = f sa
      sb' = g sb
   in bundle (sa', sb')

-- Let's us arbitrarily group the number of times the state machine executes in
-- combinational logic.  Possibly none!
adapt :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
adapt m b ta =
  let (bs, ta') = traverse (fmap (\(b, a) -> ([b], a)) $ m b) ta
   in (last (b : bs), ta')

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore =
  fmap join
    . mealy (adapt sumT) 0
    . fmap (fmap (fmap getPriority))
    . register Nothing
    . fmap (fmap histCompare)
    . register Nothing
    . twentyFourStep
    -- . fmap (pure Nothing)
    . register Nothing
    . fmap (fmap toBitHistState)
    . register Nothing
    . fmap join
    . mealy (adapt buildT) (0, repeat Nothing)
    . fmap (fmap parse)

runOutputU64 :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom (Maybe (Unsigned 64)) -> Signal dom Bit
runOutputU64 =
  fst
    . uartTx (SNat :: SNat 2083333)
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

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom Bit -> Signal dom Bit
run =
  runOutputU64
    . register Nothing
    . runCore
    . register Nothing
    . uartRx (SNat :: SNat 2083333)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal AlchitryThird Bit -> Signal AlchitryThird Bit
topEntity clk rst _ input =
  let (clk', _, _) = pllPadPrim 0 0 2 "SIMPLE" 1 "GENCLK" "FIXED" "FIXED" 0 0 0 clk (pure 0) (pure 1) (pure 0)
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

testBench :: Signal AlchitryThird Bool
testBench =
  let en = enableGen
      clk = tbClockGen (not <$> done)
      rst = resetGen
      inputSignal = stimuliGenerator clk rst testInput
      expectOutput =
        outputVerifier'
          clk
          rst
          ( replicate (SNat :: SNat 24254) 1
              ++ replicate (SNat :: SNat 2720) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
