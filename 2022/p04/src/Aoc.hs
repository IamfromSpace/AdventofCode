{-# LANGUAGE FlexibleContexts #-}

module Aoc where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Class.BitPack (BitPack (pack, unpack), (!))
import Clash.Class.Resize (resize)
import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Reset (convertReset)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen, tbSystemClockGen)
import Clash.Prelude (Bit, BitVector, CLog, Clock, DomainPeriod, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, addSNat, bundle, createDomain, enableGen, exposeClockResetEnable, knownVDomain, lengthS, mealy, register, repeat, replaceBit, replicate, resetGen, snatToNum, subSNat, unbundle, vName, vPeriod, vResetPolarity, Index, truncateB)
import Clash.Prelude.BlockRam (trueDualPortBlockRam, RamOp(..))
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (!!), (++))
import qualified Clash.Sized.Vector as Vector
import Clash.WaveDrom (ToWave, WithBits (WithBits), render, BitsWave(BitsWave), ShowWave(ShowWave), wavedromWithClock)
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

createDomain (knownVDomain @System){vName="Alchitry3", vResetPolarity=ActiveLow, vPeriod=30000}


data Parsed = Parsed
  { next :: Vec 2 (Unsigned 4),
    done :: Vec 6 (Unsigned 4)
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

data Range a = Range a a
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

data Pair a = Pair a a
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

parsedToRangePair :: Parsed -> Pair (Range (Vec 2 (Unsigned 4)))
parsedToRangePair (Parsed next done) =
  Pair
    (Range
      (Vector.select (SNat :: SNat 0) (SNat :: SNat 1) (SNat :: SNat 2) done)
      (Vector.select (SNat :: SNat 2) (SNat :: SNat 1) (SNat :: SNat 2) done)
    )
    (Range
      (Vector.select (SNat :: SNat 4) (SNat :: SNat 1) (SNat :: SNat 2) done)
      next
    )

parseT :: (Unsigned 2, Parsed) -> BitVector 8 -> ((Unsigned 2, Parsed), Maybe (Bool, Pair (Range (Vec 2 (Unsigned 4)))))
parseT (finishedNumCount, p@Parsed { next, done }) char =
  let
    isDone = char == 4
    isNumBreak = char == 10 || char == 45 || char == 44 || isDone
    bcd = truncateB (unpack (char - 48) :: Unsigned 8)
  in
    if isNumBreak then
      let parsed' = Parsed (repeat 0) (fst $ Vector.shiftInAtN done next)
      in if finishedNumCount == 3 then
            ((0, Parsed (repeat 0) (repeat 0)), Just (isDone, parsedToRangePair p))
         else
            ((finishedNumCount + 1, parsed'), Nothing)
    else
      ((finishedNumCount, Parsed (fst $ Vector.shiftInAtN next (bcd :> Nil)) done), Nothing)

oneContainsOther :: Pair (Range (Vec 2 (Unsigned 4))) -> Bool
oneContainsOther (Pair (Range a0 a1) (Range b0 b1)) =
  -- This one is interesting in that we can pipeline it in 2 or even three
  -- steps, however, because our final a && b || c && d fits perfectly on a
  -- single LUT4, the benefits of two steps is minimal, and three is likely
  -- harmful.  In the end, a two step pipeline exactly one LUT of propagation
  -- time and no routing.  This is almost certainly not the bottleneck!
  (a0 <= b0 && a1 >= b1) || (b0 <= a0 && b1 >= a1)

sumT :: Unsigned 64 -> (Bool, Unsigned 64) -> (Unsigned 64, Maybe (Unsigned 64))
sumT acc (True, new) = (0, Just (acc + new))
sumT acc (False, new) = (acc + new, Nothing)

bcd64T ::
  Maybe (Unsigned 64, Unsigned 64, Unsigned 6, Vec 20 (Unsigned 4), Vec 20 (Unsigned 4)) ->
  Maybe (Unsigned 64, Unsigned 64) ->
  ( Maybe (Unsigned 64, Unsigned 64, Unsigned 6, Vec 20 (Unsigned 4), Vec 20 (Unsigned 4)),
    Maybe (Vec 20 (Unsigned 4), Vec 20 (Unsigned 4))
  )
bcd64T Nothing Nothing =
  -- Nothing in progress, no new values to convert
  (Nothing, Nothing)
bcd64T Nothing (Just (a, b)) =
  -- New values to convert
  (Just (a, b, 63, repeat 0, repeat 0), Nothing)
bcd64T (Just (a, b, n, aBcd, bBcd)) _ =
  -- Convert values in progress
  let aBcd' = convertStep (a ! n) aBcd
      bBcd' = convertStep (b ! n) bBcd
      done = n == 0
   in if done
        then (Nothing, Just (aBcd', bBcd'))
        else (Just (a, b, (n - 1), aBcd', bBcd'), Nothing)

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
    Nothing -> Vector.replace (Vector.length v - 1) (Just (Bcd 0)) $ repeat Nothing
    Just i -> Vector.imap (\i' x -> if i' < i then Nothing else Just (Bcd x)) v

bcdOrControlToAscii :: BcdOrControl -> BitVector 8
bcdOrControlToAscii x =
  case x of
    Bcd bcd -> bcdToAscii $ pack bcd
    Return -> 10
    Eot -> 4

awaitBothT :: (Maybe a, Maybe b) -> (Maybe a, Maybe b) -> ((Maybe a, Maybe b), Maybe (a,b))
awaitBothT (mar, mbr) (mai, mbi) =
   case (mar <|> mai, mbr <|> mbi) of
     (Just a, Just b) -> ((Nothing, Nothing), Just (a, b))
     x -> (x, Nothing)

-- Let's us arbitrarily group the number of times the state machine executes in
-- combinational logic.  Possibly none!
adapt :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
adapt m b ta =
  let (bs, ta') = traverse (fmap (\(b, a) -> ([b], a)) $ m b) ta
   in (last (b : bs), ta')

runCore1 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore1 =
  fmap join
    . mealy (adapt sumT) 0
    . register Nothing
    . fmap (fmap (fmap (\x -> if x then 1 else 0)))
    . fmap (fmap (fmap oneContainsOther))
    . register Nothing
    . fmap join
    . mealy (adapt parseT) (0, Parsed (repeat 0) (repeat 0))

runCore2 :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64))
runCore2 =
  pure (pure (pure 0))

runCore :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (Unsigned 64, Unsigned 64))
runCore maybeByteSig =
  let
    part1 = runCore1 maybeByteSig
    part2 = runCore2 maybeByteSig
    out = mealy awaitBothT (Nothing, Nothing) (bundle (part1, part2))
  in
    out

runOutputU64 :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom (Maybe (Unsigned 64, Unsigned 64)) -> Signal dom Bit
runOutputU64 =
  fst
    . uartTx (SNat :: SNat 2083333)
    . register Nothing
    . fmap (fmap bcdOrControlToAscii)
    . register Nothing
    . fmap join
    -- TODO: uartTx signals when ready for the next
    . mealy (delayBufferVecT (SNat :: SNat 160) (SNat :: SNat 43)) Nothing
    . register Nothing
    . fmap (fmap (\(a, b) -> blankLeadingZeros a ++ (Just Return :> Nil) ++ blankLeadingZeros b ++ (Just Return :> Just Eot :> Nil)))
    . register Nothing
    . mealy bcd64T Nothing

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 30000) => Signal dom Bit -> Signal dom Bit
run =
  runOutputU64
    . register Nothing
    . runCore
    . register Nothing
    . uartRx (SNat :: SNat 2083333)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal Alchitry3 Bit -> Signal Alchitry3 Bit
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
testOutput1 = $(listToVecTH "2")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "0")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "5-15,12-13\n10-11,3-5\n1-5,1-5")
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
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 40 "" out

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
          ( replicate (SNat :: SNat 7916) 1
              ++ replicate (SNat :: SNat 3040) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ replicate (SNat :: SNat 3040) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput2
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
