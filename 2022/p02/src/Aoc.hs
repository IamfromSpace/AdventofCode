{-# LANGUAGE FlexibleContexts #-}
module Aoc where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Class.BitPack (BitPack (pack, unpack), (!))
import Clash.Class.Resize (resize)
import Clash.Cores.UART (uartRx, uartTx)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen, tbSystemClockGen)
import Clash.Prelude (Bit, BitVector, CLog, Clock, DomainPeriod, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, addSNat, createDomain, enableGen, exposeClockResetEnable, knownVDomain, lengthS, mealy, register, repeat, replaceBit, replicate, resetGen, snatToNum, subSNat, vName, vPeriod, vResetPolarity)
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


data CodeL
  = A
  | B
  | C
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data CodeR
  = X
  | Y
  | Z
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data Inst = Inst CodeL CodeR
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

data Input
  = More Inst
  | Done
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

parseCodeL :: BitVector 8 -> CodeL
parseCodeL 65 = A
parseCodeL 66 = B
parseCodeL 67 = C
parseCodeL _ = error "No match for CodeL"

parseCodeR :: BitVector 8 -> CodeR
parseCodeR 88 = X
parseCodeR 89 = Y
parseCodeR 90 = Z
parseCodeR _ = error "No match for CodeR"

data ParseProgress
  = ZeroChars
  | OneChar CodeL
  | TwoChars CodeL
  | ThreeChars
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

inputT :: ParseProgress -> BitVector 8 -> (ParseProgress, Maybe Input)
inputT ZeroChars 4 = (ZeroChars, Just Done)
inputT ZeroChars x = (OneChar (parseCodeL x), Nothing)
inputT (OneChar cl) 32 = (TwoChars cl, Nothing)
inputT (TwoChars cl) x = (ThreeChars, Just (More (Inst cl (parseCodeR x))))
inputT ThreeChars _ = (ZeroChars, Nothing)
inputT _ _ = error "unexpected char"

scoreRound :: Inst -> Unsigned 64
scoreRound (Inst A X) = 3 + 1
scoreRound (Inst A Y) = 6 + 2
scoreRound (Inst A Z) = 0 + 3
scoreRound (Inst B X) = 0 + 1
scoreRound (Inst B Y) = 3 + 2
scoreRound (Inst B Z) = 6 + 3
scoreRound (Inst C X) = 6 + 1
scoreRound (Inst C Y) = 0 + 2
scoreRound (Inst C Z) = 3 + 3

scoreT :: Unsigned 64 -> Input -> (Unsigned 64, Maybe (Unsigned 64))
scoreT score Done = (0, Just score)
scoreT score (More inst) = (score + scoreRound inst, Nothing)

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
runCore =
  fmap join
    . mealy (adapt scoreT) 0
    . register Nothing
    . fmap join
    . mealy (adapt inputT) ZeroChars

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
topEntity clk _ _ input =
  let (clk', _, _) = pllPadPrim 0 0 2 "SIMPLE" 1 "GENCLK" "FIXED" "FIXED" 0 0 0 clk (pure 0) (pure 1) (pure 0)
   in exposeClockResetEnable run clk' resetGen enableGen input

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
testOutput1 = $(listToVecTH "15")

testOutput2 :: Vec _ Char
testOutput2 = $(listToVecTH "")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "A Y\nB X\nC Z\n")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "A Y\nB X\nC Z\n")
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
      out = InOut <$> fmap WithBits inputSignal <*> fmap WithBits (exposeClockResetEnable runCore clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 99 "" out

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
          ( replicate (SNat :: SNat 2395) 1
              ++ replicate (SNat :: SNat 2800) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
