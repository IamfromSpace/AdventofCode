module Aoc where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Class.BitPack (BitPack (pack, unpack), (!))
import Clash.Class.Resize (resize)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen, tbSystemClockGen)
import Clash.Prelude (Bit, BitVector, Clock, Enable, HiddenClockResetEnable, KnownNat, Reset, ResetPolarity (ActiveLow), SNat (SNat), Signal, System, Unsigned, addSNat, createDomain, enableGen, exposeClockResetEnable, knownVDomain, lengthS, mealy, register, repeat, replaceBit, replicate, resetGen, vName, vPeriod, vResetPolarity)
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
import Ice40.Pll.Pad (pllPadPrim)
import System.Hclip (setClipboard)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

createDomain (knownVDomain @System){vName="Alchitry", vResetPolarity=ActiveLow, vPeriod=10000}

createDomain (knownVDomain @System){vName="AlchitryHalf", vResetPolarity=ActiveLow, vPeriod=20000}

data UartRx8N1State
  = Idle
  | Offsetting (Unsigned 4)
  | Receiving (Unsigned 4) (Unsigned 4) -- Delay / Bits RX'd
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

-- Our cases generally read bottom to top here
uartRx8N1T :: (UartRx8N1State, BitVector 8) -> Bit -> ((UartRx8N1State, BitVector 8), Maybe (BitVector 8))
uartRx8N1T ((Receiving 7 8), _) 0 =
  -- impossible case, where the stop bit isn't high
  undefined
uartRx8N1T ((Receiving 7 8), v) 1 =
  -- The 9th bit is the stop bit, which must be 1, we immediately go idle and
  -- send the result, just in case there's slack in the system and the stop bit
  -- must run short.
  ((Idle, v), Just v)
uartRx8N1T ((Receiving 7 n), v) b =
  -- This is our safest clock cycle to read the bit, so we do
  ((Receiving 0 (n + 1), replaceBit n b v), Nothing)
uartRx8N1T ((Receiving d n), v) _ =
  -- This clock cycle might be mid-edge change, so we ignore it
  (((Receiving (d + 1) n), v), Nothing)
uartRx8N1T (Offsetting 3, v) _ =
  -- We use three clock cycles per bit, and we always offset 1 from the
  -- detected falling edge between stop and start bits.  This ensure's were
  -- in the middle of the baud cycle +/- 1/6.
  ((Receiving 0 0, v), Nothing)
uartRx8N1T (Offsetting n, v) _ =
  -- We use three clock cycles per bit, and we always offset 1 from the
  -- detected falling edge between stop and start bits.  This ensure's were
  -- in the middle of the baud cycle +/- 1/6.
  ((Offsetting (n + 1), v), Nothing)
uartRx8N1T (Idle, v) 0 =
  -- Start bit recieved, meaning we're no longer idle.  We also now know
  -- (within 1/3rd of the baud rate) where the falling edge lies.  We'll
  -- offset from it, to ensure we're in a stable zone.
  ((Offsetting 0, v), Nothing)
uartRx8N1T (Idle, v) 1 =
  -- No data, still waiting for the falling edge.
  ((Idle, v), Nothing)

data UartTx8N1State
  = Start
  | Sending (Unsigned 3)
  | Stop (Maybe (BitVector 8))
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

uartTx8N1T :: Maybe (UartTx8N1State, Unsigned 4, BitVector 8) -> Maybe (BitVector 8) -> (Maybe (UartTx8N1State, Unsigned 4, BitVector 8), Bit)
uartTx8N1T (Just (Stop mv', n, v)) mv'' =
  let mv = mv' <|> mv''
   in if n == 7
        then case mv of
          Nothing -> (Nothing, 1)
          Just v -> (Just (Start, 0, v), 1)
        else (Just (Stop mv, n + 1, v), 1)
uartTx8N1T (Just (Sending 7, 7, v)) mv = (Just (Stop mv, 0, v), v ! 7)
uartTx8N1T (Just (Sending n, 7, v)) _ = (Just (Sending (n + 1), 0, v), v ! n)
uartTx8N1T (Just (Sending n, m, v)) _ = (Just (Sending n, m + 1, v), v ! n)
uartTx8N1T (Just (Start, 7, v)) _ = (Just (Sending 0, 0, v), 0)
uartTx8N1T (Just (Start, n, v)) _ = (Just (Start, n + 1, v), 0)
uartTx8N1T Nothing (Just v) = (Just (Start, 0, v), 1)
uartTx8N1T Nothing Nothing = (Nothing, 1)

data State = State
  { most1Carried :: Unsigned 64,
    most2Carried :: Unsigned 64,
    most3Carried :: Unsigned 64,
    caloriesAccumulated :: Unsigned 64,
    snackAccumulated :: Unsigned 64,
    notSent :: Bit
  }
  deriving stock (Generic)
  deriving anyclass (NFData, NFDataX, ToWave)

type TxChar = Maybe (Unsigned 8)

parseDigit :: Unsigned 8 -> Maybe (Unsigned 64)
parseDigit 10 = Nothing
parseDigit 48 = Just 0
parseDigit 49 = Just 1
parseDigit 50 = Just 2
parseDigit 51 = Just 3
parseDigit 52 = Just 4
parseDigit 53 = Just 5
parseDigit 54 = Just 6
parseDigit 55 = Just 7
parseDigit 56 = Just 8
parseDigit 57 = Just 9
parseDigit _ = Nothing

-- Left 0 -> EndSnack
-- Left 1 -> EndElf
-- Right d -> Next Digit
type Feed = Either Bit (Unsigned 64)

-- State is the number of preceeding new lines
inputT :: Bit -> Unsigned 8 -> (Bit, Maybe Feed)
inputT _ 4 =
  (0, Nothing)
inputT 0 char =
  case parseDigit char of
    Nothing ->
      (1, Just (Left 0))
    Just d ->
      (0, Just (Right d))
inputT 1 char =
  case parseDigit char of
    Nothing ->
      (0, Just (Left 1))
    Just d ->
      (0, Just (Right d))

-- Let's us arbitrarily group the number of times the state machine executes in
-- combinational logic.  Possibly none!
adapt :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
adapt m b ta =
  let (bs, ta') = traverse (fmap (\(b, a) -> ([b], a)) $ m b) ta
   in (last (b : bs), ta')

-- Must send two returns
runT :: State -> Maybe Feed -> (State, Maybe (Unsigned 64, Unsigned 64, Unsigned 64))
runT state (Just (Right d)) =
  let sa = snackAccumulated state
   in ( state {snackAccumulated = 10 * sa + d, notSent = 1},
        Nothing
      )
runT state (Just (Left 1)) =
  let ca = caloriesAccumulated state
      m1c = most1Carried state
      m2c = most2Carried state
      m3c = most3Carried state
   in ( state
          { most1Carried = max ca m1c,
            most2Carried = max (min ca m1c) m2c,
            most3Carried = max (min ca m2c) m3c,
            caloriesAccumulated = 0
          },
        Nothing
      )
runT state (Just (Left 0)) =
  let sa = snackAccumulated state
      ca = caloriesAccumulated state
   in ( state
          { caloriesAccumulated = ca + sa,
            snackAccumulated = 0
          },
        Nothing
      )
runT state Nothing =
  if notSent state == 1
    then
      ( State 0 0 0 0 0 0,
        Just
          ( most1Carried state,
            most2Carried state,
            most3Carried state
          )
      )
    else (state, Nothing)

-- NOTE: no backpressure!  If this isn't done and new data is received, it's
-- just dropped!
bcd32T ::
  Maybe (Unsigned 32, Unsigned 32, Unsigned 5, Vec 10 (Unsigned 4), Vec 10 (Unsigned 4)) ->
  Maybe (Unsigned 32, Unsigned 32) ->
  ( Maybe (Unsigned 32, Unsigned 32, Unsigned 5, Vec 10 (Unsigned 4), Vec 10 (Unsigned 4)),
    Maybe (Vec 10 (Unsigned 4), Vec 10 (Unsigned 4))
  )
bcd32T Nothing Nothing =
  -- Nothing in progress, no new values to convert
  (Nothing, Nothing)
bcd32T Nothing (Just (a, b)) =
  -- New values to convert
  (Just (a, b, 31, repeat 0, repeat 0), Nothing)
bcd32T (Just (a, b, n, aBcd, bBcd)) _ =
  -- Convert values in progress
  let aBcd' = convertStep (a ! n) aBcd
      bBcd' = convertStep (b ! n) bBcd
      done = n == 0
   in if done
        then (Nothing, Just (aBcd', bBcd'))
        else (Just (a, b, (n - 1), aBcd', bBcd'), Nothing)

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

-- TODO: Not sure how to make vector length abstract
delayBufferVec43T ::
  Maybe (Unsigned 7, Unsigned 6, Vec 43 a) ->
  Maybe (Vec 43 a) ->
  (Maybe (Unsigned 7, Unsigned 6, Vec 43 a), Maybe a)
delayBufferVec43T Nothing Nothing =
  -- Nothing to do, waiting for another input
  (Nothing, Nothing)
delayBufferVec43T Nothing (Just v) =
  -- output just finished, grab it
  (Just (0, 0, v), Nothing)
delayBufferVec43T (Just (d, n, v)) _ =
  -- Need to send characters
  let next = d == 79
      done = n == 41 && next
   in ( if done
          then Nothing
          else
            if next
              then Just (0, (n + 1), v)
              else Just (d + 1, n, v),
        (if next then Just (v !! n) else Nothing)
      )

data BcdOrControl
  = Bcd (Unsigned 4)
  | Return
  | Eot
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX, ShowX)

blankLeadingZeros :: KnownNat n => Vec n (Unsigned 4) -> Vec n (Maybe BcdOrControl)
-- blankLeadingZeros = fmap (Just . Bcd)
blankLeadingZeros v =
  case Vector.findIndex ((/=) 0) v of
    Nothing -> Vector.replace 0 (Just (Bcd 0)) $ repeat Nothing
    Just i -> Vector.imap (\i' x -> if i' < i then Nothing else Just (Bcd x)) v

bcdOrControlToAscii :: BcdOrControl -> BitVector 8
bcdOrControlToAscii x =
  case x of
    Bcd bcd -> bcdToAscii $ pack bcd
    Return -> 10
    Eot -> 4

run :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
run =
  -- TODO: Realistically need to support something like ZMODEM if we want to be
  -- broadly compatible.
  mealy uartTx8N1T Nothing
    . register Nothing
    . fmap (fmap bcdOrControlToAscii)
    . register Nothing
    . fmap join
    . mealy delayBufferVec43T Nothing
    . register Nothing
    . fmap (fmap (\(a, b) -> blankLeadingZeros a ++ (Just Return :> Nil) ++ blankLeadingZeros b ++ (Just Return :> Just Eot :> Nil)))
    . register Nothing
    . mealy bcd64T Nothing
    . register Nothing
    . fmap (fmap (\(one, twoAndThree) -> (one, one + twoAndThree)))
    . register Nothing
    . fmap (fmap (\(one, two, three) -> (one, two + three)))
    . fmap join
    . register Nothing
    . mealy (adapt runT) (State 0 0 0 0 0 0)
    . register Nothing
    . mealy (adapt inputT) 0
    . register Nothing
    . fmap (fmap unpack)
    . mealy uartRx8N1T (Idle, 0)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal Alchitry Bit -> Signal Alchitry Bit
topEntity clk rst _ input =
  let (clk', _, _) = pllPadPrim 0 0 2 "SIMPLE" 1 "GENCLK" "FIXED" "FIXED" 0 0 0 clk (pure 0) (pure 1) (pure 0)
   in exposeClockResetEnable run clk' resetGen enableGen input

charToBit :: Char -> Int -> Bit
charToBit char lsbIndex =
  (fromIntegral (ord char) :: Unsigned 8) ! lsbIndex

charToUartRx :: Char -> Vec 80 Bit
charToUartRx char =
  replicate (SNat :: SNat 8) 0
    ++ replicate (SNat :: SNat 8) (charToBit char 0)
    ++ replicate (SNat :: SNat 8) (charToBit char 1)
    ++ replicate (SNat :: SNat 8) (charToBit char 2)
    ++ replicate (SNat :: SNat 8) (charToBit char 3)
    ++ replicate (SNat :: SNat 8) (charToBit char 4)
    ++ replicate (SNat :: SNat 8) (charToBit char 5)
    ++ replicate (SNat :: SNat 8) (charToBit char 6)
    ++ replicate (SNat :: SNat 8) (charToBit char 7)
    ++ replicate (SNat :: SNat 8) 1

testOutput1 :: Vec 5 Char
testOutput1 = $(listToVecTH "24000")

testOutput2 :: Vec 5 Char
testOutput2 = $(listToVecTH "45000")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n\n")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ charToUartRx (chr 4)
          ++ (1 :> Nil)
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
      rst = resetGen
      inputSignal = stimuliGenerator clk rst testInput
      out = InOut <$> inputSignal <*> topEntity clk rst en inputSignal
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 700 "" out

testBench :: Signal System Bool
testBench =
  let en = enableGen
      clk = tbSystemClockGen (not <$> done)
      rst = resetGen
      inputSignal = stimuliGenerator clk rst testInput
      expectOutput =
        outputVerifier'
          clk
          rst
          ( replicate (SNat :: SNat 4714) 1
              ++ replicate (SNat :: SNat 1200) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput1
              ++ (charToUartRx '\n')
              ++ replicate (SNat :: SNat 1200) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput2
              ++ (charToUartRx '\n')
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
