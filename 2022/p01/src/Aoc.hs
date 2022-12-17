module Aoc where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Class.BitPack (BitPack (pack, unpack), (!))
import Clash.Class.Resize (resize)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbSystemClockGen)
import Clash.Prelude (Bit, BitVector, Clock, Enable, HiddenClockResetEnable, Reset, SNat (SNat), Signal, System, Unsigned, addSNat, enableGen, exposeClockResetEnable, lengthS, mealy, register, repeat, replaceBit, replicate, systemResetGen)
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (!!), (++))
import Clash.WaveDrom (ToWave, WithBits (WithBits), render, wavedromWithClock)
import Clash.XException (NFDataX, ShowX)
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow (arr, returnA, (&&&), (***), (<+>), (<<<), (>>>), (>>^), (|||))
import Control.DeepSeq (NFData)
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Char (ord)
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
import System.Hclip (setClipboard)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, repeat, replicate, (!!), (++))

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
inputT :: Bit -> Maybe (Unsigned 8) -> (Bit, Maybe Feed)
inputT 0 (Just char) =
  case parseDigit char of
    Nothing ->
      (1, Just (Left 0))
    Just d ->
      (0, Just (Right d))
inputT 1 (Just char) =
  case parseDigit char of
    Nothing ->
      (0, Just (Left 1))
    Just d ->
      (0, Just (Right d))
inputT _ Nothing =
  (0, Nothing)

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
bufferVec42T ::
  Maybe (Unsigned 6, Vec 42 a) ->
  Maybe (Vec 42 a) ->
  (Maybe (Unsigned 6, Vec 42 a), Maybe a)
bufferVec42T Nothing Nothing =
  -- Nothing to do, waiting for another input
  (Nothing, Nothing)
bufferVec42T Nothing (Just v) =
  -- output just finished, grab it
  (Just (0, v), Nothing)
bufferVec42T (Just (n, v)) _ =
  -- Need to send characters
  let done = n == 41
   in ( if done then Nothing else Just ((n + 1), v),
        (Just (v !! n))
      )

-- outputs leading zeros
run :: HiddenClockResetEnable dom => Signal dom TxChar -> Signal dom (Maybe (Unsigned 8))
run =
  fmap (fmap (maybe 10 (unpack . bcdToAscii . pack)))
    . register Nothing
    . mealy bufferVec42T Nothing
    . register Nothing
    -- Nothing means '\n', so that we're wire efficient.  Probably need to do
    -- something else though.
    . fmap (fmap (\(a, b) -> fmap Just a ++ (Nothing :> Nil) ++ fmap Just b ++ (Nothing :> Nil)))
    . register Nothing
    . mealy bcd64T Nothing
    . register Nothing
    . fmap (fmap (\(one, twoAndThree) -> (one, one + twoAndThree)))
    . register Nothing
    . fmap (fmap (\(one, two, three) -> (one, two + three)))
    . register Nothing
    . mealy runT (State 0 0 0 0 0 0)
    . register Nothing
    . mealy inputT 0

topEntity :: Clock System -> Reset System -> Enable System -> Signal System TxChar -> Signal System (Maybe (Unsigned 8))
topEntity = exposeClockResetEnable run

testInput :: Vec 56 Char
testInput = $(listToVecTH "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n\n")

testOutput1 :: Vec 20 Char
testOutput1 = $(listToVecTH "00000000000000024000")

testOutput2 :: Vec 20 Char
testOutput2 = $(listToVecTH "00000000000000045000")

mkTestInput :: Clock System -> Reset System -> Signal System TxChar
mkTestInput clk rst =
  stimuliGenerator
    clk
    rst
    ( (Nothing :> Nil)
        ++ (fmap (Just . fromIntegral . ord) testInput)
        ++ (Nothing :> Nil)
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
      clk = tbSystemClockGen (False <$ out)
      rst = systemResetGen
      testInput = mkTestInput clk rst
      out = (\(InOut a b) -> InOut (WithBits a) (WithBits b)) <$> (InOut <$> testInput <*> topEntity clk rst en testInput)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 180 "" out

testBench :: Signal System Bool
testBench =
  let en = enableGen
      clk = tbSystemClockGen (not <$> done)
      rst = systemResetGen
      expectOutput =
        outputVerifier'
          clk
          rst
          ( replicate (addSNat (SNat :: SNat 73) (lengthS testInput)) Nothing
              ++ (fmap (Just . fromIntegral . ord) testOutput1)
              ++ (Just 10 :> Nil)
              ++ (fmap (Just . fromIntegral . ord) testOutput2)
              ++ (Just 10 :> Nil)
              ++ (Nothing :> Nil)
          )
      done = expectOutput (topEntity clk rst en (mkTestInput clk rst))
   in done
