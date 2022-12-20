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

run :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
run = pure (pure 0)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal Alchitry Bit -> Signal Alchitry Bit
topEntity clk _ _ input =
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
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 99 "" out

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
          ( replicate (SNat :: SNat 99) 1
              ++ replicate (SNat :: SNat 1200) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput1
              ++ charToUartRx '\n'
              ++ replicate (SNat :: SNat 1200) 1 -- blank leading zeros
              ++ Vector.concatMap charToUartRx testOutput2
              ++ charToUartRx '\n'
              ++ charToUartRx (chr 4)
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
