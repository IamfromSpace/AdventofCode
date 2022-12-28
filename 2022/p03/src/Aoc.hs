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
import Clash.WaveDrom (ToWave, ShowWave(ShowWave), WithBits (WithBits), render, wavedromWithClock)
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

bvHistT :: BitVector 8 -> BitVector 8 -> (BitVector 8, BitVector 8)
bvHistT bv char =
  let x = unpack char :: Unsigned 8
      azIndexed = truncateB (x - 97) :: Unsigned 3
      bv' = replaceBit azIndexed 1 bv
      bv'' = replaceBit azIndexed 1 bv'
  in
    (bv'', bv'')

runCore :: (HiddenClockResetEnable dom) => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (BitVector 8))
runCore =
    mealy (adapt bvHistT) 0

-- Let's us arbitrarily group the number of times the state machine executes in
-- combinational logic.  Possibly none!
adapt :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
adapt m b ta =
  let (bs, ta') = traverse (fmap (\(b, a) -> ([b], a)) $ m b) ta
   in (last (b : bs), ta')

run :: (HiddenClockResetEnable dom, DomainPeriod dom ~ 10000) => Signal dom Bit -> Signal dom Bit
run =
  fst
    . uartTx (SNat :: SNat 6250000)
    . register Nothing
    . runCore
    . register Nothing
    . uartRx (SNat :: SNat 6250000)

topEntity :: Clock Alchitry -> Reset Alchitry -> Enable Alchitry -> Signal Alchitry Bit -> Signal Alchitry Bit
topEntity =
    exposeClockResetEnable run

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

testOutput :: Vec _ Char
testOutput = $(listToVecTH " 0137")

testInput :: Vec _ Bit
testInput =
  let raw = $(listToVecTH "feabc")
   in ( (1 :> 1 :> 1 :> 1 :> Nil)
          ++ Vector.concatMap charToUartRx raw
          ++ (1 :> Nil)
      )

testInputCore :: Vec _ (Maybe (BitVector 8))
testInputCore =
  let raw = $(listToVecTH "feabc")
   in (Nothing :> Nil) ++ fmap (Just . fromIntegral . ord) raw

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
      out = InOut <$> fmap (ShowWave . chr . fromIntegral . Maybe.fromMaybe 0) inputSignal <*> fmap (ShowWave . chr . fromIntegral . Maybe.fromMaybe 0) (exposeClockResetEnable runCore clk rst en inputSignal)
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 20 "" out

testBench :: Signal Alchitry Bool
testBench =
  let en = enableGen
      clk = tbClockGen (not <$> done)
      rst = resetGen
      inputSignal = stimuliGenerator clk rst testInput
      expectOutput =
        outputVerifier'
          clk
          rst
          ( replicate (SNat :: SNat 166) 1
              ++ Vector.concatMap charToUartRx testOutput
              ++ (1 :> Nil)
          )
      done = expectOutput (exposeClockResetEnable run clk rst en inputSignal)
   in done
