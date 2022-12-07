module Aoc where

import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbSystemClockGen)
import Clash.Prelude (Bit, Clock, Enable, HiddenClockResetEnable, Reset, Signal, System, Unsigned, enableGen, exposeClockResetEnable, mealy, systemResetGen)
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, (++))
import Clash.WaveDrom (ToWave, render, wavedromWithClock)
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
import Prelude hiding (foldr, init, lookup, map, (++))

data State = State
  { mostCarried :: Unsigned 16,
    caloriesAccumulated :: Unsigned 16,
    snackAccumulated :: Unsigned 16,
    snackDone :: Bit,
    outputSent :: Bit
  }
  deriving stock (Generic)
  deriving anyclass (NFData, NFDataX, ToWave)

data Tx a = Tx
  { tx :: Bit,
    d :: a
  }
  deriving stock (Generic, Eq)
  deriving anyclass (NFData, NFDataX, ShowX, ToWave)

data Answer = Answer
  { part :: Bit,
    char :: Unsigned 8
  }
  deriving stock (Generic, Eq)
  deriving anyclass (NFData, NFDataX, ShowX, ToWave)

type TxChar = Tx (Unsigned 8)

parseDigit :: Unsigned 8 -> Maybe (Unsigned 16)
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
parseDigit _ = undefined

digitToChar :: Unsigned 16 -> Unsigned 8
digitToChar 0 = 48
digitToChar 1 = 49
digitToChar 2 = 50
digitToChar 3 = 51
digitToChar 4 = 52
digitToChar 5 = 53
digitToChar 6 = 54
digitToChar 7 = 55
digitToChar 8 = 56
digitToChar 9 = 57
digitToChar _ = undefined

-- Must send two returns
-- Output is little endian
runT :: State -> TxChar -> (State, Tx Answer)
runT state (Tx 1 char) =
  case parseDigit char of
    Just d ->
      let sa = snackAccumulated state
       in ( state
              { snackAccumulated = 10 * sa + d,
                snackDone = 0
              },
            (Tx 0 (Answer 0 0))
          )
    Nothing ->
      if snackDone state == 1
        then
          let ca = caloriesAccumulated state
              mc = mostCarried state
           in ( state
                  { mostCarried = max ca mc,
                    caloriesAccumulated = 0,
                    snackDone = 0
                  },
                (Tx 0 (Answer 0 0))
              )
        else
          let sa = snackAccumulated state
              ca = caloriesAccumulated state
           in ( state
                  { caloriesAccumulated = ca + sa,
                    snackAccumulated = 0,
                    snackDone = 1
                  },
                (Tx 0 (Answer 0 0))
              )
runT state (Tx 0 _) =
  if outputSent state == 1
    then (state, (Tx 0 (Answer 0 0)))
    else
      let (rem, out) = mostCarried state `divMod` 10
          done = rem == 0
       in ( state
              { mostCarried = rem,
                outputSent = if done then 1 else 0
              },
            (Tx 1 (Answer 0 (digitToChar out)))
          )

run :: HiddenClockResetEnable dom => Signal dom TxChar -> Signal dom (Tx Answer)
run = mealy runT (State 0 0 0 0 0)

topEntity :: Clock System -> Reset System -> Enable System -> Signal System TxChar -> Signal System (Tx Answer)
topEntity = exposeClockResetEnable run

testInput :: Vec 56 Char
testInput = $(listToVecTH "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n\n")

testOutput :: Vec 5 Char
testOutput = $(listToVecTH (reverse "24000"))

mkTestInput :: Clock System -> Reset System -> Signal System TxChar
mkTestInput clk rst =
  stimuliGenerator
    clk
    rst
    ( (fmap (Tx 1 . fromIntegral . ord) testInput)
        ++ (Tx 0 0 :> Nil)
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
      out = InOut <$> testInput <*> topEntity clk rst en testInput
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 16 "" out

testBench :: Signal System Bool
testBench =
  let en = enableGen
      clk = tbSystemClockGen (not <$> done)
      rst = systemResetGen
      expectOutput =
        outputVerifier'
          clk
          rst
          ( (fmap (const (Tx 0 (Answer 0 0))) testInput)
              ++ (fmap (Tx 1 . Answer 0 . fromIntegral . ord) testOutput)
              ++ (Tx 0 (Answer 0 0) :> Nil)
          )
      done = expectOutput (topEntity clk rst en (mkTestInput clk rst))
   in done
