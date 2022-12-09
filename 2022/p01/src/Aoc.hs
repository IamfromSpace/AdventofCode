module Aoc where

import Clash.Class.Resize (resize)
import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbSystemClockGen)
import Clash.Prelude (Bit, Clock, Enable, HiddenClockResetEnable, Reset, Signal, System, Unsigned, enableGen, exposeClockResetEnable, mealy, register, systemResetGen)
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
  { most1Carried :: Unsigned 32,
    most2Carried :: Unsigned 32,
    most3Carried :: Unsigned 32,
    caloriesAccumulated :: Unsigned 32,
    snackAccumulated :: Unsigned 32,
    notSent :: Bit
  }
  deriving stock (Generic)
  deriving anyclass (NFData, NFDataX, ToWave)

data Tx a = Tx
  { tx :: Bit,
    d :: a
  }
  deriving stock (Generic, Eq)
  deriving anyclass (NFData, NFDataX, ShowX, ToWave)

data Answer a = Answer
  { part :: Bit,
    char :: a
  }
  deriving stock (Generic, Eq)
  deriving anyclass (NFData, NFDataX, ShowX, ToWave)

type TxChar = Tx (Unsigned 8)

parseDigit :: Unsigned 8 -> Maybe (Unsigned 32)
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

digitToChar :: Unsigned 32 -> Unsigned 8
digitToChar = (+) 48 . (resize :: Unsigned 32 -> Unsigned 8)

-- Left 0 -> EndSnack
-- Left 1 -> EndElf
-- Right d -> Next Digit
type Feed = Either Bit (Unsigned 32)

-- State is the number of preceeding new lines
inputT :: Bit -> Tx (Unsigned 8) -> (Bit, Tx Feed)
inputT 0 (Tx 1 char) =
  case parseDigit char of
    Nothing ->
      (1, Tx 1 (Left 0))
    Just d ->
      (0, Tx 1 (Right d))
inputT 1 (Tx 1 char) =
  case parseDigit char of
    Nothing ->
      (0, Tx 1 (Left 1))
    Just d ->
      (0, Tx 1 (Right d))
inputT _ (Tx 0 _) =
  (0, Tx 0 (Left 0))

-- Must send two returns
-- Output is little endian
runT :: State -> Tx Feed -> (State, Maybe (Unsigned 32, Unsigned 32, Unsigned 32))
runT state (Tx 1 (Right d)) =
  let sa = snackAccumulated state
   in ( state {snackAccumulated = 10 * sa + d, notSent = 1},
        Nothing
      )
runT state (Tx 1 (Left 1)) =
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
runT state (Tx 1 (Left 0)) =
  let sa = snackAccumulated state
      ca = caloriesAccumulated state
   in ( state
          { caloriesAccumulated = ca + sa,
            snackAccumulated = 0
          },
        Nothing
      )
runT state (Tx 0 _) =
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

-- states are:
--   0: Wating to finish
--   1: Sending problem 1
--   2: Sending problem 2
--   3: Waiting for next send
outputT :: (Unsigned 2, Unsigned 32, Unsigned 32) -> Maybe (Unsigned 32, Unsigned 32, Unsigned 32) -> ((Unsigned 2, Unsigned 32, Unsigned 32), Tx (Answer (Unsigned 32)))
outputT (0, _, _) Nothing =
  -- Still waiting for input/calc to finish
  ((0, 0, 0), (Tx 0 (Answer 0 0)))
outputT (0, _, _) (Just (one, two, three)) =
  -- output just finish, grab it
  ((1, one, one + two + three), Tx 0 (Answer 0 0))
outputT (1, p1, p2) _ =
  -- Need to send answer 1
  let (rem, out) = p1 `divMod` 10
      done = rem == 0
   in ( (if done then 2 else 1, rem, p2),
        (Tx 1 (Answer 0 out))
      )
outputT (2, _, p2) _ =
  -- Need to send answer 2
  let (rem, out) = p2 `divMod` 10
      done = rem == 0
   in ( (if done then 0 else 2, 0, rem),
        (Tx 1 (Answer 1 out))
      )

run :: HiddenClockResetEnable dom => Signal dom TxChar -> Signal dom (Tx (Answer (Unsigned 8)))
run =
  fmap (\(Tx x (Answer a c)) -> Tx x (Answer a (digitToChar c)))
    . register (Tx 0 (Answer 0 0))
    . mealy outputT (0, 0, 0)
    . register Nothing
    . mealy runT (State 0 0 0 0 0 0)
    . register (Tx 0 (Right 0))
    . mealy inputT 0

topEntity :: Clock System -> Reset System -> Enable System -> Signal System TxChar -> Signal System (Tx (Answer (Unsigned 8)))
topEntity = exposeClockResetEnable run

testInput :: Vec 56 Char
testInput = $(listToVecTH "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n\n")

testOutput1 :: Vec 5 Char
testOutput1 = $(listToVecTH (reverse "24000"))

testOutput2 :: Vec 5 Char
testOutput2 = $(listToVecTH (reverse "45000"))

mkTestInput :: Clock System -> Reset System -> Signal System TxChar
mkTestInput clk rst =
  stimuliGenerator
    clk
    rst
    ( (Tx 0 0 :> Nil)
        ++ (fmap (Tx 1 . fromIntegral . ord) testInput)
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
   in setClipboard $ TL.unpack $ TLE.decodeUtf8 $ render $ wavedromWithClock 80 "" out

testBench :: Signal System Bool
testBench =
  let en = enableGen
      clk = tbSystemClockGen (not <$> done)
      rst = systemResetGen
      expectOutput =
        outputVerifier'
          clk
          rst
          ( (Tx 0 (Answer 0 0) :> Nil)
              ++ (fmap (const (Tx 0 (Answer 0 0))) testInput)
              ++ (fmap (Tx 1 . Answer 0 . fromIntegral . ord) testOutput1)
              ++ (fmap (Tx 2 . Answer 0 . fromIntegral . ord) testOutput2)
              ++ (Tx 0 (Answer 0 0) :> Nil)
          )
      done = expectOutput (topEntity clk rst en (mkTestInput clk rst))
   in done
