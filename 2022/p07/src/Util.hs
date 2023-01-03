{-# LANGUAGE FlexibleContexts #-}

module Util where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Prelude (Bit, BitVector, CLog, Char, Eq, Generic, Int, KnownNat, Maybe (..), Ord, SNat (..), Show, Traversable (..), Unsigned, Vec, fmap, fromIntegral, pack, repeat, replicate, snatToNum, subSNat, (!), (!!), ($), (&&), (+), (++), (-), (/=), (<), (<|>), (==))
import qualified Clash.Sized.Vector as Vector
import Clash.XException (NFDataX, ShowX)
import Control.DeepSeq (NFData)
import Data.Char (ord)
import qualified Data.List as List
import GHC.TypeNats (type (+))

-- Let's us arbitrarily group the number of times the state machine executes in
-- combinational logic.  Possibly none!
adapt :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
adapt m b ta =
  let (bs, ta') = traverse (fmap (\(b, a) -> ([b], a)) $ m b) ta
   in (List.last (b : bs), ta')

pureSim :: (s -> a -> (s, b)) -> s -> [a] -> [(s, b)]
pureSim _ _ [] = []
pureSim f s (h : t) =
  let (s', b) = f s h
   in (s', b) : pureSim f s' t

awaitBothT :: (Maybe a, Maybe b) -> (Maybe a, Maybe b) -> ((Maybe a, Maybe b), Maybe (a, b))
awaitBothT (mar, mbr) (mai, mbi) =
  case (mar <|> mai, mbr <|> mbi) of
    (Just a, Just b) -> ((Nothing, Nothing), Just (a, b))
    x -> (x, Nothing)

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
        else (Just (a, b, n - 1, aBcd', bBcd'), Nothing)

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

type Mealy s a b = s -> a -> (s, b)
