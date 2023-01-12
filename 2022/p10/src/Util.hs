{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Util where

import Clash.Arithmetic.BCD (bcdToAscii, convertStep)
import Clash.Prelude (Bit, BitPack, BitSize, BitVector, CLog, Char, Eq, Functor, Generic, HiddenClockResetEnable, Int, KnownNat, Maybe (..), Monoid, Ord, Signal, SNat (..), Semigroup, Show, Traversable (..), Unsigned, Vec, bundle, fmap, fromIntegral, fst, maxBound, maybe, mealy, mempty, minBound, pack, repeat, replaceBit, replicate, snatToNum, split, subSNat, unbundle, undefined, (!), (!!), ($), (&&), (+), (++), (-), (/=), (<), (<$>), (<>), (<|>), (==))
import Clash.Prelude.BlockRam (ResetStrategy (ClearOnReset), blockRam1, readNew)
import qualified Clash.Sized.Vector as Vector
import Clash.XException (NFDataX, ShowX)
import Control.DeepSeq (NFData)
import Data.Char (ord)
import Data.Group (Group (..))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.TypeNats (type (+), type (^))

data MoreOrDone a
  = More a
  | Done
  deriving stock (Generic, Show, Eq, Ord, Functor)
  deriving anyclass (NFData, NFDataX, ShowX, BitPack)

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

pureSimWithRam :: Ord i => (s -> (a, r) -> (s, (b, i, Maybe (i, r)))) -> s -> [a] -> [(s, Map i r, b)]
pureSimWithRam f initial values =
  let f' (s, i, ram) a =
        let (s', (b, i', write)) = f s (a, fromMaybe undefined $ Map.lookup i ram)
            ram' = maybe ram (\(addr, v) -> Map.insert addr v ram) write
         in ((s', i', ram'), b)
   in (\((s, _, r), b) -> (s, r, b)) <$> pureSim f' (initial, undefined, mempty) values

pureSimWithRam1 :: Ord i => r -> (s -> (a, r) -> (s, (b, i, Maybe (i, r)))) -> s -> [a] -> [(s, Map i r, b)]
pureSimWithRam1 initialized f initial values =
  let f' (s, i, ram) a =
        let (s', (b, i', write)) = f s (a, fromMaybe initialized $ Map.lookup i ram)
            ram' = maybe ram (\(addr, v) -> Map.insert addr v ram) write
         in ((s', i', ram'), b)
   in (\((s, _, r), b) -> (s, r, b)) <$> pureSim f' (initial, undefined, mempty) values

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

newtype Vector a = Vector {getVector :: a}
  deriving stock (Generic, Show, Eq, Ord, Functor)
  deriving anyclass (NFData, NFDataX, BitPack)

instance Semigroup a => Semigroup (Vector (Vec n a)) where
  Vector a <> Vector b = Vector $ Vector.zipWith (<>) a b

instance (KnownNat n, Monoid a) => Monoid (Vector (Vec n a)) where
  mempty = Vector (Vector.repeat mempty)

instance (KnownNat n, Group a) => Group (Vector (Vec n a)) where
  invert (Vector v) = Vector $ fmap invert v

data DedupState n a
  = Idle
  -- ^ No previous input to dedup
  | Running a
  -- ^ Previous input is being checked
  | Flushing
  -- ^ The current input is Done, but we have a current value we're checking,
  -- so we need two clock cycles to output it and Done.
  -- Possibly this should just be a flag in Clearing to be more efficient.
  | Clearing (BitVector n)
  -- ^ Clearing RAM, current address being cleared
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData, NFDataX)

-- TODO: Ready signal while clearing
deduplicateT ::
  KnownNat width =>
  BitPack a =>
  BitSize a ~ (depth + width) =>
  SNat (2 ^ depth) ->
  a ->
  Mealy
    (DedupState depth a)
    (Maybe (MoreOrDone a), BitVector (2 ^ width))
    (Maybe (MoreOrDone a), BitVector depth, Maybe (BitVector depth, BitVector (2 ^ width)))
deduplicateT SNat defaultRead Flushing _ =
  (Clearing maxBound, (Just Done, fst $ split defaultRead, Nothing))
deduplicateT SNat defaultRead (Clearing n) _ =
  ( if n == minBound then Idle else Clearing (n - 1)
  , (Nothing, fst $ split defaultRead, Just (n, 0))
  )
deduplicateT SNat defaultRead Idle (Nothing, _) =
  (Idle, (Nothing, fst $ split defaultRead, Nothing))
deduplicateT SNat defaultRead Idle (Just Done, _) =
  (Clearing maxBound, (Just Done, fst $ split defaultRead, Nothing))
deduplicateT SNat _ Idle (Just (More next), _) =
  (Running next, (Nothing, fst $ split next, Nothing))
deduplicateT SNat defaultRead (Running prev) (mNext, prevWasPresentBv) =
  let
      readAddr =
        let v = case mNext of
              Just (More x) -> x
              _ -> defaultRead
        -- take the top bits to get the RAM address
        in fst $ split v
      next =
        case mNext of
          Nothing -> Idle
          Just (More x) -> Running x
          Just Done -> Flushing
      (ramAddr, index) = split prev
      output = if (prevWasPresentBv ! index) == 1 then
                    Nothing
                  else
                    Just (More prev)
      bv' = replaceBit index 1 prevWasPresentBv
      mWrite = Just (ramAddr, bv')
  in
  ( next,
    ( output,
      readAddr,
      mWrite
    )
  )

deduplicate :: HiddenClockResetEnable dom => KnownNat depth => BitPack a => BitSize a ~ (depth + width) => NFDataX a => SNat (2 ^ depth) -> a -> Signal dom (Maybe (MoreOrDone a)) -> Signal dom (Maybe (MoreOrDone a))
deduplicate depth defaultRead mA =
  let (out, readAddr, write) = unbundle $ mealy (deduplicateT depth defaultRead) Idle (bundle (mA, ramOut))
      ramOut = readNew (blockRam1 ClearOnReset depth 0) readAddr write
   in out
