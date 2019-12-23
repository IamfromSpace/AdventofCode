module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import AdventOfCode.IntCode
       (Computer, Insts, WaitState(..), consume, initialize, parseInsts,
        step)
import AdventOfCode.Util (elmTrace)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid ()
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

parse1 :: String -> _
parse1 = parseInsts

parse2 :: String -> _
parse2 = parse1

runComputers :: Either Integer (Seq Integer) -> Seq Computer -> Integer
runComputers eMsgs cs =
    let (i, msg, eMsgs') =
            case eMsgs of
                Left i -> (i, pure (-1), Left (i + 1))
                Right (i :<| x :<| y :<| Empty) -> (i, pure x <> pure y, Left 0)
                Right (i :<| x :<| y :<| rem) ->
                    (i, pure x <> pure y, Right rem)
    in if i == 255
           then fromJust $ Seq.lookup 1 msg
           else case consume (fromJust $ Seq.lookup (fromIntegral i) cs, msg) of
                    (c', (_, new@(_ :<| _))) ->
                        runComputers
                            (case eMsgs' of
                                 Left _ -> Right new
                                 Right old -> Right (old <> new))
                            (Seq.update (fromIntegral i) c' cs)
                    (c', (_, Empty)) ->
                        runComputers eMsgs' (Seq.update (fromIntegral i) c' cs)
                    x -> error ("bad! " ++ show x)

answer1 :: _ -> _
answer1 insts =
    runComputers (Left 0) $
    Seq.mapWithIndex (\(fromIntegral -> i) c -> fst $ step (c, Just (pure i))) $
    Seq.fromList $ replicate 50 (initialize insts)

runComputers2 ::
       Set Integer
    -> Maybe (Integer, Integer)
    -> Either Integer (Seq Integer)
    -> Seq Computer
    -> Integer
runComputers2 seenYs natPacket eMsgs cs =
    let (i, msg, eMsgs') =
            case eMsgs of
                Left i -> (i, pure (-1), Left (i + 1))
                Right (i :<| x :<| y :<| Empty) -> (i, pure x <> pure y, Left 0)
                Right (i :<| x :<| y :<| rem) ->
                    (i, pure x <> pure y, Right rem)
    in if i == 50
           then let (x, y) = fromJust natPacket
                in if Set.member y seenYs
                       then y
                       else runComputers2
                                (Set.insert y seenYs)
                                Nothing
                                (Right (pure 0 <> pure x <> pure y))
                                cs
           else if i == 255
                    then runComputers2
                             seenYs
                             (Just
                                  ( fromJust $ Seq.lookup 0 msg
                                  , fromJust $ Seq.lookup 1 msg))
                             eMsgs'
                             cs
                    else case consume
                                  ( fromJust $ Seq.lookup (fromIntegral i) cs
                                  , msg) of
                             (c', (_, new@(_ :<| _))) ->
                                 runComputers2
                                     seenYs
                                     natPacket
                                     (case eMsgs' of
                                          Left _ -> Right new
                                          Right old -> Right (old <> new))
                                     (Seq.update (fromIntegral i) c' cs)
                             (c', (_, Empty)) ->
                                 runComputers2
                                     seenYs
                                     natPacket
                                     eMsgs'
                                     (Seq.update (fromIntegral i) c' cs)
                             x -> error ("bad! " ++ show x)

answer2 :: _ -> _
answer2 insts =
    runComputers2 mempty Nothing (Left 0) $
    Seq.mapWithIndex (\(fromIntegral -> i) c -> fst $ step (c, Just (pure i))) $
    Seq.fromList $ replicate 50 (initialize insts)
