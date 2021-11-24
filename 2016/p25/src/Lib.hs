module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow
  ( arr,
    returnA,
    (&&&),
    (***),
    (<+>),
    (<<<),
    (>>>),
    (>>^),
    (|||),
  )
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (toList)
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
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (init, lookup, map, (++))

data X
  = Reg Char
  | Val Int
  deriving (Show, Eq)

data Inst
  = Copy
      X
      X -- ONLY REG IS VALID
  | Inc Char
  | Dec Char
  | JumpNotZero
      X
      X
  | Toggle Char
  | Out Char
  deriving (Show, Eq)

pInt :: APC () Int
pInt =
  (AP.optional (AP.token '-') &&& AP.many1 AP.digit)
    >>^ (\(a, b) -> read (maybe [] pure a <> b))

pX :: APC () X
pX = (pInt >>^ Val) <+> (AP.anyToken >>^ Reg)

pCopy :: APC () Inst
pCopy = proc () -> do
  _ <- AP.string "cpy " -< ()
  a <- pX -< ()
  _ <- AP.token ' ' -< ()
  b <- pX -< ()
  returnA -< Copy a b

pDec :: APC () Inst
pDec = proc () -> do
  _ <- AP.string "dec " -< ()
  a <- AP.anyToken -< ()
  returnA -< Dec a

pInc :: APC () Inst
pInc = proc () -> do
  _ <- AP.string "inc " -< ()
  a <- AP.anyToken -< ()
  returnA -< Inc a

pJumpNotZero :: APC () Inst
pJumpNotZero = proc () -> do
  _ <- AP.string "jnz " -< ()
  a <- pX -< ()
  _ <- AP.token ' ' -< ()
  b <- pX -< ()
  returnA -< JumpNotZero a b

pOut :: APC () Inst
pOut = proc () -> do
  _ <- AP.string "out " -< ()
  a <- AP.anyToken -< ()
  returnA -< Out a

pInst :: APC () Inst
pInst = pCopy <+> pDec <+> pInc <+> pJumpNotZero <+> pOut

pProg :: APC () (Seq Inst)
pProg = AP.linesOf pInst >>^ Seq.fromList

parse1 :: String -> _
parse1 = either (error . show) id . AP.parse pProg


parse2 :: String -> _
parse2 = parse1

type State = (Int, Map Char Int, Seq Inst)

step :: Inst -> State -> (Maybe Int, State)
step i s@(ic, regs, insts) =
  case i of
    Copy (Reg from) to ->
      step (Copy (Val (Map.findWithDefault 0 from regs)) to) s
    Copy _ (Val _) -> (Nothing, (ic + 1, regs, insts)) -- ignored
    Copy (Val val) (Reg to) ->
      (Nothing, (ic + 1, Map.insert to val regs, insts))
    Inc r ->
      ( Nothing,
        ( ic + 1,
          Map.alter (pure . (+) 1 . Maybe.fromMaybe 0) r regs,
          insts
        )
      )
    Dec r ->
      ( Nothing,
        ( ic + 1,
          Map.alter (pure . (\x -> x - 1) . Maybe.fromMaybe 0) r regs,
          insts
        )
      )
    JumpNotZero (Reg r) offset ->
      step (JumpNotZero (Val (Map.findWithDefault 0 r regs)) offset) s
    JumpNotZero (Val 0) _ -> (Nothing, (ic + 1, regs, insts))
    JumpNotZero (Val _) (Val offset) ->
      (Nothing, (ic + offset, regs, insts))
    -- TODO: Some chance that this is just an invalid instruction...
    JumpNotZero x (Reg r) ->
      step (JumpNotZero x (Val (Map.findWithDefault 0 r regs))) s
    Toggle r ->
      let i = ic + Map.findWithDefault 0 r regs
       in ( Nothing,
            ( ic + 1,
              regs,
              case Seq.lookup i insts of
                Nothing -> insts
                Just (Copy a b) -> Seq.update i (JumpNotZero a b) insts
                Just (Inc a) -> Seq.update i (Dec a) insts
                Just (Dec a) -> Seq.update i (Inc a) insts
                Just (JumpNotZero a b) -> Seq.update i (Copy a b) insts
                Just (Toggle a) -> Seq.update i (Inc a) insts
                _ -> error "toggled something untogglable!"
            )
          )
    Out r ->
      ( maybe (error "baaaad") Just $ Map.lookup r regs,
        (ic + 1, regs, insts)
      )

run :: State -> [Int]
run s@(ic, _, insts) =
  case Seq.lookup ic insts of
    Nothing -> []
    Just i ->
      case step i s of
        (Just i, x) -> i : run x
        (Nothing, x) -> run x

complete :: _ -> Int -> Int
complete insts i =
  let [a, b] = take 2 $ drop 40 $ run (0, Map.singleton 'a' i, insts)
   in if a /= b
        then i
        else
          if i > 1554
            then error "dead"
            else complete insts (Util.labelTrace "i" (i + 1))

-- Solved by hand, 1:11:53 :/
answer1 :: _ -> _
answer1 insts --complete insts 0
  =
  take 40 $ run (0, Map.singleton 'a' 188, insts)

-- Freebie!
answer2 :: _ -> _
answer2 = id

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "pure components" $ do
    it "should parse" $ parse1 undefined `shouldBe` undefined
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 5" $ p2 "./ex2_5.txt" undefined
