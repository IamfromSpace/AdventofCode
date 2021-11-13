module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
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
import Data.Monoid (Sum(..))
import Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Prelude hiding ((++), init, lookup, map)
import Test.Hspec (describe, it, shouldBe)
import Text.ParserCombinators.PArrow (MD, (>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.Read (readMaybe)

data Inst
    = SwapIndex Int
                Int
    | SwapLetter Char
                 Char
    | RotLeft Int
    | RotRight Int
    | RotBased Char
    | Reverse Int
              Int
    | Move Int
           Int
    deriving (Show, Eq, Ord)

singleDigit :: MD String Int
singleDigit = PA.digit >>> arr (read . pure)

stepOrSteps :: MD String String
stepOrSteps = PA.string " steps" <+> PA.string " step"

parseSwapIndex :: MD String Inst
parseSwapIndex =
    (((PA.string "swap position " &&& singleDigit) >>> arr snd) &&&
     ((PA.string " with position " &&& singleDigit) >>> arr snd)) >>>
    arr (uncurry SwapIndex)

parseSwapLetter :: MD String Inst
parseSwapLetter =
    (((PA.string "swap letter " &&& PA.anyChar) >>> arr snd) &&&
     ((PA.string " with letter " &&& PA.anyChar) >>> arr snd)) >>>
    arr (uncurry SwapLetter)

parseRotLeft :: MD String Inst
parseRotLeft =
    (((PA.string "rotate left " &&& singleDigit) >>> arr snd) >>! stepOrSteps) >>>
    arr RotLeft

parseRotRight :: MD String Inst
parseRotRight =
    (((PA.string "rotate right " &&& singleDigit) >>> arr snd) >>! stepOrSteps) >>>
    arr RotRight

parseRotBased :: MD String Inst
parseRotBased =
    (PA.string "rotate based on position of letter " &&& PA.anyChar) >>>
    arr (RotBased . snd)

parseReverse :: MD String Inst
parseReverse =
    ((((PA.string "reverse positions " <+> PA.string "reverse position ") &&&
       singleDigit) >>>
      arr snd) &&&
     ((PA.string " through " &&& singleDigit) >>> arr snd)) >>>
    arr (uncurry Reverse)

parseMove :: MD String Inst
parseMove =
    (((PA.string "move position " &&& singleDigit) >>> arr snd) &&&
     ((PA.string " to position " &&& singleDigit) >>> arr snd)) >>>
    arr (uncurry Move)

parseInst :: MD String Inst
parseInst =
    parseSwapIndex <+>
    parseSwapLetter <+>
    parseRotLeft <+>
    parseRotRight <+> parseRotBased <+> parseReverse <+> parseMove

parse1 :: String -> _
parse1 s =
    Util.runPA (PA.sepBy1 parseInst (PA.char '\n')) $ take (length s - 1) s

parse2 :: String -> _
parse2 = parse1

runInst :: Inst -> Seq Char -> Seq Char
runInst (SwapIndex i j) s =
    let t = Maybe.fromJust $ Seq.lookup i s
    in Seq.update j t $ Seq.update i (Maybe.fromJust $ Seq.lookup j s) s
runInst (SwapLetter a b) s =
    runInst
        (SwapIndex
             (Maybe.fromJust $ Seq.elemIndexL a s)
             (Maybe.fromJust $ Seq.elemIndexL b s))
        s
runInst (RotRight i) s =
    let (a, b) = Seq.splitAt (Seq.length s - i) s
    in b <> a
runInst (RotLeft i) s =
    let (a, b) = Seq.splitAt i s
    in b <> a
runInst (RotBased x) s =
    let i = Maybe.fromJust $ Seq.elemIndexL x s
        i' =
            (1 + i +
             if i >= 4
                 then 1
                 else 0) `mod`
            length s
    in runInst (RotRight i') s
runInst (Reverse i j) s =
    let (a, b') = Seq.splitAt i s
        (b, c) = Seq.splitAt (j - i + 1) b'
    in a <> Seq.reverse b <> c
runInst (Move i j) s =
    let x = Maybe.fromJust $ Seq.lookup i s
    in Seq.insertAt j x $ Seq.deleteAt i s

answer1' :: String -> [Inst] -> String
answer1' start insts =
    toList $ List.foldl' (flip runInst) (Seq.fromList start) insts

-- 1:01:12
answer1 :: _ -> _
answer1 = answer1' "abcdefgh"

runBackwards :: Inst -> Seq Char -> Seq Char
runBackwards i@(SwapIndex _ _) s = runInst i s
runBackwards i@(SwapLetter _ _) s = runInst i s
runBackwards (RotRight i) s = runInst (RotLeft i) s
runBackwards (RotLeft i) s = runInst (RotRight i) s
runBackwards (RotBased x) s =
    case Maybe.fromJust $ Seq.elemIndexL x s of
        1 -> runInst (RotLeft 1) s
        3 -> runInst (RotLeft 2) s
        5 -> runInst (RotLeft 3) s
        7 -> runInst (RotLeft 4) s
        2 -> runInst (RotLeft 6) s
        4 -> runInst (RotLeft 7) s
        6 -> s
        0 -> runInst (RotLeft 1) s
runBackwards i@(Reverse _ _) s = runInst i s
runBackwards (Move i j) s = runInst (Move j i) s

-- 1:20:25
answer2' :: String -> [Inst] -> String
answer2' start insts =
    toList $ List.foldr runBackwards (Seq.fromList start) insts

answer2 :: _ -> _
answer2 = answer2' "fbgdceah"

show1 :: String -> String
show1 = id

show2 :: String -> String
show2 = id

tests :: _
tests = do
    describe "pure components" $ do
        it "should parse" $
            parse1 "move position 2 to position 1\n" `shouldBe` [Move 2 1]
        it "should parse" $
            parse1 "swap position 0 with position 2\n" `shouldBe`
            [SwapIndex 0 2]
        it "should parse" $
            parse1 "reverse positions 1 through 6\n" `shouldBe` [Reverse 1 6]
        it "should parse" $ parse1 "rotate left 1 step\n" `shouldBe` [RotLeft 1]
        it "should parse\n" $
            parse1
                "move position 2 to position 1\nmove position 2 to position 5\nmove position 2 to position 4\n" `shouldBe`
            [Move 2 1, Move 2 5, Move 2 4]
        it "should parse" $
            answer1'
                "abcde"
                (parse1
                     "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d\n") `shouldBe`
            "decab"
        it "should parse" $
            answer1' "abcde" (parse1 "swap position 4 with position 0\n") `shouldBe`
            "ebcda"
        it "should parse" $
            answer1'
                "abcde"
                (parse1
                     "swap position 4 with position 0\nswap letter d with letter b\n") `shouldBe`
            "edcba"
        it "should parse" $
            answer1'
                "abcde"
                (parse1
                     "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\n") `shouldBe`
            "abcde"
        it "should parse" $
            answer1'
                "abcde"
                (parse1
                     "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\n") `shouldBe`
            "bcdea"
    describe "part 1" $ do
        let p1 = Util.autoFileTest (answer1 . parse1)
        it "example 1" $ p1 "./ex1_1.txt" undefined
    describe "part 2" $ do
        let p2 = Util.autoFileTest (answer2 . parse2)
        it "example 5" $ p2 "./ex2_5.txt" undefined
