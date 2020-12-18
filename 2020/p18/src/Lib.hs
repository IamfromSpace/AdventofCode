module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (>>^), (^>>), (|||), arr)
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
import Text.ParserCombinators.PArrow (MD, (>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.Read (readMaybe)

data Op
    = Add
    | Mult
    deriving (Show)

data Expr
    = Lit Int
    | E (Expr, [(Op, Expr)])
    deriving (Show)

-- lit = decimal
-- op = " * " | " + "
-- val = lit | e
-- e = "(" top ")"
-- top = val (op val)*
lit :: MD String Expr
lit = PA.digit >>^ (Lit . read . pure)

op :: MD String Op
op = (PA.string " + " >>^ const Add) <+> (PA.string " * " >>^ const Mult)

val :: MD String Expr
val = lit <+> e

top :: MD String Expr
top = (val &&& (PA.many1 (op &&& val))) >>^ E

e :: MD String Expr
e = PA.between (PA.string "(") (PA.string ")") top

input :: MD String [Expr]
input = PA.sepBy1 top (PA.char '\n')

parse1 :: String -> _
parse1 = either (error . unlines) id . PA.runParser input

parse2 :: String -> _
parse2 = parse1

toInt :: Expr -> Int
toInt (Lit x) = x
toInt (E (a, [])) = toInt a
toInt (E (a, (Add, b):t)) = toInt (E (Lit (toInt a + toInt b), t))
toInt (E (a, (Mult, b):t)) = toInt (E (Lit (toInt a * toInt b), t))

answer1 :: [_] -> Int
answer1 = sum . fmap toInt

toInt2' :: Int -> Expr -> Int
toInt2' _ (Lit x) = x
toInt2' !stack (E (a, [])) = toInt2 a * stack
toInt2' !stack (E (a, (Mult, b):t)) = toInt2' (toInt2 a * stack) (E (b, t))
toInt2' !stack (E (a, (Add, b):t)) =
    toInt2' stack (E (Lit (toInt2 a + toInt2 b), t))

toInt2 :: Expr -> Int
toInt2 = toInt2' 1

answer2 :: [_] -> _
answer2 = sum . fmap toInt2

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: _
ex1_1 = undefined

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: _
ex2_1 = undefined

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
