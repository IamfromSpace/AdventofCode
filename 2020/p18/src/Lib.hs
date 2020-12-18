module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (>>^), (|||), arr)
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

lit :: MD String Expr
lit = PA.digit >>^ (Lit . read . pure)

add :: MD String Op
add = ((expr >>! PA.string " + ") &&& expr) >>^ (const Add)

mult :: MD String Op
mult = ((expr >>! PA.string " * ") &&& expr) >>^ (const Mult)

op :: MD String Op
op = mult <+> add

e :: MD String Expr
e = (expr &&& PA.many (op &&& expr)) >>^ E

expr :: MD String Expr
expr = e <+> lit

justParse p = either (const (error "bad")) id . PA.runParser p

input :: MD String _
input = PA.sepBy1 expr (PA.char '\n')

parse1 :: String -> _
--parse1 = justParse input
parse1 = lines

parse2 :: String -> _
parse2 = parse1

doIt :: Maybe (Expr, [(Op, Expr)]) -> String -> (Expr, String)
doIt Nothing ('(':t) =
    let (inner, t') = doIt Nothing t
    in doIt (Just (inner, [])) t'
doIt Nothing (x:t) =
    (Lit (read (takeWhile Char.isDigit (x : t))), dropWhile Char.isDigit t)
doIt (Just (e, es)) (' ':'*':' ':t) =
    let (inner, t') = doIt Nothing t
    in doIt (Just (e, es <> [(Mult, inner)])) t'
doIt (Just (e, es)) (' ':'+':' ':t) =
    let (inner, t') = doIt Nothing t
    in doIt (Just (e, es <> [(Add, inner)])) t'
doIt (Just x) (')':t) = (E x, t)
doIt (Just x) [] = (E x, "")

toInt :: Expr -> Int
toInt (Lit x) = x
toInt (E (a, [])) = toInt a
toInt (E (a, (Add, b):t)) = toInt (E (Lit (toInt a + toInt b), t))
toInt (E (a, (Mult, b):t)) = toInt (E (Lit (toInt a * toInt b), t))

answer1 :: [String] -> Int
answer1 = sum . fmap (toInt . fst . doIt Nothing . (\xs -> "(" <> xs <> ")"))

--soooo lazy (not in the good Haskell-y way).
toInt2 :: Expr -> Int
toInt2 (Lit x) = x
toInt2 (E (a, [])) = toInt2 a
toInt2 (E (a, (Add, b):t)) = toInt2 (E (Lit (toInt2 a + toInt2 b), t))
toInt2 (E (a, (Mult, b):(Add, c):t)) =
    toInt2 (E (a, (Mult, Lit (toInt2 b + toInt2 c)) : t))
toInt2 (E (a, (Mult, b):(Mult, c):(Add, d):t)) =
    toInt2 (E (a, (Mult, b) : (Mult, Lit (toInt2 c + toInt2 d)) : t))
toInt2 (E (a, (Mult, b):(Mult, c):(Mult, d):(Add, e):t)) =
    toInt2
        (E (a, (Mult, b) : (Mult, c) : (Mult, Lit (toInt2 d + toInt2 e)) : t))
toInt2 (E (a, (Mult, b):(Mult, c):(Mult, d):(Mult, e):(Add, f):t)) =
    toInt2
        (E
             ( a
             , (Mult, b) :
               (Mult, c) : (Mult, d) : (Mult, Lit (toInt2 e + toInt2 f)) : t))
toInt2 (E (a, (Mult, b):t)) = toInt2 (E (Lit (toInt2 a * toInt2 b), t))

answer2 :: [_] -> _
answer2 = sum . fmap (toInt2 . fst . doIt Nothing . (\xs -> "(" <> xs <> ")"))

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
