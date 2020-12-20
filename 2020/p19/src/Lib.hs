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
import Data.Foldable (fold, foldl1, toList)
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

(&<>&) :: Monoid a => MD i a -> MD i a -> MD i a
(&<>&) a b = a &&& b >>^ uncurry (<>)

type Rule = (String, Either Char [[String]])

dQuote :: MD String String
dQuote = PA.string "\""

baseRule :: MD String Char
baseRule = (PA.char ' ' &&& PA.between dQuote dQuote (PA.anyOf "ab")) >>^ snd

refRule :: MD String [[String]]
refRule =
    PA.sepBy1
        (PA.many1 (((PA.char ' ') &&& (PA.many1 PA.digit)) >>^ snd))
        (PA.string " |")

rule :: MD String Rule
rule =
    (PA.many1 PA.digit >>! PA.char ':') &&&
    ((baseRule >>^ Left) <+> (refRule >>^ Right))

input :: MD String ([Rule], [String])
input =
    (PA.many1 (rule >>! PA.string "\n")) &&&
    (PA.sepBy1 (PA.many (PA.anyOf "ab")) (PA.char '\n'))

parse1 :: String -> ([Rule], [String])
parse1 = either (error . unlines) id . PA.runParser input

parse2 :: String -> _
parse2 = parse1

buildParser :: Map String (Either Char [[String]]) -> String -> MD String String
buildParser m s =
    case Maybe.fromJust $ Map.lookup s m of
        Left c -> PA.string (pure c)
        Right xs ->
            foldl1 (<+>) $ fmap (foldl1 (&<>&) . fmap (buildParser m)) xs

answer1 :: ([Rule], [String]) -> _
answer1 (rules, msgs) =
    let parser = buildParser (Map.fromList rules) "0"
    in length $
       Either.rights $
       fmap (PA.runParser (parser >>! PA.notFollowedBy PA.anyChar)) msgs

-- Must match the second parser at least once, and then the first parse must
-- match more times than the second
moreAThanBMin1 :: MD a b -> MD a c -> MD a (([b], [c]), Bool)
moreAThanBMin1 a b =
    (PA.many1 a &&& PA.many1 b) >>^ (\x@(as, bs) -> (x, length as > length bs))

answer2 :: _ -> _
answer2 (rules, msgs) =
    let _42 = buildParser (Map.fromList rules) "42"
        _31 = buildParser (Map.fromList rules) "31"
    in length $
       filter (\x -> x == Right True) $
       fmap
           (PA.runParser
                (((moreAThanBMin1 _42 _31) >>^ snd) >>!
                 PA.notFollowedBy PA.anyChar))
           msgs

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
