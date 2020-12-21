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
import Data.Foldable (fold, toList)
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

type Food = (Set String, Set String)

parseLine :: String -> Food
parseLine s =
    let [i, a'] = Split.splitOn " (contains " s
        (a:_) = Split.splitOn ")" a'
    in (Set.fromList (Split.splitOn " " i), Set.fromList (Split.splitOn ", " a))

parse1 :: String -> _
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

inters' :: Map String (Set String) -> [Food] -> Map String (Set String)
inters' !build ((is, as):t) =
    inters'
        (List.foldl'
             (\m a ->
                  case Map.lookup a m of
                      Just is' -> Map.insert a (is `Set.intersection` is') m
                      Nothing -> Map.insert a is m)
             build $
         Set.toList as)
        t
inters' !build [] = build

inters :: [Food] -> Map String (Set String)
inters = inters' mempty

-- Done by hand on scratch based on the result of `inters`
mapping :: [(String, String)]
mapping =
    [ ("dairy", "zfcqk")
    , ("fish", "mdtvbb")
    , ("nuts", "ggdbl")
    , ("peanuts", "frpvd")
    , ("sesame", "mgczn")
    , ("shellfish", "zsfzq")
    , ("soy", "kdqls")
    , ("wheat", "kktsjbh")
    ]

answer1 :: [Food] -> _
answer1 foods =
    let allIngredients = fold (fmap fst foods)
        transAlergens = Set.fromList $ fmap snd mapping
        goodIngredients =
            Set.filter (not . flip Set.member transAlergens) allIngredients
    in length $
       filter (flip Set.member goodIngredients) $
       concatMap (Set.toList . fst) foods

answer2 :: [Food] -> _
answer2 _ = List.intercalate "," $ fmap snd $ List.sort mapping

show1 :: Show _a => _a -> String
show1 = show

show2 :: a -> a
show2 = id

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
