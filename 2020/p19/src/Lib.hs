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

(&<>&) :: Monoid a => MD i a -> MD i a -> MD i a
(&<>&) a b = a &&& b >>^ uncurry (<>)

countAndFold :: Monoid a => Int -> MD i a -> MD i a
countAndFold n a = PA.count n a >>^ fold

_17 :: MD String String
_17 = (_72 &<>& _112) <+> (_71 &<>& _3)

_31 :: MD String String
_31 = (_71 &<>& _33) <+> (_72 &<>& _57)

_49 :: MD String String
_49 = (_72 &<>& _110) <+> (_71 &<>& _45)

_37 :: MD String String
_37 = (_71 &<>& _68) <+> (_72 &<>& _45)

_124 :: MD String String
_124 = (_71 &<>& _128) <+> (_72 &<>& _64)

_1 :: MD String String
_1 = (_105 &<>& _71) <+> (_29 &<>& _72)

_44 :: MD String String
_44 = (_72 &<>& _130) <+> (_71 &<>& _93)

_93 :: MD String String
_93 = (_72 &<>& _45) <+> (_71 &<>& _105)

_126 :: MD String String
_126 = (_34 &<>& _71) <+> (_59 &<>& _72)

_89 :: MD String String
_89 = (_71 &<>& _16) <+> (_72 &<>& _118)

_92 :: MD String String
_92 = (_21 &<>& _71) <+> (_12 &<>& _72)

_55 :: MD String String
_55 = (_47 &<>& _72) <+> (_61 &<>& _71)

_7 :: MD String String
_7 = (_29 &<>& _72) <+> (_29 &<>& _71)

_45 :: MD String String
_45 = (_72 &<>& _72) <+> (_72 &<>& _71)

_28 :: MD String String
_28 = (_110 &<>& _71) <+> (_109 &<>& _72)

_10 :: MD String String
_10 = (_110 &<>& _72) <+> (_63 &<>& _71)

_29 :: MD String String
_29 = _72 &<>& _71

_27 :: MD String String
_27 = (_71 &<>& _53) <+> (_72 &<>& _13)

_107 :: MD String String
_107 = _45 &<>& _71

_84 :: MD String String
_84 = (_5 &<>& _71) <+> (_39 &<>& _72)

_119 :: MD String String
_119 = (_72 &<>& _105) <+> (_71 &<>& _110)

_53 :: MD String String
_53 = (_28 &<>& _71) <+> (_10 &<>& _72)

_18 :: MD String String
_18 = (_72 &<>& _86) <+> (_71 &<>& _71)

_25 :: MD String String
_25 = (_110 &<>& _72) <+> (_105 &<>& _71)

_82 :: MD String String
_82 = (_106 &<>& _71) <+> (_75 &<>& _72)

_46 :: MD String String
_46 = (_29 &<>& _71) <+> (_63 &<>& _72)

_71 :: MD String String
_71 = (PA.char 'a') >>^ pure

_129 :: MD String String
_129 = (_49 &<>& _71) <+> (_80 &<>& _72)

_123 :: MD String String
_123 = (_12 &<>& _71) <+> (_79 &<>& _72)

_111 :: MD String String
_111 = (_110 &<>& _72) <+> (_96 &<>& _71)

_35 :: MD String String
_35 = (_72 &<>& _107) <+> (_71 &<>& _104)

_113 :: MD String String
_113 = (_10 &<>& _72) <+> (_1 &<>& _71)

_106 :: MD String String
_106 = (_26 &<>& _72) <+> (_6 &<>& _71)

_8 :: MD String String
_8 = _42

_8n :: Int -> MD String String
_8n n = countAndFold n _42

_36 :: MD String String
_36 = (_71 &<>& _10) <+> (_72 &<>& _79)

_26 :: MD String String
_26 = (_72 &<>& _122) <+> (_71 &<>& _51)

_74 :: MD String String
_74 = (_48 &<>& _72) <+> (_2 &<>& _71)

_5 :: MD String String
_5 = (_72 &<>& _104) <+> (_71 &<>& _22)

_120 :: MD String String
_120 = (_71 &<>& _48) <+> (_72 &<>& _2)

_64 :: MD String String
_64 = (_72 &<>& _96) <+> (_71 &<>& _56)

_72 :: MD String String
_72 = (PA.char 'b') >>^ pure

_131 :: MD String String
_131 = (_63 &<>& _72) <+> (_56 &<>& _71)

_79 :: MD String String
_79 = _56 &<>& _71

_23 :: MD String String
_23 = (_72 &<>& _96) <+> (_71 &<>& _29)

_6 :: MD String String
_6 = (_71 &<>& _111) <+> (_72 &<>& _32)

_43 :: MD String String
_43 = (_71 &<>& _73) <+> (_72 &<>& _70)

_78 :: MD String String
_78 = (_14 &<>& _71) <+> (_74 &<>& _72)

_16 :: MD String String
_16 = (_71 &<>& _29)

_54 :: MD String String
_54 = (_110 &<>& _72) <+> (_18 &<>& _71)

_80 :: MD String String
_80 = (_2 &<>& _72) <+> (_109 &<>& _71)

_75 :: MD String String
_75 = (_108 &<>& _72) <+> (_30 &<>& _71)

_13 :: MD String String
_13 = (_25 &<>& _71) <+> (_116 &<>& _72)

_132 :: MD String String
_132 = (_15 &<>& _72) <+> (_24 &<>& _71)

_70 :: MD String String
_70 = (_114 &<>& _71) <+> (_113 &<>& _72)

_112 :: MD String String
_112 = (_7 &<>& _71) <+> (_131 &<>& _72)

_88 :: MD String String
_88 = (_72 &<>& _89) <+> (_71 &<>& _102)

_130 :: MD String String
_130 = (_71 &<>& _110) <+> (_72 &<>& _56)

_0 :: MD String String
_0 = _8 &<>& _11

_0n :: MD String String -> MD String String -> MD String String
_0n _8 _11 = _8 &<>& _11

_117 :: MD String String
_117 = (_66 &<>& _71) <+> (_27 &<>& _72)

_127 :: MD String String
_127 = (_72 &<>& _126) <+> (_71 &<>& _67)

_52 :: MD String String
_52 = (_72 &<>& _109) <+> (_71 &<>& _96)

_122 :: MD String String
_122 = (_45 &<>& _72) <+> (_2 &<>& _71)

_103 :: MD String String
_103 = (_72 &<>& _44) <+> (_71 &<>& _36)

_58 :: MD String String
_58 = (_72 &<>& _41) <+> (_71 &<>& _35)

_114 :: MD String String
_114 = (_72 &<>& _121) <+> (_71 &<>& _52)

_85 :: MD String String
_85 = (_72 &<>& _120) <+> (_71 &<>& _23)

_19 :: MD String String
_19 = (_72 &<>& _18) <+> (_71 &<>& _68)

_2 :: MD String String
_2 = (_72 &<>& _71) <+> (_71 &<>& _86)

_65 :: MD String String
_65 = (_54 &<>& _71) <+> (_90 &<>& _72)

_96 :: MD String String
_96 = (_71 &<>& _71) <+> (_71 &<>& _72)

_69 :: MD String String
_69 = (_72 &<>& _17) <+> (_71 &<>& _40)

_116 :: MD String String
_116 = (_71 &<>& _48) <+> (_72 &<>& _29)

_66 :: MD String String
_66 = (_72 &<>& _100) <+> (_71 &<>& _92)

_115 :: MD String String
_115 = (_72 &<>& _48) <+> (_71 &<>& _63)

_39 :: MD String String
_39 = (_72 &<>& _62) <+> (_71 &<>& _83)

_22 :: MD String String
_22 = _71 &<>& _96

_50 :: MD String String
_50 = (_105 &<>& _71) <+> (_134 &<>& _72)

_108 :: MD String String
_108 = (_87 &<>& _72) <+> (_46 &<>& _71)

_34 :: MD String String
_34 = (_55 &<>& _72) <+> (_9 &<>& _71)

_11 :: MD String String
_11 = _42 &<>& _31

_11n :: Int -> MD String String
_11n n = (countAndFold n _42) &<>& (countAndFold n _31)

_14 :: MD String String
_14 = (_56 &<>& _71) <+> (_96 &<>& _72)

_24 :: MD String String
_24 = (_58 &<>& _72) <+> (_95 &<>& _71)

_83 :: MD String String
_83 = (_72 &<>& _45) <+> (_71 &<>& _29)

_47 :: MD String String
_47 = (_72 &<>& _110) <+> (_71 &<>& _109)

_94 :: MD String String
_94 = (_37 &<>& _72) <+> (_97 &<>& _71)

_12 :: MD String String
_12 = _48 &<>& _72

_51 :: MD String String
_51 = (_72 &<>& _48) <+> (_71 &<>& _68)

_98 :: MD String String
_98 = (_109 &<>& _71) <+> (_18 &<>& _72)

_32 :: MD String String
_32 = (_72 &<>& _105) <+> (_71 &<>& _68)

_95 :: MD String String
_95 = (_72 &<>& _77) <+> (_71 &<>& _78)

_118 :: MD String String
_118 = (_72 &<>& _68) <+> (_71 &<>& _2)

_21 :: MD String String
_21 = (_72 &<>& _45) <+> (_71 &<>& _134)

_59 :: MD String String
_59 = (_71 &<>& _123) <+> (_72 &<>& _133)

_105 :: MD String String
_105 = _71 &<>& _71

_76 :: MD String String
_76 = (_72 &<>& _109) <+> (_71 &<>& _29)

_56 :: MD String String
_56 = (_71 &<>& _71) <+> (_72 &<>& _72)

_68 :: MD String String
_68 = (_72 &<>& _86) <+> (_71 &<>& _72)

_3 :: MD String String
_3 = (_107 &<>& _72) <+> (_19 &<>& _71)

_86 :: MD String String
_86 = _72 <+> _71

_33 :: MD String String
_33 = (_72 &<>& _69) <+> (_71 &<>& _43)

_128 :: MD String String
_128 = (_2 &<>& _72) <+> (_68 &<>& _71)

_67 :: MD String String
_67 = (_72 &<>& _84) <+> (_71 &<>& _125)

_60 :: MD String String
_60 = (_29 &<>& _71) <+> (_110 &<>& _72)

_100 :: MD String String
_100 = (_79 &<>& _72) <+> (_4 &<>& _71)

_97 :: MD String String
_97 = (_56 &<>& _72) <+> (_45 &<>& _71)

_133 :: MD String String
_133 = (_76 &<>& _71) <+> (_122 &<>& _72)

_104 :: MD String String
_104 = (_45 &<>& _72) <+> (_109 &<>& _71)

_109 :: MD String String
_109 = (_72 &<>& _71) <+> (_71 &<>& _71)

_30 :: MD String String
_30 = (_60 &<>& _71) <+> (_4 &<>& _72)

_99 :: MD String String
_99 = (_71 &<>& _50) <+> (_72 &<>& _38)

_9 :: MD String String
_9 = (_115 &<>& _72) <+> (_101 &<>& _71)

_87 :: MD String String
_87 = (_68 &<>& _71) <+> (_63 &<>& _72)

_77 :: MD String String
_77 = (_81 &<>& _72) <+> (_20 &<>& _71)

_101 :: MD String String
_101 = (_71 &<>& _2) <+> (_72 &<>& _110)

_20 :: MD String String
_20 = (_71 &<>& _45) <+> (_72 &<>& _56)

_40 :: MD String String
_40 = (_129 &<>& _71) <+> (_65 &<>& _72)

_42 :: MD String String
_42 = (_127 &<>& _71) <+> (_132 &<>& _72)

_91 :: MD String String
_91 = (_71 &<>& _105) <+> (_72 &<>& _109)

_81 :: MD String String
_81 = _72 &<>& _45

_62 :: MD String String
_62 = (_45 &<>& _72) <+> (_63 &<>& _71)

_61 :: MD String String
_61 = (_71 &<>& _110) <+> (_72 &<>& _63)

_4 :: MD String String
_4 = (_71 &<>& _134) <+> (_72 &<>& _134)

_57 :: MD String String
_57 = (_82 &<>& _72) <+> (_117 &<>& _71)

_41 :: MD String String
_41 = (_71 &<>& _91) <+> (_72 &<>& _116)

_134 :: MD String String
_134 = _71 &<>& _72

_73 :: MD String String
_73 = (_71 &<>& _99) <+> (_72 &<>& _85)

_90 :: MD String String
_90 = (_63 &<>& _71) <+> (_2 &<>& _72)

_15 :: MD String String
_15 = (_71 &<>& _103) <+> (_72 &<>& _88)

_38 :: MD String String
_38 = _72 &<>& _63

_121 :: MD String String
_121 = (_134 &<>& _71) <+> (_96 &<>& _72)

_110 :: MD String String
_110 = _86 &<>& _86

_48 :: MD String String
_48 = (_71 &<>& _72) <+> (_72 &<>& _72)

_63 :: MD String String
_63 = (_72 &<>& _71) <+> (_71 &<>& _72)

_125 :: MD String String
_125 = (_71 &<>& _124) <+> (_72 &<>& _94)

_102 :: MD String String
_102 = (_119 &<>& _72) <+> (_98 &<>& _71)

parse1 :: String -> [String]
parse1 = head . drop 1 . multiLines

parse2 :: String -> _
parse2 = parse1

answer1 :: [String] -> _
answer1 =
    length .
    Either.rights . fmap (PA.runParser (_0 >>! PA.notFollowedBy PA.anyChar))

answer2 :: _ -> _
answer2 xs =
    length $
    Set.toList $
    fold $
    App.liftA2
        (\n8 n11 ->
             Set.fromList $
             Either.rights $
             fmap
                 (PA.runParser
                      (_0n (_8n n8) (_11n n11) >>! PA.notFollowedBy PA.anyChar))
                 xs)
        [1 .. 7]
        [1 .. 7]

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
