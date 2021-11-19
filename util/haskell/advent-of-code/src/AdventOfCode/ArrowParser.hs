{-# LANGUAGE Arrows #-}
{-# LANGUAGE ViewPatterns #-}

-- | Arrow parsing provides almost the same power and expressiveness of monadic
-- parsers, while still lending itself to static analysis (though admittedly,
-- this library doesn't yet capitalize on such promises).  Also, it has the
-- benefits of applicative parsing, but arrow parsing recovers some of the
-- missing convenience of do notation via proc notation.
--
-- == Arrow Proc Notation
--
-- In general, the most ergonomic way to construct parsers is to use arrow proc
-- notation, which will figure out the right string of arrow operators for you.
-- Most parsers (but not all) don't have meaningful inputs, so as a slight
-- chore we need to ensure we pass in `()` for them.  The parser position is
-- internal state, so it need not be managed directly.
--
-- @
-- {-\# LANGUAGE Arrows \#-}
--
-- import Control.Arrow (returnA)
--
-- myNameIs :: APC () (String, Int)
-- myNameIs = proc () -> do
--     _ <- string "My name is " -< ()
--     name <- many1 (tokenIs (not . (==) ' ')) -< ()
--     _ <- string " and I am " -< ()
--     numStr <- many1 digit -< ()
--     _ <- string " year" -< ()
--     _ <- optional (token \'s\') -< ()
--     _ <- string " old." -< ()
--     end -< ()
--     returnA -< (name, read numStr)
-- @
--
-- Once we have a parser defined, we can execute it.
--
-- >>> parse myNameIs "My name is Alonzo and I am 118 years old."
-- ("Alonzo", 118)
--
-- == Arrow Operators
--
-- Alternatively (and often conveniently for small parsers), we can use arrow
-- operators to combine parsers.  Only a few operators are need to be of use to
-- us.
--
-- === `&&&`
--
-- `&&&` sequences two parsers, where the output of both is merged into a
-- single tuple.
--
-- >>> parse (token 'a' &&& token 'b') "ab"
-- Right ('a', 'b')
--
-- === `!>>` and `>>!`
--
-- These parsers are shortcuts like `&&&` (provide by this lib) but they only
-- consider the output of one of the two parsers.
--
-- >>> parse (token 'a' >>! token 'b') "ab"
-- Right 'a'
--
-- >>> parse (token 'a' !>> token 'b') "ab"
-- Right 'b'
--
-- === `<+>`
--
-- Two parsers can be combined to represent distinct options via the `<+>`
-- operator.  Whichever matches first will be output.
--
-- >>> parse (token 'a' <+> token 'b') "a"
-- Right 'a'
--
-- >>> parse (token 'a' <+> token 'b') "b"
-- Right 'b'
--
-- === `arr`
--
-- We can use `arr` to inject values and mappings into our parsing process.
-- Any function that is lifted into an arrow will act on the incoming value
-- /without/ advancing the parsing position.
--
-- >>> parse (arr (const "Hello")) ""
-- Right "Hello"
--
-- === `>>>` and `>>^`
--
-- Once one has a useful parse result, it can be \"mapped\" via `>>>`, often in
-- concern with `const`.  Or we can often simplify with `>>^`
--
-- >>> parse (token 'a' >>> arr toUpper) "a"
-- Right "A"
--
-- >>> parse (token 'a' >>^ toUpper) "a"
-- Right "A"
--
-- === etc
--
-- In the rare instances it is useful, `zeroError` acts as an \"empty list\" of
-- options, where the it must fail (and has no effect if valid options are
-- combined via `<+>`).
--
-- >>> parse zeroArrow ""
-- Left (0, DeadEnd)
--
-- Operators like `+++` and `|||` seem best left to proc notation's if/else and
-- case expressions, as they are dramatically more intuitive.
module AdventOfCode.ArrowParser
  ( AP,
    APError (..),
    APC,
    parse,

    -- * Parser Creators
    token,
    anyOf,
    anyToken,
    end,
    tokenIs,
    foldable,

    -- * Parser Combinators
    optional,
    many,
    many1,
    sepBy1,
    between,
    (!>>),
    (>>!),
    count,
    notFollowedBy,

    -- * String Parsers
    digit,
    string,
    linesOf,
    linesOf',
  )
where

import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), returnA, (>>>), (>>^), (^>>))
import Control.Category (Category (..))
import Data.Foldable (Foldable (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prelude hiding (foldr, id, (.))

-- | Possible parser errors.
data APError t
  = UnexpectedEndOfInput
  | NoOptionsMatch
  | UnexpectedToken t
  | -- | A `DeadEnd` is a special case of `NoOptionsMatch`, where no options were
    -- even presented.  This is triggered specifically via `zeroArrow`.  If `<+>`
    -- is akin to `<>`, then `zeroArrow` is akin to `mempty`.  The `mempty`
    -- instance of this parser is zero options (a dead end), and the parser must
    -- fail.  The way that options work, this satisfies a `mempty`, in that it
    -- has no effect, because failure just moves on to the next option.  As an
    -- example, all three of the following are equivalent:
    --
    -- @
    -- zeroArrow \<+\> p
    -- @
    -- @
    -- p \<+\> zeroArrow
    -- @
    -- @
    -- p
    -- @
    --
    -- This is useful in certain combinators like `notFollowedBy`.
    DeadEnd
  deriving (Show)

-- | Generic type for a parser, where t is the token type.
newtype AP t a b = AP
  { runAP :: (Seq t, Int, a) -> Either (Int, APError t) (Int, b)
  }

instance Category (AP t) where
  id = AP (\(_, i, a) -> Right (i, a))
  (runAP -> g) . (runAP -> f) =
    AP (\(s, i, a) -> f (s, i, a) >>= (\(i', b) -> g (s, i', b)))

instance Arrow (AP t) where
  arr f = AP (\(_, i, a) -> Right (i, f a))
  first (runAP -> f) =
    AP (\(s, i, (a, c)) -> (\(i', b) -> (i', (b, c))) <$> f (s, i, a))

instance ArrowZero (AP t) where
  zeroArrow = AP (\(_, i, _) -> Left (i, DeadEnd))

instance ArrowPlus (AP t) where
  (runAP -> f) <+> (runAP -> g) =
    AP
      ( \x@(_, i, _) ->
          case f x of
            Right x -> Right x
            Left e1 -> case (g x) of
              Right y -> Right y
              Left e2 -> Left (i, NoOptionsMatch)
      )

instance ArrowChoice (AP t) where
  left (runAP -> f) =
    AP
      ( \(s, i, ea) ->
          case ea of
            Left a -> case f (s, i, a) of
              Left i -> Left i
              Right (i', b) -> Right (i', Left b)
            Right d -> Right (i, Right d)
      )

-- | A parser with a concrete token type (Char).  For parsing Strings.
type APC = AP Char

-- | Execute a parser.  Note that this only consumes all the input if that is
-- explicitly part of the parser.
--
-- >>> parse (token 'a') "a"
-- Right 'a'
--
-- >>> parse (token 'a') ""
-- Left (0, UnexpectedEndOfInput)
--
-- >>> parse (token 'a') "aa"
-- Right 'a'
--
-- >>> parse (token 'a' >>! end) "aa"
-- Left (1, UnexpectedToken 'a')
parse :: Foldable f => AP t () a -> f t -> Either (Int, APError t) a
parse (runAP -> f) s = snd <$> f (Seq.fromList $ toList s, 0, ())

-- | A parser that accepts any token that satisfies the predicate.
--
-- >>> parse (tokenIs isUpper) "A"
-- Right 'A'
--
-- >>> parse (tokenIs isUpper) "a"
-- Left (0, UnexpectedToken 'a')
tokenIs :: (t -> Bool) -> AP t () t
tokenIs f =
  AP
    ( \(s, i, _) ->
        case Seq.lookup i s of
          Nothing -> Left (i, UnexpectedEndOfInput)
          Just t ->
            if f t
              then Right (i + 1, t)
              else Left (i, UnexpectedToken t)
    )

-- | A parser that matches the end of input only.  Useful if you want to ensure
-- the entire input is consumed.
--
-- >>> parse end ""
-- Right ()
--
-- >>> parse end "a"
-- Left (0, UnexpectedToken 'a')
end :: AP t () ()
end =
  AP
    ( \(s, i, _) ->
        case Seq.lookup i s of
          Nothing -> Right (i, ())
          Just t -> Left (i, UnexpectedToken t)
    )

-- | A parser that exactly matches a single token.
--
-- >>> parse (token 'a') "a"
-- Right 'a'
--
-- >>> parse (token 'a') "b"
-- Left (0, UnexpectedToken 'b')
token :: Eq t => t -> AP t () t
token = tokenIs . (==)

-- | A parser that matches any token, but not end of input.
--
-- >>> parse anyToken "a"
-- Right 'a'
--
-- >>> parse anyToken ""
-- Left (0, UnexpectedEndOfInput)
anyToken :: AP t () t
anyToken = tokenIs (const True)

-- | A parser that matches any token once in the provided list (or foldable).
--
-- >>> parse (anyOf "ab") "a"
-- Right 'a'
--
-- >>> parse (anyOf "ab") "b"
-- Right 'b'
--
-- >>> parse (anyOf "ab") "c"
-- Left (0, UnexpectedToken 'c')
anyOf :: Eq t => Foldable f => f t -> AP t () t
anyOf = foldr (\t p -> p <+> token t) zeroArrow

-- | A combinator that turns a parser into a parser that can fail into a Maybe.
-- Notably, the parser position will not advance on failure, even in the case
-- of a partial match.
--
-- >>> parse (optional (token 'a')) "a"
-- Right (Just 'a')
--
-- >>> parse (optional (token 'a')) ""
-- Right Nothing
optional :: AP t a b -> AP t a (Maybe b)
optional p = (p >>^ Just) <+> (arr (const Nothing))

-- | A combinator that greedily matches as many repetitions of a parser as
-- possible (and possibly not at all).
--
-- >>> parse (many (token 'a')) "aaa"
-- Right "aaa"
--
-- >>> parse (many (token 'a')) ""
-- Right ""
many :: AP t a b -> AP t a [b]
many p = proc a -> do
  mx <- optional p -< a
  case mx of
    Just x -> do
      xs <- many p -< a
      returnA -< (x : xs)
    Nothing -> do
      returnA -< []

-- | A combinator that greedily matches as many repetitions of a parser as
-- possible (and at minimum once).
--
-- >>> parse (many1 (token 'a')) "aaa"
-- Right "aaa"
--
-- >>> parse (many1 (token 'a')) ""
-- Left (0, NoOptionsMatch)
many1 :: AP t a b -> AP t a [b]
many1 p = proc a -> do
  x <- p -< a
  xs <- many p -< a
  returnA -< (x : xs)

-- | A combinator that greedily matches as many repetitons of on parser that
-- separates another.  Very useful for things like comma delimited input.  This
-- fails on 'dangling' separators.
--
-- >>> parse (sepBy1 (token 'a') (token ',')) "a,a"
-- Right "aa"
--
-- >>> parse (sepBy1 (token 'a') (token ',')) "a"
-- Right "a"
--
-- >>> parse (sepBy1 (token 'a') (token ',')) "a,"
-- Left (2, UnexpectedEndOfInput)
sepBy1 :: AP t a b -> AP t a b' -> AP t a [b]
sepBy1 p sep = proc a -> do
  xs <- many ((p &&& sep) >>^ fst) -< a
  x <- p -< a
  returnA -< (xs <> [x])

-- | A combinator that matches a parser between two others.  This is useful for
-- things like quotations.
--
-- >>> parse (between (token '[') (token 'a') (token ']')) "[a]"
-- Right 'a'
--
-- >>> parse (between (token '[') (token 'a') (token ']')) "a]"
-- Left (0, UnexpectedToken 'a')
--
-- >>> parse (between (token '[') (token 'a') (token ']')) "[a"
-- Left (2, UnexpectedEndOfInput)
between :: AP t a open -> AP t a close -> AP t a b -> AP t a b
between o c p = proc a -> do
  o -< a
  x <- p -< a
  c -< a
  returnA -< x

-- | Match a single numeric character.  Useful for building out numeric parsers.
--
-- >>> parse digit "4"
-- Right '4'
--
-- >>> parse digit "a"
-- Left (0, UnexpectedToken 'a')
digit :: APC () Char
digit = anyOf "0123456789"

-- | Match a string of tokens held inside any instance of foldable.  This can
-- save some conversions if the concrete type is known and it is not a
-- String--otherwise it may cause more issues (via type inference) than it
-- solves.
--
-- >>> parse (foldable ("123" :: String)) "123"
-- Right "123"
--
-- >>> parse (foldable ("123" :: String)) "124"
-- Left (2, UnexpectedToken '4')
foldable :: Eq t => Foldable f => f t -> AP t () (f t)
foldable ts = (foldr (\t ps -> (token t &&& ps) >>^ const ()) (arr id) ts) >>^ const ts

-- | A combinator that matches another parser an exact number of times.
-- Notably, the count is *part* of the arrow, which means that it can be
-- determined based on the input itself, which we would typically only expect
-- to do with a monadic parser.  This is especially useful when parsing binary
-- formats that hint length or repetition of sections.
--
-- >>> parse (arr (const (3, ())) >>> count (token 'a')) "aaa"
-- Right "aaa"
--
-- >>> parse (arr (const (3, ())) >>> count (token 'a')) "aab"
-- Left (2, UnexpectedToken 'b')
--
-- >>> parse (digit >>> arr (\n -> (read [n], ())) >>> count (token 'a')) "3aaa"
-- Right "aaa"
--
-- >>> parse (digit >>> arr (\n -> (read [n], ())) >>> count (token 'a')) "4aaaa"
-- Right "aaaa"
--
-- >>> parse (digit >>> arr (\n -> (read [n], ())) >>> count (token 'a')) "2a"
-- Left (2, UnexpectedEndOfInput)
count :: AP t a b -> AP t (Int, a) [b]
count p = proc (i, a) -> do
  if i > 0
    then do
      x <- p -< a
      xs <- count p -< (i -1, a)
      returnA -< (x : xs)
    else returnA -< []

-- | A combinator that executes two parsers in sequence, but ingores the output
-- of the first (the side with the !).
--
-- >>> parse (token 'a' !>> token 'b') "ab"
-- Right 'b'
(!>>) :: AP t a b' -> AP t a b -> AP t a b
(!>>) a b = (a &&& b) >>^ snd

-- | A combinator that executes two parsers in sequence, but ingores the output
-- of the second (the side with the !).
--
-- >>> parse (token 'a' >>! token 'b') "ab"
-- Right 'a'
(>>!) :: AP t a b -> AP t a b' -> AP t a b
(>>!) a b = (a &&& b) >>^ fst

-- | A parser that succeeds if looking ahead fails.  Notably, the parser
-- position does not advance even on a partial match.
--
-- >>> parse (notFollowedBy (token 'a')) ""
-- Right ()
--
-- >>> parse (notFollowedBy (token 'a')) "b"
-- Right ()
--
-- >>> parse (notFollowedBy (token 'a')) "a"
-- Left (0, DeadEnd)
--
-- This parser uses the `DeadEnd` error, because it leverages `zeroArrow` to
-- trigger failure.  At a high level, this parser works by sequencing a
-- `zeroArrow` after the supplied parser, so that if it matches it will
-- encounter the `DeadEnd` and fail.
notFollowedBy :: AP t a b -> AP t a ()
notFollowedBy p = proc a -> do
  didMatch <- (p >>^ const True) <+> arr (const False) -< a
  if didMatch
    then do
      zeroArrow -< ()
    else do
      arr (const ()) -< ()

-- | Create a parser that matches an exact String.
--
-- >>> parse (string "123") "123"
-- Right "123"
--
-- >>> parse (string "123") "124"
-- Left (2, UnexpectedToken '4')
string :: String -> APC () String
string = foldable

-- | A combinator that given a particular parser, matches many times as
-- separated by newlines.  This is like `sepBy1`, but acts more like the
-- Prelude `lines` function, where a trailing newline is optional. Caution, the
-- parser must match the entire line and must never match '\n'!  Also, just as
-- each line must match entirely, this combinator has an implicit end, to
-- ensure that the last line is also consumed in entirety.  Consider `linesOf'`
-- if you want more lienient behavior.
--
-- >>> parse (linesOf (token 'a')) "a"
-- Right "a"
--
-- >>> parse (linesOf (token 'a')) "a\n"
-- Right "a"
--
-- >>> parse (linesOf (token 'a')) "a\na\n"
-- Right "aa"
--
-- >>> parse (linesOf (token 'a')) "a\naa\n"
-- Left (3, UnexpectedToken 'a')
--
-- >>> parse (linesOf (token 'a')) "a\na\naa"
-- Left (5, UnexpectedToken 'a')
linesOf :: APC a b -> APC a [b]
linesOf p =
  ( ( many (p >>! (arr (const ()) >>> token '\n'))
        &&& optional p
    )
      >>! (arr (const ()) >>> end)
  )
    >>^ (\(a, b) -> a <> maybe [] pure b)

-- | A lenient version of `linesOf`.
--
-- A combinator that given a particular parser, matches many times as separated
-- by newlines.  This is like `sepBy1`, but acts more like the Prelude `lines`
-- function, where a trailing newline is optional. This version is more lenient
-- than `linesOf`, in that it will automatically throw away any excess
-- characters between the end of the parser and the next newline.  Still, be
-- cautious not to match a `'\n'` or it will not treat each line separately.
--
-- Notice how extra 'a's are always ignored here (vs `linesOf`):
--
-- >>> parse (linesOf' (token 'a')) "aaa"
-- Right "a"
--
-- >>> parse (linesOf' (token 'a')) "aaa\n"
-- Right "a"
--
-- >>> parse (linesOf' (token 'a')) "aaa\naaa\n"
-- Right "aa"
--
-- >>> parse (linesOf' (token 'a')) "aaa\naaa\naaa"
-- Right "aaa"
--
-- >>> parse (linesOf' (token 'a') &&& (token '-' !>> token 'b')) "a\na\na-b"
-- Right ("aaa", 'b')
linesOf' :: APC a b -> APC a [b]
linesOf' p =
  ( ( many
        (p >>! (const () ^>> (many (tokenIs (not . (==) '\n')) !>> token '\n')))
    )
      &&& optional p
  )
    >>^ (\(a, b) -> a <> maybe [] pure b)
