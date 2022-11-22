{-|
  Monadic parser and helper functions.
-}

module Parsing (
  Parser, -- | parser type
  parse, -- | running parser on a string
  sat,
  item,
  failure, success, -- | basic utility parsers to build others on top of them; useless on their own
  oneOrMore, zeroOrMore, zeroOrOne, -- | combined utility parsers
  (<|>), -- | parser combinator; "or" operator if you will
  (<*>), -- | sequence operator from `Applicative` instance of a `Parser`
  (<$>), -- | infix alias for `fmap` from `Functor` instance of a `Parser`
  return
)
where

import Data.Char
import Data.Maybe
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (liftM, ap)

{-|
  Type constructor: a parser is a function from a string
  that returns a Maybe of something that was parsed and
  an unparsed leftovers of a string.

  A `Just (a, str)` would denote a successful parsing,
  whereas `Nothing` would denote a parsing failure.
-}
newtype Parser a = P (String -> Maybe (a, String))

{-|
  Runs a parser function on a given string.
-}
parse :: Parser a -> String -> Maybe (a, String)
parse (P fn) str = fn str

{-|
  A utility parser which always fails (returns Nothing).
-}
failure :: Parser a
failure = P (\_ -> Nothing)

{-|
  A utility parser which always succeeds (returns a Just of a default value and an entire string).
-}
success :: a -> Parser a
success a = P (\str -> Just (a, str))

{-|
  A parser which expects any one or more characters in a string.
  Would fail for an empty string.
-}
item :: Parser Char
item = P $ \str -> case str of
  "" -> Nothing
  (ch:chs) -> Just (ch, chs)

{-|
  A parser which takes a predicate (function from character to boolean)
  and succeeds only if that predicate satisfies the first character of a string.
-}
sat :: (Char -> Bool) -> Parser Char
sat pred = item >>= (\str -> if pred str then success str else failure)

{-|
  A combined parser, which takes a parser as an argument and returns a combination of parsers.

  A given parser `p` will be executed on a string while it succeeds and the whole combination
  will return success if it succeeded even once.
  If `p` fails - the combination will still return success (but an empty one, for that matter).

  This utilizes the combination operator of a parser, `<|>` (see below)
  to combine two parsers together.
-}
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> success []

{-|
  A combined parser, which takes a parser as an argument and returns a combination of parsers.

  A given parser `p` will be executed on a string while it succeeds and the whole combination
  will return success if it succeeded even once.
  If the parser `p` fails - the combination will fail immediately.

  This utilizes the `bind` (aka monadic `map`) operator of a parser
  to combine two parsers together.
-}
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = p >>= (\a -> fmap (a:) (zeroOrMore p))

{-|
  A combined parser, which takes a parser as an argument and returns a combination of parsers.

  The combination will succeed if either the parser `p` succeeded or not, but won't run `p` more than once
  on a string (unline `zeroOrMore` and `oneOrMore` combinators).

  This utilizes the combination operator of a parser, `<|>` (see below)
  to combine two parsers together.
-}
zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne p = (p >>= (\a -> success (Just a))) <|> success Nothing

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = success -- | constructs a parser with a default "success" behaviour

  (<*>) = ap

instance Monad Parser where
  {-|
    Instance of a `bind` operator (or monadic `map`), which chains two parsers together.
    It short-circuit the execution if a parser `p1` fails.
    If parser `p1` succeeds, it will return the result of running parser `p2` on the unparsed portion of a string,
    returned by running `p1`.
  -}
  p1 >>= p2 = P $ \str -> case parse p1 str of
    Just (a, str') -> parse (p2 a) str'
    Nothing -> Nothing

  return = pure

instance Alternative Parser where
  empty = failure

  {-|
    Instance of a `<|>` operator, which combines two parsers together in a "if one fails - try another" manner.
    If parser `p1` fails, it will return the result of running parser `p1` on an input string.
    If parser `p1` fails, it will run parser `p2` on an input string and return that result.
  -}
  p1 <|> p2 = P $ \str -> case parse p1 str of
    Nothing -> parse p2 str
    result -> result
