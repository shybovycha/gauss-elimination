{-|
  Monadic parser and helper functions.

  More like a state machine for consuming the characters satisfying the rules, while they match (satisfy) the rules.

  The idea is that the parser is an annotated function from an input string to _maybe_ something (parsing result) and a string (the unparsed part).
  This module defines few trivial parsers which, when combined together, make up more complex and meaningful parsers.
  The parsers presented in this module are:

  * `success a` - always succeeds, returning a `Just (a, str)` (some default value and an entire input string)
  * `failure` - always fails, returning `Nothing`
  * `item` - parses any one arbitrary character, returning `Just (chr, str)`
    (first character and the rest of the string) if the input string is non-empty or `Nothing` if the input string is empty
  * `parser1 <|> parser2` - tries the first parser, `parser1` and if it succeeds - returns the result,
    but if it fails - returns whatever running the second parser, `parser2` returns
  * `parser1 <*> (a -> parser2)` - chains two parsers together, runs `parser1` and passes its output string to the second parser, `parser2`;
    but since a parser result is a tuple, not just a string that can be passed to the second parser, this function takes a _function_
    from the output of the first parser to the second parser

  Since a parser is just a function `String -> Maybe (a, String)`, in order to __parse__ something, one needs to __run__ it.
  Running a parser is as easy as executing a function `parse`, passing it a parser and an input string:

  >>> parse item "123"

  These basic building blocks are then extended with a bit more complex building blocks, which are a little bit more handy for building
  complex parsers:

  * `sat (ch -> Bool)` - returns a parser which only succeeds if the first character of a (non-empty) input string satisfies the function (`ch -> Bool`)
  * `zeroOrMore p` - returns a parser which succeeds if the parser `p` (passed as a param) succeeds on the input string (potentially multiple times)
    or fails - regardless, the resulting parser will succeed
  * `zeroOrOne p` - returns a parser which succeeds if the parser `p` (passed as a param) succeeds exactly once on an input string or if it fails
  * `oneOrOne p` - returns a parser which succeeds if the parser `p` (passed as a param) succeeds once or more on an input string

  With these building blocks, one can build more complex parsers:

  __parsing a digit:__

  @
  digit = sat isDigit
  @

  >>> (parse digit) "123abc"

  __parsing a number (as a string):__

  @
  numberStr = oneOrMore (sat isDigit)
  @

  >>> (parse numberStr) "123abc"

  Since `Parser` is an instance of `Monad`, you can use `fmap` or `<$>` to combine it with other functions:

  __parsing a number (as a non-negative number):__

  @
  naturalNumber :: Parser Integer
  naturalNumber = read <$> oneOrMore (sat isDigit)
  @

  >>> (parse naturalNumber) "123abc"

  __parsiung a potentially negative number:__

  @
  sign = fmap (maybe 1 (\_ -> -1)) (zeroOrOne (sat (== '-')))
  intNumber = ((*) <$> sign) <*> number
  @

  The code above requires a bit of an explanation, I guess.
  The first part,

  @
  sign = fmap (maybe 1 (\_ -> -1)) (zeroOrOne (sat (== '-')))
  @

  parses a potential '-' sign at the beginning of a string and returns either `1` or `-1`, aka the sign multiplier.
  The tricky part is `fmap (maybe 1 (\_ -> -1)) ...` - the `zeroOrOne` parser will return `Maybe a` -- that is, `Maybe (Maybe a, String)`.
  So if the wrapped part (in this case - `sat (== '-')`) is present in the string, it will return `Just a` (in this case - `Just '-'`).
  Hence we need to cast this `Maybe Char` to something reasonable - a number would do. We call the `maybe` helper with two params - the first
  one is what would be returned if it is applied to `Nothing` and the second one is a function which will be called on the value wrapped in `Just`
  the thing is applied to:

  >>> maybe 1 (\_ -> -1) (Just '-')

  >>> maybe 1 (\_ -> -1) Nothing

  >>> maybe False (\ch -> ch == '-') (Just '-')

  >>> maybe True (\ch -> ch == '-') Nothing

  The `fmap` bit then applies this function (returned by `maybe 1 (\_ -> -1)`) to the value wrapped by the next argument:

  >>> fmap (maybe 1 (\_ -> -1)) (Just (Just '-'))

  >>> fmap (maybe 1 (\_ -> -1)) (Just Nothing)

  This very same code could be rewritten as follows:

  @
  (maybe 1 (\_ -> -1)) <$> (Just Nothing)
  @

  Finally, the second part:

  @
  ((*) <$> sign) <*> number
  @

  The left part of it is just like the previously rewritten function from `fmap` to `<$>`, so this entire line can be written down as follows:

  @
  (fmap (*) sign) <*> number
  @

  What it does is applies the `(*)` function (multiplication) to the value wrapped in the second argument.
  In this case it is `sign`, which is a `Parser Integer`. This is the tricky bit: the type of this expression is __not__ `Parser Integer`.
  It is `Parser (a -> a)`, a parser of a function. This function, wrapped in a `Parser` type, can be applied to whatever other parser returns
  and hence chained together.
  The `<*>` operator, as mentioned before, chains the two parsers. So the entire expression applies the multiplication operation to the value
  returned by the `sign` parser and the value returned by the `number` parser.

  TL;DR: the whole thing _analyzes_ the first character of a string (without consuming it) and returns either `1` or `-1`; it then multiplies this value by the number
  returned by the `number` parser.

  >>> intNumber "-123"

  >>> intNumber "123"

  Using a `<|>` operator, one can parse integer (both negative and non-negative) numbers in this weird manner:

  @
  negativeNumber = (sat (== '-')) >> (* (-1)) <$> read <$> oneOrMore (sat isDigit)
  positiveNumber = read <$> oneOrMore (sat isDigit)
  number2 = negativeNumber <|> positiveNumber
  @

  >>> (parse number) "-42"

  >>> (parse number) "123"
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
