{--|
  Monadic parser and helper functions.
--}

module Parsing (
  Parser,
  parse,
  sat, 
  item,
  failure, success,
  oneOrMore, zeroOrMore, zeroOrOne,
  (<|>),
  (<*>),
  (<$>),
  return
)
where

import Data.Char
import Data.Maybe
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (liftM, ap)

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P f) s = f s

failure :: Parser a
failure = P $ \_ -> Nothing

success :: a -> Parser a
success a = P $ \s -> Just (a, s)

item :: Parser Char
item = P $ \s -> case s of
  "" -> Nothing
  (ch:chs) -> Just (ch, chs)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \s -> if p s then success s else failure

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> success []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = p >>= \a -> fmap (a:) (zeroOrMore p)

zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne p = (p >>= \s -> success (Just s)) <|> success Nothing

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = success
  (<*>) = ap

instance Monad Parser where
  p >>= f = P $ \s -> case parse p s of
    Just (a, s') -> parse (f a) s'
    Nothing -> Nothing

  return = pure

instance Alternative Parser where
  empty = failure
  p1 <|> p2 = P $ \s -> case parse p1 s of
    Nothing -> parse p2 s
    result -> result
