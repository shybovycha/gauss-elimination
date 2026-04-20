module EquationParser where

import Data.Maybe
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Fraction
import Parsing

digit :: Parser Char
digit = sat isDigit

naturalNumber :: Parser Integer
naturalNumber = read <$> (oneOrMore digit)

negativeInteger :: Parser Integer
negativeInteger = do
  _ <- sat (== '-')
  n <- naturalNumber
  return (-1 * n)

integerNumber :: Parser Integer
integerNumber = naturalNumber <|> negativeInteger

rationalNumber :: Parser Fraction
rationalNumber = do
  nom <- integerNumber
  _ <- sat (== '/')
  denom <- naturalNumber
  return (nom % denom)

rationalIntegerNumber :: Parser Fraction
rationalIntegerNumber = fmap (% 1) integerNumber

rationalFactor :: Parser Fraction
rationalFactor = rationalNumber <|> rationalIntegerNumber

positiveFactorPrefix :: Parser Fraction
positiveFactorPrefix = fmap (\_ -> 1%1) (sat (== '+'))

negativeFactorPrefix :: Parser Fraction
negativeFactorPrefix = fmap (\_ -> -1%1) (sat (== '-'))

factorSign :: Parser Fraction
factorSign = positiveFactorPrefix <|> negativeFactorPrefix <|> (success (1%1))

equationFactor :: Parser Fraction
equationFactor = do
    _sign <- factorSign

    _ <- zeroOrMore (sat isSpace)

    factor <- fmap (fromMaybe (1%1)) (zeroOrOne rationalFactor)

    return (_sign * factor)

equationMember :: Parser (Fraction, String)
equationMember = do
    factor <- equationFactor

    _ <- zeroOrMore (sat isSpace)
    _ <- zeroOrOne (sat (== '*'))
    _ <- zeroOrMore (sat isSpace)

    nameFirst <- oneOrMore (sat isAlpha)
    nameRest <- zeroOrMore (sat isAlphaNum)

    _ <- zeroOrMore (sat isSpace)

    return (factor, nameFirst ++ nameRest)

-- An equation consists of a list of pairs (factor, variable name) and a free member
equation :: Parser ([(Fraction, String)], Fraction)
equation = do
    members <- oneOrMore equationMember

    _ <- zeroOrMore (sat isSpace)
    _ <- sat (== '=')
    _ <- zeroOrMore (sat isSpace)

    freeMember <- rationalFactor

    return (members, freeMember)

parseEquationSystem :: [String] -> Maybe [([(Fraction, String)], Fraction)]
parseEquationSystem ls = (map fst) <$> mapM (parse equation) ls
