module EquationParser
(
  digit, 
  naturalNumber, 
  negativeInteger, 
  integerNumber,
  rationalNumber, 
  rationalIntegerNumber,
  rationalFactor,
  equationFactor,
  equationMember,
  equation
)
where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Fraction
import Parsing

digit :: Parser Char
digit = sat isDigit

naturalNumber :: Parser Integer
naturalNumber = read <$> (oneOrMore digit)

negativeInteger :: Parser Integer
negativeInteger = do
  (sat (== '-'))
  n <- naturalNumber 
  return (-1 * n)

integerNumber :: Parser Integer
integerNumber = naturalNumber <|> negativeInteger

rationalNumber :: Parser Fraction
rationalNumber = do
  nominator <- integerNumber
  sat (== '/')
  denominator <- naturalNumber
  return (nominator % denominator)

rationalIntegerNumber :: Parser Fraction
rationalIntegerNumber = fmap (% 1) integerNumber

rationalFactor :: Parser Fraction
rationalFactor = rationalNumber <|> rationalIntegerNumber

equationFactor :: Parser Fraction
equationFactor = ((sat (== '+')) >> rationalFactor) <|> rationalFactor

equationMember :: Parser (Fraction, String)
equationMember = do
    factor <- equationFactor

    zeroOrMore (sat isSpace)
    zeroOrOne (sat (== '*'))
    zeroOrMore (sat isSpace)
    
    nameFirst <- oneOrMore (sat isAlpha)
    nameRest <- zeroOrMore (sat isAlphaNum)

    zeroOrMore (sat isSpace)

    return (factor, nameFirst ++ nameRest)

-- An equation consists of a list of pairs (factor, variable name) and a free member]
equation :: Parser ([(Fraction, String)], Fraction)
equation = do
    members <- oneOrMore equationMember
    
    zeroOrMore (sat isSpace)
    sat (== '=')
    zeroOrMore (sat isSpace)

    freeMember <- rationalFactor

    return (members, freeMember)
