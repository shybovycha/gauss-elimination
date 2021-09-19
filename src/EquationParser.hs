module EquationParser
(
  digit, 
  naturalNumber, 
  negativeInteger, 
  integerNumber,
  rationalNumber, 
  rationalIntegerNumber,
  rationalFactor
)
where

import Data.Char (isDigit)
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
