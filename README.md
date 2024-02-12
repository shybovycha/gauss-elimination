# Matrix equation solver

## Summary

This is an implementation of a rather simple linear equation system solver. It uses Gauss method to solve the equation systems. It is written in 2016 as a coursework for "Functional Programming" course in Jagiellonian University.

Project does not have any third-party dependencies.

## Build

The application uses Cabal build system.
To build it run

    cabal build

## Run

Run application with

    cabal run

## Use

The application is a CLI tool, taking a number of lines as input. The end of input is marked by an empty line.
Each line is expected to have the following format:

    ((<Fraction> ([*]?) <Variable>) (+|-) (\1))+ = <Fraction>

Where

    <Fraction> :: (-)?(\d+)(\/(\d+))?
    <Variable> :: [a-zA-Z0-9_]

For instance:

    -3/4 x1 + 11/15x2 - 7*x_3 = 19

## How it works

First the input lines are parsed into a matrix form. Then the Gauss reduction method is applied to the matrix to obtain the solution.

### Parsing

There are two parsers defined in the code - `LegacyEquationParser` and `EquationParser`. The one used is the latter, the former is left for history.

#### Legacy parsing

The `LegacyEquationParser` module defines a state machine parser. It is not really a Haskell way of doing things, but it gets the job done.

The state transition diagram is the following:

![state machine graph](https://github.com/shybovycha/gauss-elimination/raw/master/input_parser_grammar.png)

Each line is represented as a tuple `(list of multipliers, list of variable names)`.

The next step is to iteratively reduce multipliers in each matrix row down to `0` (if there is more than one multiplier in a given row) or `1` (if there is just one multiplier left in a given row). Doing so transforms each equation into the `x_i = y_i` shape, giving the solution of a given equation system.

Last stage is forming the output using the list of variable names and the reduced equations.

#### Monadic parser

The parser used in the code as default is built from the ground up based on the lectures by [Dave Sands](https://www.youtube.com/watch?v=H7aYfGP76AI) from Chalmers University of Technology.

It defines the rules which can be combined together to form a grammar definition and a `parse` function which takes a `String` and a grammar as its inputs and eagerly executes all of the grammar rules,
returning the results of running the rules and the unparsed portion of a string.

The `Parser` is defined as a type

    newtype Parser a = P (String -> Maybe (a, String))

It simply wraps a rule (or a combination of rules, effectively an entire grammar),
which in turn is just a function, taking a `String` and returning a `Maybe` of a parsing result and an unparsed remainder of an input string.

The `parse` function simply executes the wrapped parser function:

    parse :: Parser a -> String -> Maybe (a, String)
    parse (P fn) str = fn str

The module also defines a number of functions to combine parsers:

A parser that always fails

    failure :: Parser a
    failure = P (\_ -> Nothing)

A parser that always succeeds (with a set result)

    success :: a -> Parser a
    success a = P (\str -> Just (a, str))

A parser that requires one or more characters (any characters)

    item :: Parser Char
    item = P $ \str -> case str of
        "" -> Nothing
        (ch:chs) -> Just (ch, chs)

A parser which takes a predicate (function from character to boolean)
and succeeds only if that predicate satisfies the first character of a string

    sat :: (Char -> Bool) -> Parser Char
    sat pred = item >>= (\str -> if pred str then success str else failure)

A parser which succeeds if it matches a string or not (optional parser)

    zeroOrMore :: Parser a -> Parser [a]
    zeroOrMore p = oneOrMore p <|> success []

A parser which requires at least one match

    oneOrMore :: Parser a -> Parser [a]
    oneOrMore p = p >>= (\a -> fmap (a:) (zeroOrMore p))

A parser which either matches exactly once or does not match at all

    zeroOrOne :: Parser a -> Parser (Maybe a)
    zeroOrOne p = (p >>= (\a -> success (Just a))) <|> success Nothing

A parser is a monad, so it defines the `bind` and `>>=` (which allows to chain parsers into "this and then that" manner):

    instance Monad Parser where
        p1 >>= p2 = P $ \str -> case parse p1 str of
            Just (a, str') -> parse (p2 a) str'
            Nothing -> Nothing

        return = pure

An alternate helper (combining parsers in a "if not this, try that" manner)

    instance Alternative Parser where
        empty = failure

        p1 <|> p2 = P $ \str -> case parse p1 str of
            Nothing -> parse p2 str
            result -> result

As an the example, the parser rule for an integer number is defined as following:

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

The parsing is then performed by running the parser combination (e.g. `integerNumber`) with `parse` function:

    parse integerNumber "-42"
    -- Just (-42, "")

### Non-solvable cases

The following systems of equations do not have solutions

#### Example 1

    x  + y  +  z = 5
    x  + 2y -  z = 6
    2x + 3y + 0z = 13

#### Example 2

    x + y = 10
    x + y = 20

#### Example 3

    x + y + z = 10
    x - y + z = 20
    2x + 0y + 2z = 50

The program will handle the above cases by returning the `Inconsistent` result.

### Infinite solutions cases

#### Example 1

    x  + y  +  z = 5
    x  + 2y -  z = 6
    2x + 3y + 0z = 11

For the above system, program will return the `Infinite [values]` result, where `[values]` is one of the possible solutions.

### Simple cases

In all other cases program will return `Simple [values]` result, denoting the only solution to the system.
