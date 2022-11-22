# Matrix equation solver

## Summary

This is an implementation of a rather simple linear equation system solver. It uses Gauss method to solve the equation systems. It is written in 2016 as a coursework for "Functional Programming" course in Jagiellonian University.

The program does not have any third-party dependencies.

## Building

The program uses Cabal for (dependency management) building.
To build it simply run

	cabal build

## Running

It is as simple as

	cabal run

## Usage

Program runs as a CLI application, taking a number of strings as input. Each line is expected to have the following format:

	((<Integer> ([*]?) <Variable>) (+|-) (\1))+ = <Integer>

Where

	<Integer> :: (-)?(\d+)?
	<Variable> :: [a-zA-Z0-9_]

For instance:

	-4 x1 + 15x2 - 7*x_3 = 19

The end of the input is denoted by an empty line.

## How it works

Initially program parses the entire user input (read from `STDIN`) into matrix form.
Each input line is parsed using the following state machine:

![state machine graph](https://github.com/shybovycha/gauss-elimination/raw/master/input_parser_grammar.png)

Each line is represented as a tuple `(list of multipliers, list of variable names)`.

The next step is to iteratively reduce multipliers in each matrix row down to `0` (if there is more than one multiplier in a given row) or `1` (if there is just one multiplier left in a given row). Doing so transforms each equation into the `x_i = y_i` shape, giving the solution of a given equation system.

Last stage is forming the output using the list of variable names and the reduced equations.

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
