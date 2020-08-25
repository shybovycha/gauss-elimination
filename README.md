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

Program runs as a CLI application, taking a number of strings as input. Each line should be in format

	((<Integer ([*]?) <Variable>) (+|-) (\1))+ = <Integer>

Where

	<Integer> :: (-)?(\d+)?
	<Variable> :: [a-zA-Z0-9_]

For instance:

	-4 x1 + 15x2 - 7*x_3 = 19

The end of the input is denoted by an empty line.

## How it works

**Beware: my Polish skills were terrible back in the day**

Napierw program przekształczy wszystko co użytkownik wprowadził do postaci macierzy. Konwersja polega na końcowym automacie:

![state machine graph](https://raw.github.com/shybovycha/uklady-rownian/master/grammatic_for_matrix_row.png)

Wynikiem tej konwersji jest para `(koeficienci, nazwy zmiennych)`. Mapowanie funkcji konwersji na każdy ciąg wejścia produkuje macierze koeficienci oraz liste nazw zmiennych.

Po konwersji, program wyłowa funkcje, ktora rozwiązuje układ metodą Gausa. Wynik wywołania tej funkcji jest macierz, w której są koeficienci przy każdej zmiennej.

Na koniec program zamienia wszystkie koeficienci w tej macierzy na koeficient oraz nazwę odpowiedniej zmiennej. Wynikiem tego jest ciąg, który jest wyświetlony na monitorze.

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
