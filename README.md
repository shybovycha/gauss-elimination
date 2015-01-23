# Układy Równiań

## Opis

Ten program służy do rozwiązania układów równiań liniowych. Napisany jest w Haskelu. Nie używa innych modułów niż `Prelude`, `IO` oraz `Environment`.

## Wywołanie

Kompilacje oraz uruchamienie programu wykonuje `cabal`.

Żeby zbudować program, dość wywołać

	cabal build

Tak samo, żeby uruchomić program, dość użyć

	cabal run

## Warunki działania

Poprawne ciągi, które program rozumie - to ciągi w postaci

	((<Liczba> ([*]?) <Zmienna>) (+|-) (\1))+ = <Liczba>

Gdzie

	<Liczba> :: (-)?(\d+)?
	<Zmienna> :: [a-zA-Z0-9_]

N.p.:

	-4 x1 + 15x2 - 7*x_3 = 19

Wprowadzanie danych zakonczy kiedu użytkownik wpisuje pusty ciąg (`CLCR`, <key>Enter</key>).

**Uwaga:** w każdym ciągu muszą być wszystki zmienne, które pojawią w układzie i w jednym porządku. To znaczy, że nie wolno opuszcić żadnej zmiennej w żadnym wierszu. Żeby opuszcić zmienne, trzeba użyć koeficientu `0`. Porządek zmiennych nie wolno zmienić w żadnym z równiań.

N.p.:

**Nie wolno:**

	-3x1 + 2x2 - x3 = 15
	x1 + x3 = -8
	<CLCR>

**Powinno być:**

	-3x1 + 2x2 - x3 = 15
	x1 + 0x2 + x3 = -8
	<CLCR>

**Nie wolno:**

	2x1 + y = 10
	y + x = 5
	<CLCR>

**Powinno być:**

	2x1 + y = 10
	x + y = 5
	<CLCR>

## Działanie

Napierw program przekształczy wszystko co użytkownik wprowadził do postaci macierzy. Konwersja polega na końcowym automacie:

![state machine graph](https://raw.github.com/shybovycha/uklady-rownian/master/grammatic_for_matrix_row.png)

Wynikiem tej konwersji jest para `(koeficienci, nazwy zmiennych)`. Mapowanie funkcji konwersji na każdy ciąg wejścia produkuje macierze koeficienci oraz liste nazw zmiennych.

Po konwersji, program wyłowa funkcje, ktora rozwiązuje układ metodą Gausa. Wynik wywołania tej funkcji jest macierz, w której są koeficienci przy każdej zmiennej.

Na koniec program zamienia wszystkie koeficienci w tej macierzy na koeficient oraz nazwę odpowiedniej zmiennej. Wynikiem tego jest ciąg, który jest wyświetlony na monitorze.

## Sprzeczne układy

Np.:

	x  + y  +  z = 5
	x  + 2y -  z = 6
	2x + 3y + 0z = 13

albo nawęt prostszej:

	x + y = 10
	x + y = 20

albo złożonej:

	x + y + z = 10
	x - y + z = 20
	2x + 0y + 2z = 50

Tu widzimy, że równiania są sprzeczne. Program obsługuje te sytuację.

## Układ z nieskonczoną ilościu rozwiązań

Np.:

	x  + y  +  z = 5
	x  + 2y -  z = 6
	2x + 3y + 0z = 11

Tu sytuacja jest taka, że nigdy nie znajdziemy żadnego rozwiązania dla tego układu. Te sytuację program zarównież obsługuje.

## Wypisać dowolne rozwiązanie

Ten system ma nieskonczoność rozwiązań:

	x  + y  +  z = 5
	x  + 2y -  z = 6
	2x + 3y + 0z = 11

Rozwiązaniem jest

	x = 4 - 3 * z
	y = 1 + 2 * z

*Remove all rows, consisting of zeroes only, and try to solve the resulting matrix*