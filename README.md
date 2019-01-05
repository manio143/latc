# Kompilator Latte

## Kompilacja i uruchomienie
Potrzebna jest biblioteka `mtl` w wersji `2.2.2`, która jest na students w wersji `2.1.2`. Więc jednorazowo należy zrobić

    cabal update && cabal install mtl

A następnie

    make
    ./latc_x86_64 file.lat

Kompilator pozwala na połączenie wielu plików źródłowych, ustawienie nazwy programu wynikowego (`-o out`) oraz wypisanie poszczególnych kroków kompilacji (`-p`).

## Zakres
Kompiluję to x86_64.

Robione przeze mnie rozszerzenia to

- tablice
- klasy z dziedziczeniem
- metody wirtualne
- odśmiecanie

Dodatkowo

- słowo kluczowe `var` służące do inferencji typu zmiennej podczas jej deklaracji z wartością
- typ bazowy `Object`, po którym dziedziczą wszystkie typy poza primitywnymi
- typy primitywne to `int`, `bool` i `byte`

## Frontend

- parser
- desugaring
- redeclaration checker
- type checker
- constant folder
- return checker
- scope renamer

Przy czym sprawdzanie redeklaracji zawiera się w module `TypeChecker`.

### Krok po kroku
Wygenerowałem parser na podstawie gramatyki BNFC (plik `src/parser/Latte.cf`). Po sparsowaniu pliku, zostaje on poddany procesowi odsładzania (desugaring) w wyniku którego otrzymuję moją własną strukturę AST.

Następnie na tej strukturze przeprowadzane jest sprawdzanie typów i redeklaracji klas/funkcji. Jego wynikiem jest zaktualizowane AST (m.in. odwołania do pól i metod dostają explicite `this.`, wszelkie występowania `var` są zastępowane właściwym typem).

Następnie dochodzi do propagacji stałych oraz składanie stałych wyrażeń (w zakresie danego typu).

Kolejnym krokiem jest przejście się po funkcjach i metodach oraz sprawdzenie czy dla funkcji typu innego niż `void` jest zwracana jakaś wartość na każdej ścieżce egzekucji (w dość prosty sposób, bez uproszczania wyrażeń w warunkach).

Ostatnim elementem po stronie drzewa AST jest zmiana nazw wszystkich zmiennych dodając im informację o zagnieżdżeniu.

## Backend
Kiedy przerobiłem już drzewo AST, to konwertuję je do liniowej reprezentacji (LIR), gdzie wyrażenia rozbijane są na pojedyncze przypisania, a pętle, warunki i porównania są zamieniane na skoki i etykiety.

Ponieważ translacja jest bardzo nieoptymalna, to następnym krokiem jest propagacja wartości, która wycina niepotrzebne wyrażenia.

Następnie eliminuję identyczne wyrażenia w ramach bloku.

Używam systemu liczenia referencji do zarządzania pamięcią obiektów. Na podstawie analizy żywotności dodaję wywołania funkcji `__incRef` i `__decRef`.

Następnie alokuję rejestry i miejsce na stosie - w sposób liniowy, zapisując żywe zmienne na stosie na koniec bloku.

Mając te informacje emituję assembler x86_64 do postaci rozumianej przez NASM.

Tuż przed kompilacją czyszczę nieco wyemitowany kod, usuwając niepotrzebne komendy lub upraszczając sekwencje.

Następnie odbywa się asemblowanie do pliku `.o`, który jest następnie linkowany z przygotowanym runtimem (`src/native_runtime`).

Skompilowany plik można normalnie wywołać.

## Używane biblioteki
W kompilatorze używam mtl oraz to co wygenerował BNFC.

W runtime używam libc oraz libunistring.

## Standardowa biblioteka Latte
Do dyspozycji oddaję następujące funkcje:

- void printString(string)
- void printInt(int)
- void printBoolean(boolean)
- void print(Object)  - wywołuje `.toString()` i woła printString
- string boolToString(boolean)
- string intToString(int)
- void error()
- int readInt()
- string readString()

Oraz następujące klasy

    class Object {
        boolean equals(Object other);
        int getHashCode();
        string toString();
    }

    class Array extends Object {
        int length;
        string toString();
    }

    class String extends Object {
        int charAt(int pos);
        boolean equals(Object other);
        string concat(string other);
        boolean startsWith(string substr);
        boolean endsWith(string substr);
        byte[] getBytes();
        int indexOf(string substr, int startIndex);
        int length();
        string substring(int index, int length);
        string toString();
        int getHashCode();
    }
