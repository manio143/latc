# Kompilator Latte

## Kompilacja i uruchomienie
Potrzebna jest biblioteka `mtl` w wersji `2.2.2`, która jest na students w wersji `2.1.2`. Więc jednorazowo należy zrobić

    cabal update && cabal install mtl

A następnie

    make
    ./latc_frontend file.lat

## Frontend
Na chwilę obecną mam zrobione:

- parser
- redeclaration checker
- type checker
- return checker

Przy czym sprawdzanie redeklaracji zawiera się w module `TypeChecker`.

Robione przeze mnie rozszerzenia to

- tablice
- klasy z dziedziczeniem
- metody wirtualne

Dodatkowo

- słowo kluczowe `var` służące do inferencji typu zmiennej podczas jej deklaracji z wartością
- typ bazowy `Object`, po którym dziedziczą wszystkie typy poza primitywnymi
- typy primitywne to `int`, `bool` i `byte`

### Krok po kroku
Wygenerowałem parser na podstawie gramatyki BNFC (plik `src/parser/Latte.cf`). Po sparsowaniu pliku, zostaje on poddany procesowi odsładzania (desugaring) w wyniku którego otrzymuję moją własną strukturę AST.

Następnie na tej strukturze przeprowadzane jest sprawdzanie typów i redeklaracji klas/funkcji. Jego wynikiem jest zaktualizowane AST (m.in. odwołania do pól i metod dostają explicite `this.`, wszelkie występowania `var` są zastępowane właściwym typem).

Kolejnym krokiem jest przejście się po funkcjach i metodach oraz sprawdzenie czy dla funkcji typu innego niż `void` jest zwracana jakaś wartość na każdej ścieżce egzekucji (w dość prosty sposób, bez uproszczania wyrażeń w warunkach).

No i na tę chwilę na koniec wypisywany jest sprawdzony kod na standardowe wyjście.
