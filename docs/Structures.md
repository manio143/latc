# Struktury
Wszystkie obiekty oparte o klasy są przekazywane przez referencję. Referencja niesie ze sobą informację o typie obiektu oraz licznik referencji, kiedy licznik spadnie do 0 następuje wywołanie 
    
    ref->type->destructor(ref->data);
    free(ref);

Tak wygląda struktura referencji:

    struct Reference {
        Type* type;
        void* data;
        int32_t counter;
    }

## Tablice
W przypadku tablic, pole `type` w referencji wskazuje na ogólny typ `Array`, z którego metod można korzystać. Pole `data` wskazuje na poniższą strukturę;

    struct Array {
        int32_t dataSize;
        int32_t length;
        void* elements;
    }

Tablice dla typów `int`, `boolean` i `char` korzystają z tablicy intów/bajtów, a tablice referencji mają tablicę wskaźników.

## Klasy
Klasa opisuje strukturę fragmentu pamięci obiektu oraz funkcje, które można na nim wywołać.

    struct Type {
        int dataSize;
        void* methods;
    }

Na etapie kompilacji będziemy chcieli uzyskać wywołania tego typu:

    Reference r;
    r.type = //constant
    r.data = malloc(r.type->dataSize);
    r.counter++;
    (r.data* + 0x04)* = 20;        // field assignment
    ((void**)r.type->methods)[0x03](r, 2);  // method invokation

Kiedy dziedziczymy po pewnej klasie to nasze pola są dopisywane na końcu, podobnie nowe metody. Jeśli jest metoda o tej samej nazwie co w nadklasie (i sygnatura się zgadza) to nią zastąpimy tę metodę z nadklasy. Jak sygnatura się nie zgadza to błąd, bo Latte nie ma overloadingu metod.

## Typy prymitywne

    int32_t x;   // int
    int8_t b;  // boolean / byte
    
    struct String {
        in32_t length;  //unicode characters
        uint8_t *data;  //unistring
    }

Mamy taką strukturę dziedziczenia ogólnie:

    int boolean byte  Object
                     /    |  \
                String Array  ...

W języku Latte metody/funkcje nie będą wartościami.

