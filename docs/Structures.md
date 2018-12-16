# Struktury
Wszystkie obiekty oparte o klasy są przekazywane przez referencję. Referencja niesie ze sobą informację o typie obiektu oraz licznik referencji, kiedy licznik spadnie do 0 następuje wywołanie 
    
    ref->type->destructor(ref->data);
    free(ref);

Tak wygląda struktura referencji:

    struct Reference {
        Type* type;
        void* data;
        int counter;
    }

## Tablice
W przypadku tablic, pole `type` w referencji wskazuje na ogólny typ `Array`, z którego metod można korzystać. Pole `data` wskazuje na jedną z poniższych struktur;

    struct RefArray {
        char arrM = 0x01;
        char[] padding = {0,0,0};
        int size;
        Reference* elements;
    }

    struct IntArray {
        char arrM = 0x02;
        char[] padding = {0,0,0};
        int size;
        int* elements;
    }

    struct ByteArray {
        char arrM = 0x03;
        char[] padding = {0,0,0};
        int size;
        char* elements;
    }

Tablice dla typów `int`, `boolean` i `char` korzystają z uproszczonych struktur, które nie wymagają "boxowania" tych prostych typów.

## Klasy
Klasa opisuje strukturę fragmentu pamięci obiektu oraz funkcje, które można na nim wywołać.

    struct Type {
        parent Type*;
        int dataSize;
        char[] padding = {0,0,0,0};
        void* methods;
    }

Na etapie kompilacji będziemy chcieli uzyskać wywołania tego typu:

    Reference r;
    r.type = //constant
    r.data = malloc(r.type->dataSize);
    r.counter++;
    (r.data + 0x04)* = 20;        // field assignment
    r.type->methods[0x03](r, 2);  // method invokation

Kiedy dziedziczymy po pewnej klasie to nasze pola są dopisywane na końcu, podobnie nowe metody. Jeśli jest metoda o tej samej nazwie co w nadklasie (i sygnatura się zgadza) to nią zastąpimy tę metodę z nadklasy. Jak sygnatura się nie zgadza to błąd, bo Latte nie ma overloadingu metod.

## Typy prymitywne

    int x;   // int
    char b;  // boolean / byte
    
    class String extends Object {
        int[] characters;
        int charAt(int index);
        int length();
        // like in Java
    }

    class Object {
        boolean equals(Object other);
        int getHashCode();
        string toString();
    }

    class Array extends Object {
        readonly int length {offset = 0x04};
        hidden void* array; // a[i] -> if (a.array->size > i && i >= 0) a.array->elements[i] else error()
    }

Mamy taką strukturę dziedziczenia ogólnie:

    int boolean byte  Object
                     /    |  \
                String Array  ...

W języku Latte metody/funkcje nie będą wartościami.

