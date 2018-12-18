#include <stdio.h>
#include "runtime.h"

void *object_methods[] = {(void *)_Object_toString, (void *)_Object_getHashCode,
                          (void *)_Object_equals};
struct Type _class_Object = {0, object_methods};

void *string_methods[] = {(void *)_String_toString, (void *)_String_getHashCode,
                          (void *)_String_equals};
struct Type _class_String = {0, string_methods};

void *array_methods[] = {(void *)_Array_toString, (void *)_Object_getHashCode,
                         (void *)_Object_equals};
struct Type _class_Array = {16, array_methods};

int main() {
    obj o = __new(&_class_Object);
    __incRef(o);
    obj iarr = __newIntArray(5);
    __incRef(iarr);
    for (int i = 1; i <= 5; i++) {
        int32_t *els = ((struct Array *)iarr->data)->elements;
        els[i] = i;
    }
    obj s = __createString("Hello World!");
    __incRef(s);
    obj subs = _String_substring(s, 2, 3);
    __incRef(subs);
    obj esubs = __createString("llo");
    __incRef(esubs);
    if(_String_equals(subs, esubs)) {
        printf("Ok: ");
        print(subs);
    } else {
        printf("Bad: ");
        print(subs);
    }
    obj es = _String_concat(s, subs);
    __incRef(es);
    obj hundred = intToString(100);
    __incRef(hundred);
    print(o);
    print(iarr);
    print(s);
    print(es);
    print(hundred);
    __decRef(o);
    __decRef(iarr);
    __decRef(s);
    __decRef(hundred);
    __decRef(subs);
    __decRef(esubs);
    __decRef(es);

    return 0;
}