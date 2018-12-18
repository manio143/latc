
#include <inttypes.h>

struct Type {
    int32_t dataSize;
    void *methods;
};

struct Reference {
    struct Type *type;
    void *data;
    int32_t counter;
};

struct Array {
    int32_t elementSize;
    int32_t length;
    void *elements;
};

struct String {
    int32_t length;
    uint8_t *data;
};

typedef struct Reference *obj;

obj __new(struct Type *t);
void __free(obj r);

void __incRef(obj r);
void __decRef(obj r);

obj __newRefArray();
obj __newIntArray();
obj __newByteArray();
obj __newArray(int32_t size, int32_t length);

obj __createString(char *c);

// BuiltIn classes' methods
obj _Object_toString(obj o);
int32_t _Object_getHashCode(obj o);
int8_t _Object_equals(obj o1, obj o2);

obj _Array_toString(obj arr);

obj _String_toString(obj str);
int32_t _String_getHashCode(obj str);
int8_t _String_equals(obj o1, obj o2);
obj _String_substring(obj str, int32_t startIndex, int32_t length);
int32_t _String_length(obj str);
int32_t _String_indexOf(obj str, obj substr, int32_t startFrom);
obj _String_getBytes(obj str);
int8_t _String_endsWith(obj str, obj substr);
int8_t _String_startsWith(obj str, obj substr);
obj _String_concat(obj str, obj secondstr);
int32_t _String_charAt(obj str, int32_t index);

// functions
int8_t printString(obj str);
int8_t printInt(int32_t i);
int8_t printBoolean(int8_t b);
obj intToString(int32_t i);
int8_t print(obj o);
int8_t error();
int32_t readInt();
obj readString();