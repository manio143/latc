#include <stdio.h>
#include <stdlib.h>
#include <unistdio.h>
#include <unistr.h>

#include "runtime.h"

extern void bzero(void *s, size_t n);

extern struct Type _class_Array;
extern struct Type _class_Object;
extern struct Type _class_String;

typedef obj (*toStringPtr)(obj);

char *errMsg;

obj __new(struct Type *t) {
    obj r = malloc(sizeof(struct Reference));
    r->type = t;
    r->counter = 0;
    if (t->dataSize > 0) {
        r->data = malloc(t->dataSize);
        bzero(r->data, t->dataSize);
    } else {
        r->data = NULL;
    }
    return r;
}

void __free(obj r) {
    if (r->type == &_class_Array) {
        void *els = ((struct Array *)r->data)->elements;
        if (els != NULL)
            free(els);
    } else if (r->type == &_class_String) {
        void *els = ((struct String *)r->data)->data;
        if (els != NULL)
            free(els);
    }
    if (r->data != NULL)
        free(r->data);
    free(r);
}

void __incRef(obj r) { r->counter++; }
void __decRef(obj r) {
    r->counter--;
    if (r->counter <= 0)
        __free(r);
}

obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
obj __newIntArray(int32_t length) {
    return __newArray(sizeof(int32_t), length);
}
obj __newByteArray(int32_t length) {
    return __newArray(sizeof(int8_t), length);
}
obj __newArray(int32_t size, int32_t length) {
    obj r = __new(&_class_Array);
    struct Array *arr = malloc(sizeof(struct Array));
    r->data = arr;
    arr->elementSize = size;
    arr->length = length;
    if (length > 0) {
        arr->elements = malloc(size * length);
        bzero(arr->elements, size * length);
    } else
        arr->elements = NULL;
    return r;
}

obj __createString(char *c) {
    obj r = __new(&_class_String);
    struct String *str = malloc(sizeof(struct String));
    r->data = str;
    str->length = u8_strlen(c);
    if (u8_check(c, str->length) != NULL) {
        errMsg = "ERROR: Non-unicode string encoding.";
        error();
    }
    if (str->length > 0) {
        str->data = u8_cpy_alloc(c, str->length);
    } else
        str->data = NULL;
    str->length = 0;
    uint8_t *walker = str->data;
    while (walker != NULL) {
        str->length++;
        ucs4_t c;
        if (u8_next(&c, walker) != NULL)
            walker += u8_next(&c, walker) - walker;
        else
            walker = NULL;
    }
    return r;
}

// BuiltIn classes' methods
obj _Object_toString(obj o) { return __createString("Object"); }
int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }

obj _Array_toString(obj arr) {
    char start[] = "[";
    char delim[] = ", ";
    char end[] = "]";
    struct Array *array = arr->data;

    obj *strings = malloc(sizeof(obj) * array->length);
    int32_t *lenghts = malloc(sizeof(int32_t) * array->length);
    int32_t totalLenght = 0;

    for (int i = 0; i < array->length; i++) {
        if (array->elementSize == sizeof(int32_t)) {
            int32_t *elements = array->elements;
            strings[i] = intToString(elements[i]);
        } else if (array->elementSize == sizeof(int8_t)) {
            int8_t *elements = array->elements;
            strings[i] = intToString(elements[i]);
        } else {
            obj *elements = array->elements;
            obj element = elements[i];
            obj (*toString)(obj) = ((void **)element->type->methods)[0];
            strings[i] = toString(element);
        }
        __incRef(strings[i]);
        lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
        totalLenght += lenghts[i];
    }

    int32_t bufferSize = u8_strlen(start) + totalLenght +
                         (array->length - 1) * u8_strlen(delim) +
                         u8_strlen(end) + 1;
    uint8_t *buffer = malloc(bufferSize);
    int32_t index = 0;
    u8_strcpy(buffer + index, start);
    index++;
    for (int i = 0; i < array->length; i++) {
        u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
        index += lenghts[i];
        if (i != array->length - 1) {
            u8_strcpy(buffer + index, delim);
            index += 2;
        }
        __decRef(strings[i]);
    }
    u8_strcpy(buffer + index, end);
    buffer[bufferSize - 1] = 0;
    obj ret = __createString(buffer);
    free(lenghts);
    free(strings);
    free(buffer);
    return ret;
}

obj _String_toString(obj str) { return str; }
int32_t _String_getHashCode(obj str) {
    int32_t hash = 0x811c9dc5;
    uint8_t *rawstring = ((struct String *)str->data)->data;
    int32_t strlen = u8_strlen(rawstring);
    for (int i = 0; i < strlen; i++) {
        hash ^= rawstring[i];
        hash *= 0x01000193;
    }
    return hash;
}
int8_t _String_equals(obj o1, obj o2) {
    if (o2->type != &_class_String)
        return false;
    if (((struct String *)o1->data)->length !=
        ((struct String *)o2->data)->length)
        return false;
    uint8_t *rs1 = ((struct String *)o1->data)->data;
    uint8_t *rs2 = ((struct String *)o2->data)->data;
    return u8_strcmp(rs1, rs2) == 0;
}
obj _String_substring(obj str, int32_t startIndex, int32_t length) {
    if (length < 0) {
        errMsg = "ERROR: Substring with negative length.";
        error();
    }
    if (length == 0)
        return __createString("");
    if (startIndex >= _String_length(str)) {
        errMsg = "ERROR: Substring starting index is too big.";
        error();
    }
    uint8_t *rs = ((struct String *)str->data)->data;
    uint8_t *offset_str = rs;
    ucs4_t character;
    while (startIndex-- > 0)
        offset_str += u8_next(&character, offset_str) - offset_str;
    uint8_t *end = offset_str;
    int32_t counter = 0;
    while (counter < length) {
        if (u8_next(&character, end) == NULL) {
            errMsg = "ERROR: Substring reached end of string.";
            error();
        }
        end += u8_next(&character, end) - end;
        counter++;
    }
    int32_t bufferSize = end - offset_str + 1;
    uint8_t *buffer = malloc(bufferSize);
    u8_strncpy(buffer, offset_str, bufferSize);
    buffer[bufferSize - 1] = 0;
    obj ret = __createString(buffer);
    free(buffer);
    return ret;
}
int32_t _String_length(obj str) { return ((struct String *)str->data)->length; }
int32_t _String_indexOf(obj str, obj substr, int32_t startFrom) {
    if (startFrom >= _String_length(str)) {
        errMsg = "ERROR: IndexOf starting index is too big.";
        error();
    }
    uint8_t *rs = ((struct String *)str->data)->data;
    uint8_t *rsub = ((struct String *)substr->data)->data;
    uint8_t *start = rs;
    ucs4_t c;
    while (startFrom-- > 0) {
        if (u8_next(&c, start) == NULL)
            return -1;
        start += u8_next(&c, start) - start;
    }
    uint8_t *index = u8_strstr(start, rsub);
    uint32_t counter = 0;
    while ((rs += u8_next(&c, rs) - rs) != index)
        counter++;
    return counter;
}
obj _String_getBytes(obj str) {
    uint8_t *rs = ((struct String *)str->data)->data;
    int32_t len = u8_strlen(rs);
    obj arr = __newByteArray(len + 1);
    u8_strcpy(((struct Array *)str->data)->elements, rs);
    return arr;
}
int8_t _String_endsWith(obj str, obj substr) {
    uint8_t *rs = ((struct String *)str->data)->data;
    uint8_t *rsub = ((struct String *)substr->data)->data;
    return u8_endswith(rs, rsub);
}
int8_t _String_startsWith(obj str, obj substr) {
    uint8_t *rs = ((struct String *)str->data)->data;
    uint8_t *rsub = ((struct String *)substr->data)->data;
    return u8_startswith(rs, rsub);
}
obj _String_concat(obj str, obj secondstr) {
    uint8_t *rs1 = ((struct String *)str->data)->data;
    uint8_t *rs2 = ((struct String *)secondstr->data)->data;
    int32_t len1 = u8_strlen(rs1);
    int32_t len2 = u8_strlen(rs2);
    uint8_t *buffer = malloc(len1 + len2 + 1);
    u8_strcpy(buffer, rs1);
    u8_strcpy(buffer + len1, rs2);
    buffer[len1 + len2] = 0;
    obj ret = __createString(buffer);
    free(buffer);
    return ret;
}

char charAtErr[] = "ERROR: String too short.";
int32_t _String_charAt(obj str, int32_t index) {
    uint8_t *rs = ((struct String *)str->data)->data;
    ucs4_t c;
    while (index-- > 0) {
        if (u8_next(&c, rs) == NULL) {
            errMsg = charAtErr;
            error();
        }
        rs += u8_next(&c, rs) - rs;
    }
    if (u8_strmbtouc(&c, rs) <= 0) {
        errMsg = charAtErr;
        error();
    }
    return c;
}

// functions
int8_t printString(obj str) {
    uint8_t *rs = ((struct String *)str->data)->data;
    printf("%s\n", rs);
    return 0;
}
int8_t printInt(int32_t i) {
    printf("%d\n", i);
    return 0;
}
int8_t printBoolean(int8_t b) {
    if (b)
        printf("true\n");
    else
        printf("false\n");
    return 0;
}
obj intToString(int32_t i) {
    char buffer[11];
    sprintf(buffer, "%d", i);
    return __createString(buffer);
}

int8_t print(obj o) {
    obj (*toStr)(obj) = ((void **)o->type->methods)[0];
    obj str = toStr(o);
    __incRef(str);
    printString(str);
    __decRef(str);
}

int8_t error() {
    if (errMsg != NULL)
        fprintf(stderr, "%s\n", errMsg);
    else
        fprintf(stderr, "%s\n", "ERROR: User error.");
    exit(1);
    return 1;
}

int32_t readInt() {
    int32_t i;
    scanf("%d", &i);
    return i;
}
obj readString() {
    char *line;
    size_t size;
    getline(&line, &size, stdin);
    return __createString(line);
}
