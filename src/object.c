/* A implementation of general Object datatype */

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

static ObjStr* allocateString(char* chars, int length, uint32_t hash) {
    ObjStr* str = ALLOCATE_OBJ(ObjStr, OBJ_STR);
    str->length = length;
    str->chars = chars;
    str->hash = hash;
    tableSet(&vm.strings, str, NULL_VAL);
    return str;
}

uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjStr* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjStr* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }
    return allocateString(chars, length, hash);
}

ObjStr* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjStr* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned;

    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

static void printFunction(ObjFunction* function) {
    if (function->name == NULL) {
        printf("<TopLvlScript>");
        return;
    }
    printf("<fun %s>", function->name->chars);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STR:
            printf("%s", AS_CSTR(value));
            break;
        
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;
        
        case OBJ_NATIVE:
            printf("<Native fun>");
            break;
    }
}

char* objectToString(Value value) {
    char* result = NULL;
    switch (OBJ_TYPE(value)) {
        case OBJ_STR: {
            // Allocate memory for the string representation of the string object
            result = strdup(AS_CSTR(value));
            break;
        }

        case OBJ_FUNCTION: {
            // Get the function name and create a string representation
            ObjFunction* function = AS_FUNCTION(value);
            if (function->name == NULL) {
                result = strdup("<TopLvlScript>");
            } else {
                // Allocate memory for the function name string
                char* functionName = ALLOCATE(char, (strlen(function->name->chars) + 6)); // 6 for "<fun >"
                sprintf(functionName, "<fun %s>", function->name->chars);
                result = functionName;
            }
            break;
        }

        case OBJ_NATIVE:
            result = strdup("<Native fun>");
            break;
    }

    return result;
}

char* objectToType(Value value) {
    char* result = NULL;
    switch (OBJ_TYPE(value)) {
        case OBJ_STR: {
            result = strdup("str");
            break;
        }

        case OBJ_FUNCTION: {
            result = strdup("function");
            break;
        }

        case OBJ_NATIVE:
            result = strdup("nativefn");
            break;
    }

    return result;
}
