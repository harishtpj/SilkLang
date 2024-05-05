/* Handles memory allocation for dynamic data types */
#include <stdlib.h>

#include "memory.h"
#include "vm.h"

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}

static void freeObject(Obj* object) {
    switch (object->type) {
        case OBJ_STR: {
            ObjStr* str = (ObjStr*)object;
            FREE_ARRAY(char, str->chars, str->length + 1);
            FREE(ObjStr, object);
            break;
        }

        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }

        case OBJ_NATIVE:
            FREE(ObjNative, object);
            break;
        
        case OBJ_CLOSURE: {
            FREE(ObjClosure, object);
            break;
        }

        case OBJ_UPVALUE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues,
                                    closure->upvalueCount);
            FREE(ObjUpvalue, object);
            break;
        }
    }
}

void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
}
