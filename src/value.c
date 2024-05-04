/* Implementation of constant pool for the language. */
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, 
                oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

char* valueToString(Value value) {
    char* result = NULL;
    switch (value.type) {
        case VAL_BOOL:
            result = AS_BOOL(value) ? strdup("true") : strdup("false");
            break;
        case VAL_NULL:
            result = strdup("null");
            break;
        case VAL_NUMBER: {
            // Convert the number to a string and allocate memory for it
            char* numberString = ALLOCATE(char, 128);
            sprintf(numberString, "%g", AS_NUMBER(value));
            result = numberString;
            break;
        }
        case VAL_OBJ: {
            result = objectToString(value);
            break;
        }
    }
    return result;
}

char* valueToType(Value value) {
    char* result = NULL;
    switch (value.type) {
        case VAL_BOOL:
            result = strdup("bool");
            break;
        case VAL_NULL:
            result = strdup("null");
            break;
        case VAL_NUMBER: {
            double num = AS_NUMBER(value);
            if (num == (double)(int)num) {
                result = strdup("int");
            } else {
                result = strdup("float");
            }
            break;
        }
        case VAL_OBJ: {
            result = objectToType(value);
            break;
        }
    }
    return result;
}

void printValue(Value value) {
    switch (value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NULL: printf("null"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
        case VAL_OBJ: printObject(value); break;
    }
}

bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) return false;
    switch (a.type) {
        case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NULL: return true;
        case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ: return AS_OBJ(a) == AS_OBJ(b);
        default: return false;
    }
}
