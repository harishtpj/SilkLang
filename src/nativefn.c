/* The implementation of built-in native functions
 * for the Silk Programming Language.
 * 
 * These functions are intended to be used directly, 
 * without importing any modules, just like python 
 */

#include "nativefn.h"

Value clockNative(int argCount, Value* args) {
    if (argCount != 0) {
        runtimeError("Expected 0 argument but got %d.", argCount);
        return ERR_VAL;
    }
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

Value chrNative(int argCount, Value* args) {
    if (argCount != 1) {
        runtimeError("Expected 1 argument but got %d.", argCount);
        return ERR_VAL;
    }
    if (!IS_NUMBER(args[0])) {
        runtimeError("Argument must be a number.");
        return ERR_VAL;
    }
    int value = AS_NUMBER(args[0]);
    if (value < 0 || value > 255) {
        runtimeError("Argument must be between 0 and 255.");
        return ERR_VAL;
    }
    char* chars = ALLOCATE(char, 2);
    chars[0] = (char)value;
    chars[1] = '\0';
    ObjStr* result = takeString(chars, 1);
    return OBJ_VAL(result);
}

Value ordNative(int argCount, Value* args) {
    if (argCount != 1) {
        runtimeError("Expected 1 argument but got %d.", argCount);
        return ERR_VAL;
    }
    if (!IS_STR(args[0])) {
        runtimeError("Argument must be a string.");
        return ERR_VAL;
    }
    ObjStr* str = AS_STR(args[0]);
    if (str->length != 1) {
        runtimeError("Argument must be a single character.");
        return ERR_VAL;
    }
    return NUMBER_VAL((double)str->chars[0]);
}

Value exitNative(int argCount, Value* args) {
    if (argCount > 1) {
        runtimeError("Expected 1 argument but got %d.", argCount);
        return ERR_VAL;
    }
    if (argCount != 0 && !IS_NUMBER(args[0])) {
        runtimeError("Argument must be a number.");
        return ERR_VAL;
    }
    int exitCode = argCount != 0 ? AS_NUMBER(args[0]) : 0;
    exit(exitCode);
}

Value inputNative(int argCount, Value* args) {
    if (argCount > 1) {
        runtimeError("Expected 1 argument but got %d.", argCount);
        return ERR_VAL;
    }

    if (argCount == 1 && !IS_STR(args[0])) {
        runtimeError("Argument must be a string.");
        return ERR_VAL;
    }

    if (argCount == 0)
        printf("");
    else
        printf("%s", AS_CSTR(args[0]));
    
    // Code for string input
    char *buffer = malloc(128 * sizeof(char));
    int capacity = 128;
    int n = 0;

    if (buffer == NULL) {
        runtimeError("Memory allocation failed.");
        return ERR_VAL;
    }

    while ((buffer[n] = getchar()) != '\n' && buffer[n] != EOF) {
        n++;

        if (n + 1 >= capacity) {
            capacity *= 2;
            char *temp = realloc(buffer, capacity * sizeof(char));
            if (temp == NULL) {
                free(buffer);
                return ERR_VAL;
            }
            buffer = temp;
        }
    }

    if (n == 0) {
        buffer[n] = '\0';
    } else {
        if (buffer[n] == '\n') {
            buffer[n] = '\0';
        } else {
            runtimeError("Unexpected EOF encountered.");
            return ERR_VAL;
        }
    }
    // End of code for string input

    ObjStr* result = takeString(buffer, n);
    return OBJ_VAL(result);
}

Value intNative(int argCount, Value* args) {
    if (argCount != 1 && argCount != 2) {
        runtimeError("Expected 2 arguments but got %d.", argCount);
        return ERR_VAL;
    }

    if (IS_STR(args[0])) {
        char *str = AS_CSTR(args[0]);
        char *endptr;
        // Check for base
        int base = argCount == 2 ? AS_NUMBER(args[1]) : 10;
        long int number = strtol(str, &endptr, base);

        if ((number == 0) && (str == endptr) && (errno != EINVAL)) {
            runtimeError("invalid literal for int() with base 10: %s", str);
            return ERR_VAL;
        } else if (errno == ERANGE) {
            runtimeError("Error: Input value out of integer range.");
            return ERR_VAL;
        } else {
            return NUMBER_VAL((double)number);
        }
    } else if (IS_NUMBER(args[0])) {
        if (argCount == 2) {
            runtimeError("int() can't convert non-string with explicit base");
            return ERR_VAL;
        }
        return NUMBER_VAL((int)AS_NUMBER(args[0]));
    } else {
        runtimeError("Argument must be a string or number.");
        return ERR_VAL;
    }
}

Value floatNative(int argCount, Value* args) {
    if (argCount != 1 ) {
        runtimeError("Expected 1 argument but got %d.", argCount);
        return ERR_VAL;
    }
    if (IS_STR(args[0])) {
        char *str = AS_CSTR(args[0]);
        char *endptr;
        double number = strtod(str, &endptr);
        if ((number == 0) && (str == endptr) && (errno != EINVAL)) {
            runtimeError("could not convert string to float:: %s", str);
            return ERR_VAL;
        } else if (errno == ERANGE) {
            runtimeError("Error: Input value out of float range.");
            return ERR_VAL;
        } else {
            return NUMBER_VAL(number);
        }
    } else if (IS_NUMBER(args[0])) {
        return NUMBER_VAL((double)AS_NUMBER(args[0]));
    } else {
        runtimeError("Argument must be a string or number.");
        return ERR_VAL;
    }
}

Value strNative(int argCount, Value* args) {
    if (argCount != 1) {
        runtimeError("Expected 1 argument but got %d.", argCount);
        return ERR_VAL;
    }
    char* strRep = valueToString(args[0]);
    return OBJ_VAL(takeString(strRep, strlen(strRep)));
}

Value typeNative(int argCount, Value* args) {
    if (argCount != 1) {
        runtimeError("Expected 1 argument but got %d.", argCount);
        return ERR_VAL;
    }
    char* strType = valueToType(args[0]);
    return OBJ_VAL(takeString(strType, strlen(strType)));
}

Value fmtNative(int argCount, Value* args) {
    char* format = AS_CSTR(args[0]);
    size_t list_len = argCount - 1;
    size_t result_len = strlen(format);
    char *result = malloc(result_len + 1);
    size_t result_i = 0;
    size_t list_i = 0;

    for (size_t i = 0; i < result_len; ++i) {
        if (format[i] == '#') {
            if (i + 1 < result_len && format[i + 1] == '#') {
                result[result_i++] = '#';
                i++; // Skip the next character (second '#')
            } else if (list_i < list_len) {
                char* strObj = valueToString(args[list_i + 1]);
                size_t str_len = strlen(strObj);
                result_len += str_len - 1;  // Adjust for replacing '#'
                result = realloc(result, result_len + 1);
                memcpy(result + result_i, strObj, str_len);
                result_i += str_len;
                list_i++;
            } else {
                result[result_i++] = format[i];
            }
        } else {
            result[result_i++] = format[i];
        }
    }

    result[result_i] = '\0';
    return OBJ_VAL(takeString(result, result_len));
}
