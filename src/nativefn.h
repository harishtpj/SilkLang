#ifndef define silk_nativefn_h
#define define silk_nativefn_h

#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#include "memory.h"
#include "object.h"

// Forward declaration of functions from VM.c
void runtimeError(const char* format, ...);

Value clockNative(int argCount, Value* args);
Value chrNative(int argCount, Value* args);
Value ordNative(int argCount, Value* args);
Value exitNative(int argCount, Value* args);
Value inputNative(int argCount, Value* args);

// Simple type conversions
Value intNative(int argCount, Value* args);

#endif