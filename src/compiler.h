#ifndef silk_compiler_h
#define silk_compiler_h

#include "object.h"
#include "vm.h"

ObjFunction* compile(const char* source, bool isREPL);
void markCompilerRoots();

#endif