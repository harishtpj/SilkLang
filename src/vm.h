#ifndef silk_vm_h
#define silk_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 128
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
  ObjClosure* closure;
  uint8_t* ip;
  Value* slots;
} CallFrame;

typedef struct {
    Chunk* chunk;
    uint8_t* ip;
    CallFrame frames[FRAMES_MAX];
    int frameCount;
    Value stack[STACK_MAX];
    Value* stackTop;
    Table globals;
    Table strings;
    ObjUpvalue* openUpvalues;
    Obj* objects;
} HVM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern HVM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source, bool isREPL);
void push(Value value);
Value pop();

#endif