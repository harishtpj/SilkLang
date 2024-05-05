#ifndef silk_chunk_h
#define silk_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONST,
    OP_LCONST, // 24-bit constant implementation
    OP_NULL,
    OP_TRUE,
    OP_FALSE,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_NOT,
    OP_MOD,
    OP_POW,
    OP_NEGATE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_PRINT,
    OP_PRINT_NL,
    OP_POP,
    OP_GET_GLOBAL,
    OP_DEF_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_CLOSE_UPVALUE,
    OP_RET,
} OpCode;

typedef struct {
    int capacity;
    int count;
    uint8_t* code;
    int* lines;
    ValueArray consts;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);
uint8_t writeConstant(Chunk* chunk, Value value, int line);

#endif