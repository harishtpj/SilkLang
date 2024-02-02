#ifndef silk_chunk_h
#define silk_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONST,
    OP_LCONST, // 24-bit constant implementation
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
void writeConstant(Chunk* chunk, Value value, int line);

#endif