/* An Implementation of Bytecode format for the language. */
#include <stdlib.h>

#include "chunk.h"
#include "memory.h"

void initChunk(Chunk* chunk) {
    chunk->capacity = 0;
    chunk->count = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->consts);
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->consts);
    initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, 
            oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines,
            oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->consts, value);
    return chunk->consts.count - 1;
}

// A More effective implementation of addConstant fn
void writeConstant(Chunk* chunk, Value value, int line) {
    int index = addConstant(chunk, value);
    if (index < 256) {
        writeChunk(chunk, OP_CONST, line);
        writeChunk(chunk, (uint8_t)index, line);
    } else {
        writeChunk(chunk, OP_LCONST, line);
        writeChunk(chunk, (uint32_t)(index & 0xff), line);
        writeChunk(chunk, (uint32_t)((index >> 8) & 0xff), line);
        writeChunk(chunk, (uint32_t)((index >> 16) & 0xff), line);
    }
    
}
