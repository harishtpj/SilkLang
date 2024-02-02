/* The main Entry point for the interpreter. */
#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, char const *argv[]) {
    initVM();

    Chunk chunk;
    initChunk(&chunk);

    writeConstant(&chunk, 3.14, 123);
    writeConstant(&chunk, 2, 123);
    writeChunk(&chunk, OP_DIV, 123);
    writeConstant(&chunk, 10, 123);
    writeConstant(&chunk, 2, 123);
    writeChunk(&chunk, OP_POW, 123);
    writeChunk(&chunk, OP_MUL, 123);

    writeConstant(&chunk, 5, 123);
    writeChunk(&chunk, OP_ADD, 123);
    writeConstant(&chunk, 10, 123);
    writeChunk(&chunk, OP_MOD, 123);
    writeChunk(&chunk, OP_NEGATE, 123);
    writeChunk(&chunk, OP_RET, 123);

    interpret(&chunk);
    
    freeVM();
    freeChunk(&chunk);
    return 0;
}
