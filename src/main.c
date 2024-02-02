/* The main Entry point for the interpreter. */
#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, char const *argv[]) {
    Chunk chunk;
    initChunk(&chunk);

    writeConstant(&chunk, 3.14, 123);
    writeConstant(&chunk, 2.718, 0xffff);
    writeChunk(&chunk, OP_RET, 0x10000);

    disassembleChunk(&chunk, "Test Chunk");
    freeChunk(&chunk);
    return 0;
}
