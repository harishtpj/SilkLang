/* Provides a disassembler for the Bytecode chunks */
#include <stdio.h>

#include "debug.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);

    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

static int constantInstruction(const char* name, Chunk* chunk, 
                               int offset) {
    uint8_t constant = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constant);
    printValue(chunk->consts.values[constant]);
    printf("'\n");
    return offset + 2;
}

static int lConstInstruction(const char* name, Chunk* chunk, 
                             int offset) {
    uint32_t constant = chunk->code[offset + 1] |
                       (chunk->code[offset + 2] << 8) |
                       (chunk->code[offset + 3] << 16);
    printf("%-16s %4d '", name, constant);
    printValue(chunk->consts.values[constant]);
    printf("'\n");
    return offset + 4;
}

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);
    if (offset > 0 && 
        chunk->lines[offset] == chunk->lines[offset - 1]) {
        printf("   | ");
    } else {
        printf("%4d ", chunk->lines[offset]);
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_CONST:
            return constantInstruction("OP_CONST", chunk, offset);
        
        case OP_LCONST:
            return lConstInstruction("OP_LCONST", chunk, offset);
        
        case OP_NULL:
            return simpleInstruction("OP_NULL", offset);
        
        case OP_TRUE:
            return simpleInstruction("OP_TRUE", offset);
        
        case OP_FALSE:
            return simpleInstruction("OP_FALSE", offset);
        
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        
        case OP_SUB:
            return simpleInstruction("OP_SUB", offset);
        
        case OP_MUL:
            return simpleInstruction("OP_MUL", offset);
        
        case OP_DIV:
            return simpleInstruction("OP_DIV", offset);
        
        case OP_NOT:
            return simpleInstruction("OP_NOT", offset);
        
        case OP_EQUAL:
            return simpleInstruction("OP_EQUAL", offset);
        
        case OP_GREATER:
            return simpleInstruction("OP_GREATER", offset);
        
        case OP_LESS:
            return simpleInstruction("OP_LESS", offset);
        
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        
        case OP_POW:
            return simpleInstruction("OP_POW", offset);
        
        case OP_POP:
            return simpleInstruction("OP_POP", offset);
        
        case OP_MOD:
            return simpleInstruction("OP_MOD", offset);
        
        case OP_PRINT:
            return simpleInstruction("OP_PRINT", offset);
        
        case OP_PRINT_NL:
            return simpleInstruction("OP_PRINT_NL", offset);
        
        case OP_DEF_GLOBAL:
            return constantInstruction("OP_DEF_GLOBAL", chunk, offset);
        
        case OP_GET_GLOBAL:
            return constantInstruction("OP_GET_GLOBAL", chunk, offset);
        
        case OP_SET_GLOBAL:
            return constantInstruction("OP_SET_GLOBAL", chunk, offset);

        case OP_RET:
            return simpleInstruction("OP_RET", offset);
        
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}