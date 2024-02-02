/* The Virtual Machine for executing the Bytecode */
#include <stdio.h>
#include <math.h>
#include "common.h"
#include "debug.h"
#include "vm.h"

HVM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
}

void freeVM() {

}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONST() (vm.chunk->consts.values[READ_BYTE()])
#define BINARY_OP(op) \
    do { \
        double b = pop(); \
        double a = pop(); \
        push(a op b); \
    } while (false)
#define BINARY_FUN(fn) \
    do { \
        double b = pop(); \
        double a = pop(); \
        push(fn(a, b)); \
    } while (false)

    while (true) {
        #ifdef DEBUG_TRACE_EXEC
            printf("          ");
            for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
                printf("[");
                printValue(*slot);
                printf("]");
            }
            printf("\n");
            disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
        #endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
        case OP_CONST: {
            Value constant = READ_CONST();
            push(constant);
            break;
        }

        case OP_LCONST: {
            uint8_t l1 = READ_BYTE(), l2 = READ_BYTE(),  l3 = READ_BYTE();
            uint32_t index = l1 | (l2 << 8) | (l3 << 16);
            push(vm.chunk->consts.values[index]);
            break;
        }

        case OP_ADD:      BINARY_OP(+);     break;
        case OP_SUB:      BINARY_OP(-);     break;
        case OP_MUL:      BINARY_OP(*);     break;
        case OP_DIV:      BINARY_OP(/);     break;
        case OP_POW:      BINARY_FUN(pow);  break;
        case OP_MOD:      BINARY_FUN(fmod); break;
        case OP_NEGATE:   push(-pop());     break;

        case OP_RET: {
            printValue(pop());
            printf("\n");
            return INTERPRET_OK;
        }
        
        default:
            break;
        }
    }

#undef READ_BYTE
#undef READ_CONST
#undef BINARY_OP
#undef BINARY_FUN
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    vm.ip = vm.chunk->code;
    return run();
}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}
