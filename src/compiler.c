/* Implementation of the compiler for the language. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // :=
  PREC_CONDITIONAL, // <expr> if <cond> else <expr>
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // = !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * / %
  PREC_POW,         // ^
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  int depth;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;
    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
    int loopStart;
    int exitJump;
} Compiler;

Parser parser;
Compiler* current = NULL;
bool isREPLMode = false;

static Chunk* currentChunk() {
  return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    emitByte(OP_NULL);
    emitByte(OP_RET);
}

static void emitConstant(Value value) {
    writeConstant(currentChunk(), value, parser.previous.line);
}

static void addToInPlaceJumpOffsetList(int offset, int jumpAddress) {
	// Skip to end of list. Could be optimized away by directly storing the end of the list in the compiler struct:
	while (true) {
		int next = (currentChunk()->code[offset] << 8) | currentChunk()->code[offset + 1];
		if (next == UINT16_MAX)
			break;
		offset += next;
	}

	int jump = jumpAddress - offset;
	if (jump >= UINT16_MAX) {
		error("Too much code for offset.");
	}

	// Append new offset from previous jump to current jump to the list:
	currentChunk()->code[offset] = (jump >> 8) & 0xff;
	currentChunk()->code[offset + 1] = jump & 0xff;
}

static void patchJump(int offset) {
	// Walk the list and patch all the jumps:
	while (true) {
		int jump = currentChunk()->count - offset - 2;
		if (jump > UINT16_MAX) {
			error("Too much code to jump over.");
		}

		int next = (currentChunk()->code[offset] << 8) | currentChunk()->code[offset + 1];

		currentChunk()->code[offset] = (jump >> 8) & 0xff;
		currentChunk()->code[offset + 1] = jump & 0xff;

		if (next == UINT16_MAX)
			break;

		offset += next;
	}
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start,
                                            parser.previous.length);
    }

    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->name.start = "";
    local->name.length = 0;
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
    #ifdef DEBUG_PRINT_CODE
        if (!parser.hadError) {
            disassembleChunk(currentChunk(), function->name != NULL
        ? function->name->chars : "<TopLvlScript>");
        }
    #endif
    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth >
            current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    return (uint8_t)constant;
}

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index,
                      bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

static void declareVariable() {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
        break; 
        }

        if (identifiersEqual(name, &local->name)) {
        error("Already a variable with this name in this scope.");
        }
    }
    addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;
    
    return identifierConstant(&parser.previous);
}

static void markInitialized() {
    if (current->scopeDepth == 0) return;
    current->locals[current->localCount - 1].depth =
        current->scopeDepth;
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }
    emitBytes(OP_DEF_GLOBAL, global);
}

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    if (operatorType == TOKEN_CARET)
        parsePrecedence((Precedence)(rule->precedence));
    else
        parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL: emitByte(OP_EQUAL); break;
        case TOKEN_GREATER: emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS: emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL: emitBytes(OP_GREATER, OP_NOT); break;

        case TOKEN_PLUS:    emitByte(OP_ADD); break;
        case TOKEN_MINUS:   emitByte(OP_SUB); break;
        case TOKEN_STAR:    emitByte(OP_MUL); break;
        case TOKEN_SLASH:   emitByte(OP_DIV); break;
        case TOKEN_PERCENT: emitByte(OP_MOD); break;
        case TOKEN_CARET:   emitByte(OP_POW); break;
        default: return;
    }
}

static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = identifierConstant(&parser.previous);

  if (canAssign && match(TOKEN_COLON_EQUAL)) {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NULL: emitByte(OP_NULL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return;
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                    parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    
    if (canAssign && match(TOKEN_COLON_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_PLUS_EQUAL)) {
        emitBytes(getOp, (uint8_t)arg);
        expression();
        emitByte(OP_ADD);
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_MINUS_EQUAL)) {
        emitBytes(getOp, (uint8_t)arg);
        expression();
        emitByte(OP_SUB);
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_STAR_EQUAL)) {
        emitBytes(getOp, (uint8_t)arg);
        expression();
        emitByte(OP_MUL);
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_SLASH_EQUAL)) {
        emitBytes(getOp, (uint8_t)arg);
        expression();
        emitByte(OP_DIV);
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_PERCENT_EQUAL)) {
        emitBytes(getOp, (uint8_t)arg);
        expression();
        emitByte(OP_MOD);
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_CARET_EQUAL)) {
        emitBytes(getOp, (uint8_t)arg);
        expression();
        emitByte(OP_POW);
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_PLUS_PLUS)) {
        emitBytes(getOp, (uint8_t)arg);
        emitConstant(NUMBER_VAL(1));
        emitByte(OP_ADD);
        emitBytes(setOp, (uint8_t)arg);
    } else if (canAssign && match(TOKEN_MINUS_MINUS)) {
        emitBytes(getOp, (uint8_t)arg);
        emitConstant(NUMBER_VAL(1));
        emitByte(OP_SUB);
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return;
    }
}

static void ifExpr(bool canAssign) {
    parsePrecedence(PREC_CONDITIONAL);
    //emitByte(OP_SWAP);
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);
    emitByte(OP_POP);

    consume(TOKEN_ELSE, "Expect 'else' after 'if' expresssion.");

    parsePrecedence(PREC_ASSIGNMENT);
    patchJump(endJump);

}

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CARET]         = {NULL,     binary, PREC_POW},
  [TOKEN_PERCENT]       = {NULL,     binary, PREC_FACTOR},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_COLON]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COLON_EQUAL]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_MINUS]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_BREAK]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CONTINUE]      = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     ifExpr, PREC_CONDITIONAL},
  [TOKEN_IMPORT]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NULL]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SELF]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_LET]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LOOP]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_STD]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_COLON_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope(); 

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void classDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect class name.");
  uint8_t nameConstant = identifierConstant(&parser.previous);
  declareVariable();

  emitBytes(OP_CLASS, nameConstant);
  defineVariable(nameConstant);

  consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_COLON_EQUAL)) {
      expression();
    } else {
      emitByte(OP_NULL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

static void expressionStatement() {
    expression();
    if (isREPLMode) {
        emitConstant(OBJ_VAL(takeString(strdup(":= "), 3)));
        emitByte(OP_PRINT);
        emitBytes(OP_PRINT, OP_PRINT_NL);
    } else {
        consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
        emitByte(OP_POP);
    }
}

static void ifStatement() {
    expression();

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    consume(TOKEN_LEFT_BRACE, "Expect '{' after if clause.");
    beginScope();
    block();
    endScope();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);
    
    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

static void printStatement() {
    bool printNewline = true;
    do {
        if (check(TOKEN_SEMICOLON)) {
            printNewline = false;
            break;
        }
        expression();
        emitByte(OP_PRINT);
        emitConstant(OBJ_VAL(takeString(strdup(" "), 1)));
        emitByte(OP_PRINT);
    } while (match(TOKEN_COMMA));
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    if (printNewline) emitByte(OP_PRINT_NL);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RET);
    }
}

static void breakStatement() {
    consume(TOKEN_SEMICOLON, "Expect ';' after break.");
	int breakAddress = emitJump(OP_JUMP);
	addToInPlaceJumpOffsetList(current->exitJump, breakAddress);
}

static void continueStatement() {
    consume(TOKEN_SEMICOLON, "Expect ';' after continue.");
	emitLoop(current->loopStart);
}

static void loopStatement(bool isWhile) {
    int loopStartBackup = current->loopStart;
	current->loopStart = currentChunk()->count;

    if (isWhile) expression(); else emitByte(OP_TRUE);

    int exitJumpBackup = current->exitJump;
    current->exitJump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP);

    statement();

    emitLoop(current->loopStart);

	patchJump(current->exitJump);
	emitByte(OP_POP);

	// Back to the enclosing loop:
	current->loopStart = loopStartBackup;
	current->exitJump = exitJumpBackup;
}

static void forStatement() {
    beginScope();
    if (match(TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(TOKEN_LET)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStartBackup = current->loopStart;
	current->loopStart = currentChunk()->count;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        int exitJumpBackup = current->exitJump;
        current->exitJump = emitJump(OP_JUMP_IF_FALSE);
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);

        emitLoop(current->loopStart);
        current->loopStart = incrementStart;
        patchJump(bodyJump);
    }

    consume(TOKEN_LEFT_BRACE, "Expect '{' after loop statement.");
    block();

    emitLoop(current->loopStart);

    if (current->exitJump != -1) {
        patchJump(current->exitJump);
        emitByte(OP_POP); // Condition.
    }

    endScope();
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
          case TOKEN_CLASS:
          case TOKEN_FUN:
          case TOKEN_LET:
          case TOKEN_FOR:
          case TOKEN_IMPORT:
          case TOKEN_IF:
          case TOKEN_WHILE:
          case TOKEN_LOOP:
          case TOKEN_PRINT:
          case TOKEN_RETURN:
          case TOKEN_BREAK:
          case TOKEN_CONTINUE:
            return;

          default:
            ; // Do nothing.
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_LET)) {
        varDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_LOOP)) {
        loopStatement(false);
    } else if (match(TOKEN_WHILE)) {
        loopStatement(true);
    } else if (match(TOKEN_FOR)) {
        forStatement();
    }  else if (match(TOKEN_BREAK)) {
        breakStatement();
    } else if (match(TOKEN_CONTINUE)) {
        continueStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    }  else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

ObjFunction* compile(const char* source, bool isREPL) {
    isREPLMode = isREPL;
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
