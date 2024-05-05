#ifndef silk_object_h
#define silk_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE)
#define IS_STR(value)          isObjType(value, OBJ_STR)

#define AS_CLOSURE(value)      ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value)     ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)
#define AS_STR(value)       ((ObjStr*)AS_OBJ(value))
#define AS_CSTR(value)      (((ObjStr*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STR,
    OBJ_UPVALUE,
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next;
};

typedef struct {
  Obj obj;
  int arity;
  int upvalueCount;
  Chunk chunk;
  ObjStr* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

struct ObjStr {
    Obj obj;
    int length;
    char* chars;
    uint32_t hash;
};

typedef struct ObjUpvalue {
  Obj obj;
  Value* location;
  Value closed;
  struct ObjUpvalue* next;
} ObjUpvalue;

typedef struct {
  Obj obj;
  ObjFunction* function;
  ObjUpvalue** upvalues;
  int upvalueCount;
} ObjClosure;

ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjNative* newNative(NativeFn function);
ObjStr* takeString(char* chars, int length);
ObjStr* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);
void printObject(Value value);
char* objectToString(Value value);
char* objectToType(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif