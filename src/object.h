#ifndef silk_object_h
#define silk_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_STR(value)       isObjType(value, OBJ_STR)

#define AS_STR(value)       ((ObjStr*)AS_OBJ(value))
#define AS_CSTR(value)      (((ObjStr*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_STR,
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next;
};

struct ObjStr {
    Obj obj;
    int length;
    char* chars;
    uint32_t hash;
};

ObjStr* takeString(char* chars, int length);
ObjStr* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif