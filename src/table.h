#ifndef silk_table_h
#define silk_table_h

#include "common.h"
#include "value.h"

typedef struct {
    ObjStr* key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);
bool tableGet(Table* table, ObjStr* key, Value* value);
bool tableSet(Table* table, ObjStr* key, Value value);
bool tableDelete(Table* table, ObjStr* key);
void tableAddAll(Table* from, Table* to);
ObjStr* tableFindString(Table* table, const char* chars,
                           int length, uint32_t hash);

#endif