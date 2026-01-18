#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>

typedef struct {
    int64_t length;
    char data[];
} String;

String* string_from_cstr(const char* cstr);
String* string_concat(const String* a, const String* b);
int64_t string_eq(const String* a, const String* b);

void printInt(int64_t x);
void printString(const String* s);
void error(void);
int64_t readInt(void);
String* readString(void);

#endif
