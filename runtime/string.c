#include "runtime.h"
#include <stdlib.h>
#include <string.h>

String* string_from_cstr(const char* cstr) {
    int64_t len = strlen(cstr);
    String* s = (String*) malloc(sizeof(int64_t) + len + 1);
    if (!s) abort();
    s->length = len;
    memcpy(s->data, cstr, len + 1);
    return s;
}

String* string_concat(const String* a, const String* b) {
    int64_t len = a->length + b->length;
    String* s = (String*) malloc(sizeof(int64_t) + len + 1);
    if (!s) abort();
    s->length = len;
    memcpy(s->data, a->data, a->length);
    memcpy(s->data + a->length, b->data, b->length + 1);
    return s;
}

int64_t string_eq(const String* a, const String* b) {
    if (a->length != b->length) return 0;
    return memcmp(a->data, b->data, a->length) == 0;
}
