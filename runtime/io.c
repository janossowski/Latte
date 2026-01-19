#include "runtime.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>

void printInt(int64_t x) {
    printf("%" PRId64 "\n", x);
    fflush(stdout);
}

void printString(const String* s) {
    if (!s) {
        printf("(null)\n");
        fflush(stdout);
        return;
    }

    fwrite(s->data, 1, s->length, stdout);
    fputc('\n', stdout);
    fflush(stdout);
}

void error(void) {
    fprintf(stderr, "runtime error\n");
    exit(1);
}

int64_t readInt(void) {
    char* line = NULL;
    size_t cap = 0;

    ssize_t len = getline(&line, &cap, stdin);
    if (len < 0)
        error();

    errno = 0;
    char* endptr;
    int64_t value = strtoll(line, &endptr, 10);

    if (errno != 0 || endptr == line) {
        free(line);
        error();
    }

    free(line);
    return value;
}

String* readString(void) {
    char* buffer = NULL;
    size_t cap = 0;

    ssize_t len = getline(&buffer, &cap, stdin);
    if (len < 0)
        error();

    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
        len--;
    }

    if (len > 0 && buffer[len - 1] == '\r') {
        buffer[len - 1] = '\0';
        len--;
    }

    String* s = string_from_cstr(buffer);
    free(buffer);
    return s;
}
