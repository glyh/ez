#include <stdint.h>
#include <stdio.h>

#include <gc.h>
#include <string.h>

#define UTF8_NO_STD_MALLOC
#include "utf8.h"

bool string_eq(const char *lhs, const char *rhs) {
  return utf8cmp(lhs, rhs) == 0;
}

char *string_add(const char *str1, const char *str2) {
  size_t len1 = strlen(str1);
  size_t len2 = strlen(str2);

  // (+1 for the null terminator)
  char *result = (char *)GC_MALLOC(len1 + len2 + 1);
  strcpy(result, str1);
  strcpy(result + len1, str2);

  return result;
}

void print_string(const char *str) { printf("%s", str); }
