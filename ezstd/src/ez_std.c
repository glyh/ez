#include <gc.h>
#include <gc/gc.h>
#include <stdint.h>
#include <stdio.h>

void print_int(int64_t num) { printf("%ld", num); }

void print_endline() { putchar('\n'); }

void ezstd_init() { GC_INIT(); }
