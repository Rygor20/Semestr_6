#ifndef LIBRARY_H
#define LIBRARY_H

#include <stdbool.h>
#include <stdint.h>

typedef struct {
	int64_t x;
	int64_t y;
	bool valid;
} Diophantine;

uint64_t factorial(uint64_t n);
uint64_t gcd(uint64_t a, uint64_t b);
Diophantine diophantine(int64_t a, int64_t b, int64_t c);

uint64_t factorial_recursion(uint64_t n);
uint64_t gcd_recursion(uint64_t a, uint64_t b);
Diophantine diophantine_recursion(int64_t a, int64_t b, int64_t c);

#endif