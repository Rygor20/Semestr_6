#ifndef WRAPPER_H
#define WRAPPER_H

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct {
	int32_t x;
	int32_t y;
	bool valid;
} Diophantine;

extern uint32_t factorial(uint32_t n);
extern uint32_t gcd(uint32_t a, uint32_t b);
extern Diophantine diophantine(int32_t a, int32_t b, int32_t c);

extern uint32_t factorial_recursion(uint32_t n);
extern uint32_t gcd_recursion(uint32_t a, uint32_t b);
extern Diophantine diophantine_recursion(int32_t a, int32_t b, int32_t c);

#endif // WRAPPER_H