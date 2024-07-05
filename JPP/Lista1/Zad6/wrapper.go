package main

/*
#cgo CFLAGS: -Wall
#include "../Zad1/library.c"
*/
import "C"

type Diophantine struct {
	x, y  int64
	valid bool
}

func factorial(n uint64) uint64 {
	return uint64(C.factorial(C.uint64_t(n)))
}

func factorial_recursion(n uint64) uint64 {
	return uint64(C.factorial_recursion(C.uint64_t(n)))
}

func gcd(a, b uint64) uint64 {
	return uint64(C.gcd(C.uint64_t(a), C.uint64_t(b)))
}

func gcd_recursion(a, b uint64) uint64 {
	return uint64(C.gcd(C.uint64_t(a), C.uint64_t(b)))
}

func diophantine(a, b, c int64) Diophantine {
	result := C.diophantine(C.int64_t(a), C.int64_t(b), C.int64_t(c))
	return Diophantine{x: int64(result.x), y: int64(result.y), valid: bool(result.valid)}
}

func diophantine_recursion(a, b, c int64) Diophantine {
	result := C.diophantine_recursion(C.int64_t(a), C.int64_t(b), C.int64_t(c))
	return Diophantine{x: int64(result.x), y: int64(result.y), valid: bool(result.valid)}
}
