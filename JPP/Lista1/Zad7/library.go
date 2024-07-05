package main

/*
	#include <stdbool.h>
	#include <stdint.h>

	struct Diophantine {
		int64_t x;
		int64_t y;
		bool valid;
	};
*/
import "C"

//export factorial
func factorial(n C.uint64_t) C.uint64_t {
	var result C.uint64_t = 1

	for i := C.uint64_t(1); i <= n; i++ {
		result *= i
	}

	return result
}

//export factorial_recursion
func factorial_recursion(n C.uint64_t) C.uint64_t {
	if n == 0 || n == 1 {
		return 1
	}

	return n * factorial_recursion(n-1)
}

//export gcd
func gcd(a, b C.uint64_t) C.uint64_t {
	var temp C.uint64_t
	for b != 0 {
		temp = b
		b = a % b
		a = temp
	}
	return a
}

//export gcd_recursion
func gcd_recursion(a, b C.uint64_t) C.uint64_t {
	if b == 0 {
		return a
	}
	return gcd_recursion(b, a%b)
}

//export diophantine
func diophantine(a, b, c C.int64_t) C.struct_Diophantine {
	var results C.struct_Diophantine = C.struct_Diophantine{
		x:     0,
		y:     0,
		valid: false,
	}

	x1 := 1
	y1 := 0
	x2 := 0
	y2 := 1

	for b != 0 {
		q := a / b
		r := a % b

		temp := x1 - int(q)*x2
		x1 = x2
		x2 = temp

		temp = y1 - int(q)*y2
		y1 = y2
		y2 = temp

		a = b
		b = r
	}

	if c%C.int64_t(a) != 0 {
		// return C.struct_Diophantine{0, 0, false}
		return results
	} else {
		x := x1 * int(c/a)
		y := y1 * int(c/a)

		results.x = C.int64_t(x)
		results.y = C.int64_t(y)
		results.valid = true

		// return C.struct_Diophantine{C.int64_t(x), C.int64_t(y), true}
		return results
	}
}

//export diophantine_recursion
func diophantine_recursion(a, b, c C.int64_t) C.struct_Diophantine {
	var results C.struct_Diophantine = C.struct_Diophantine{
		x:     0,
		y:     0,
		valid: false,
	}

	if a == 0 && b == 0 && c != 0 {
		// return C.struct_Diophantine{valid: false}
		return results
	} else if a == 0 {
		results.y = C.int64_t(c) / C.int64_t(b)
		results.valid = true
		// return C.struct_Diophantine{0, C.int64_t(c) / C.int64_t(b), true}
		return results
	} else if b == 0 {
		results.x = C.int64_t(c) / C.int64_t(a)
		results.valid = true
		// return C.struct_Diophantine{C.int64_t(c) / C.int64_t(a), 0, true}
		return results
	} else {
		result := diophantine_recursion(b, a%b, c)
		if !result.valid {
			return C.struct_Diophantine{valid: false}
		}
		x := result.y
		y := result.x - C.int64_t(a/b)*result.y

		results.x = x
		results.y = y
		results.valid = true

		// return C.struct_Diophantine{x, y, true}
		return results
	}
}

func main() {}
