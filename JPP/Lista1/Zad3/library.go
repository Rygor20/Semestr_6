package main

type Diophantine struct {
	x, y  int64
	valid bool
}

func factorial(n uint64) uint64 {
	var result uint64 = 1

	for i := uint64(1); i <= n; i++ {
		result *= i
	}

	return result
}

func factorial_recursion(n uint64) uint64 {
	if n == 0 || n == 1 {
		return 1
	}

	return n * factorial_recursion(n-1)
}

func gcd(a, b uint64) uint64 {
	var temp uint64
	for b != 0 {
		temp = b
		b = a % b
		a = temp
	}
	return a
}

func gcd_recursion(a, b uint64) uint64 {
	if b == 0 {
		return a
	}
	return gcd_recursion(b, a%b)
}

func diophantine(a, b, c int64) Diophantine {
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

	if c%int64(a) != 0 {
		return Diophantine{0, 0, false}
	} else {
		x := x1 * int(c/a)
		y := y1 * int(c/a)
		return Diophantine{int64(x), int64(y), true}
	}
}

func diophantine_recursion(a, b, c int64) Diophantine {
	if a == 0 && b == 0 && c != 0 {
		return Diophantine{valid: false}
	} else if a == 0 {
		return Diophantine{0, int64(c) / int64(b), true}
	} else if b == 0 {
		return Diophantine{int64(c) / int64(a), 0, true}
	} else {
		result := diophantine_recursion(b, a%b, c)
		if !result.valid {
			return Diophantine{valid: false}
		}
		x := result.y
		y := result.x - int64(a/b)*result.y
		return Diophantine{x, y, true}
	}
}
