package main

// go run test.go wrapper.go

import (
	"fmt"
)

func main() {
	fmt.Println("Loops:")
	fmt.Printf("Factorial of 5: %d\n", factorial(5))
	fmt.Printf("\tCorrect: 120\n")

	fmt.Printf("GCD of 15 and 25: %d\n", gcd(15, 25))
	fmt.Printf("\tCorrect: 5\n")

	solution := diophantine(3, 7, 22)
	fmt.Printf("Diophantine solution for 3x + 7y = 22: x = %d, y = %d\n", solution.x, solution.y)
	fmt.Printf("\tCorrect: x = 5, y = 1\n")
	fmt.Printf("\tOther correct: x = -44, y = 22\n")

	fmt.Println("\n\nRecursion:")
	fmt.Printf("Factorial of 5: %d\n", factorial_recursion(5))
	fmt.Printf("\tCorrect: 120\n")

	fmt.Printf("GCD of 15 and 25: %d\n", gcd_recursion(15, 25))
	fmt.Printf("\tCorrect: 5\n")

	solution = diophantine_recursion(3, 7, 22)
	fmt.Printf("Diophantine solution for 3x + 7y = 22: x = %d, y = %d\n", solution.x, solution.y)
	fmt.Printf("\tCorrect: x = 5, y = 1\n")
	fmt.Printf("\tOther correct: x = -44, y = 22\n")
}
