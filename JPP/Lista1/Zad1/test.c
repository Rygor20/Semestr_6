#include "library.h"
#include <stdio.h>

int main()
{
    printf("Loops:\n");
	printf("Factorial of 5: %lu\n", factorial(5));
	printf("\tCorrect: 120\n");

	printf("GCD of 15 and 25: %lu\n", gcd(15, 25));
	printf("\tCorrect: 5\n");

	Diophantine solution = diophantine(3, 7, 22);
	printf("Diophantine solution for 3x + 7y = 22: x = %ld, y = %ld\n", solution.x, solution.y);
	printf("\tCorrect: x = 5, y = 1\n");
	printf("\tOther correct: x = -44, y = 22\n");
    

    printf("\n\nRecursion:\n");
	printf("Factorial of 5: %lu\n", factorial_recursion(5));
	printf("\tCorrect: 120\n");

	printf("GCD of 15 and 25: %lu\n", gcd_recursion(15, 25));
	printf("\tCorrect: 5\n");

	solution = diophantine_recursion(3, 7, 22);
	printf("Diophantine solution for 3x + 7y = 22: x = %ld, y = %ld\n", solution.x, solution.y);
	printf("\tCorrect: x = 5, y = 1\n");
	printf("\tOther correct: x = -44, y = 22\n");
	return 0;
}