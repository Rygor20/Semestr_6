#include "library.h"

uint64_t factorial(uint64_t n)
{
	uint64_t result = 1;
	for (uint64_t i = 1; i <= n; i++)
		result *= i;
	return result;
}

uint64_t factorial_recursion(uint64_t n)
{
	if (n == 0 || n == 1)
		return 1;
	else
		return n * factorial_recursion(n - 1);
}

uint64_t gcd(uint64_t a, uint64_t b)
{
	while (b != 0)
	{
		uint64_t temp = b;
		b = a % b;
		a = temp;
	}
	return a;
}

uint64_t gcd_recursion(uint64_t a, uint64_t b)
{
	if (b == 0)
		return a;
	else
		return gcd_recursion(b, a % b);
}

DiophantineResult diophantine(int64_t a, int64_t b, int64_t c)
{
	int x1 = 1;
    int y1 = 0;
    int x2 = 0;
    int y2 = 1;

    while (b != 0) {
        int q = a / b;
        int r = a % b;

        int temp = x1 - q * x2;
        x1 = x2;
        x2 = temp;

        temp = y1 - q * y2;
        y1 = y2;
        y2 = temp;

        a = b;
        b = r;
    }

    if (c % a != 0) {
        return (DiophantineResult){0, 0, false};
    } else {
        int x = x1 * (c / a);
        int y = y1 * (c / a);
        return (DiophantineResult){x, y, true};
    }
}

DiophantineResult diophantine_recursion(int64_t a, int64_t b, int64_t c)
{
	if (a == 0 && b == 0 && c != 0) {
        return (DiophantineResult){.valid = false};
    } else if (a == 0) {
        return (DiophantineResult){0, c / b, true};
    } else if (b == 0) {
        return (DiophantineResult){c / a, 0, true};
    } else {
        DiophantineResult result = diophantine_recursion(b, a % b, c);
        if (!result.valid) {
            return (DiophantineResult){.valid = false};
        }
		int x = result.y;
        int y = result.x - (a / b) * result.y;
        return (DiophantineResult){x, y, true};
    }
}