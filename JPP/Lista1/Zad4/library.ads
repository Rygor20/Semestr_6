package Library is
    type DiophantineResult is record
		x, y : Integer;
		valid: Boolean;
	end record;

    function Factorial(n : Natural) return Natural;
    pragma Export (C, Factorial, "factorial");
    function FactorialRecursion(n : Natural) return Natural;
    pragma Export (C, FactorialRecursion, "factorial_recursion");

    function GCD(a, b : Natural) return Natural;
    pragma Export (C, GCD, "gcd");
    function GCDRecursion(a, b : Natural) return Natural;
    pragma Export (C, GCDRecursion, "gcd_recursion");
    
    function Diophantine(a, b, c : Integer) return DiophantineResult;
    pragma Export (C, Diophantine, "diophantine");
    function DiophantineRecursion(a, b, c : Integer) return DiophantineResult;
    pragma Export (C, DiophantineRecursion, "diophantine_recursion");
end Library;
