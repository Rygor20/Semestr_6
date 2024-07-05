package Library is
    type DiophantineResult is record
		x, y : Integer;
		valid: Boolean;
	end record;

    function Factorial(n : Natural) return Natural;
    function FactorialRecursion(n : Natural) return Natural;

    function GCD(a, b : Natural) return Natural;
    function GCDRecursion(a, b : Natural) return Natural;
    
    function Diophantine(a, b, c : Integer) return DiophantineResult;
    function DiophantineRecursion(a, b, c : Integer) return DiophantineResult;
end Library;
