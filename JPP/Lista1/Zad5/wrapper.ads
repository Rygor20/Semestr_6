with Interfaces.C; use Interfaces.C;
with System; use System;

--  UWAGA: Biblioteka napisana w C znajduje się w folderze "c".

package Wrapper is

  type DiophantineResult is record
		x, y : Integer;
		valid: Boolean;
	end record;
  pragma Convention (C, DiophantineResult);

  function Factorial(n : Natural) return Natural;
  Import => True, Convention => C, External_Name => "factorial";
  function FactorialRecursion(n : Natural) return Natural;
  Import => True, Convention => C, External_Name => "factorial_recursion";

  function GCD(a, b : Natural) return Natural;
  Import => True, Convention => C, External_Name => "gcd";
  function GCDRecursion(a, b : Natural) return Natural;
  Import => True, Convention => C, External_Name => "gcd_recursion";

  function Diophantine(a, b, c : Integer) return DiophantineResult;
  Import => True, Convention => C, External_Name => "diophantine";
  function DiophantineRecursion(a, b, c : Integer) return DiophantineResult;
  Import => True, Convention => C, External_Name => "diophantine_recursion";

  -- function Factorial (Number : int) return int with
   -- Import => True, Convention => C, External_Name => "factorial";

  -- function Factorial_rec (Number : int) return int with
   -- Import => True, Convention => C, External_Name => "factorial_rec";

  -- function GCD (A, B : int) return int with
   -- Import => True, Convention => C, External_Name => "GCD";

  -- function GCD_rec (A, B : int) return int with
   -- Import => True, Convention => C, External_Name => "GCD_rec";

  -- function Diophantine (A, B, C : int) return Answer with
   -- Import => True, Convention => C, External_Name => "diophantine";

  -- function Diophantine_rec (A, B, C : int) return Answer with
   -- Import => True, Convention => C, External_Name => "diophantine_rec";

end Wrapper;
