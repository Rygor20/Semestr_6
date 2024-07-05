with Ada.Text_IO; use Ada.Text_IO;

package body Library is

    function Factorial(n : Natural) return Natural is
		result : Natural := 1;
	begin
		for i in 1..n loop
			result := result * i;
		end loop;
		return result;
	end Factorial;

    function FactorialRecursion(n : Natural) return Natural is
	begin
		if n = 0 then
			return 1;
		else
			return n * FactorialRecursion(n - 1);
		end if;
	end FactorialRecursion;


    function GCD(a, b : Natural) return Natural is
		a2, b2, temp : Natural;
	begin
		a2 := a;
		b2 := b;

		while b2 /= 0 loop
			temp := b2;
			b2 := a2 mod b2;
			a2 := temp;
		end loop;
		return a2;
	end GCD;

    function GCDRecursion(a, b : Natural) return Natural is
	begin
		if b = 0 then
			return a;
		else
			return GCDRecursion(b, a mod b);
		end if;
	end GCDRecursion;


    function Diophantine(a, b, c : Integer) return DiophantineResult is
		a2, b2, x1, y1, x2, y2, q, r, temp, x, y : Integer;
	begin
		a2 := a;
		b2 := b;

		x1 := 1;
		y1 := 0;
		x2 := 0;
		y2 := 1;

		while b2 /= 0 loop
			q := a2 / b2;
			r := a2 mod b2;

			temp := x1 - q * x2;
			x1 := x2;
			x2 := temp;

			temp := y1 - q * y2;
			y1 := y2;
			y2 := temp;

			a2 := b2;
			b2:= r;
		end loop;

		if c mod a2 /= 0 then
			return (0, 0, False);
		else
			x := x1 * (c / a2);
			y := y1 * (c / a2);
			return (x, y, True);
		end if;
	end Diophantine;

    function DiophantineRecursion(a, b, c : Integer) return DiophantineResult is
		x, y: Integer;
		result: DiophantineResult;
	begin
		if a = 0 and b = 0 and c /= 0 then
			return (0, 0, False);
		elsif a = 0 then
			x := 0;
			y := c / b;
			return (x, y, True);
		elsif b = 0 then
			x := c / a;
			y := 0;
			return (x, y, True);
		end if;

		result := DiophantineRecursion(b, a mod b, c);
		x := result.y;
		y := result.x;
		y := y - (a / b) * x;
		return (x, y, result.valid);
	end DiophantineRecursion;

end Library;
