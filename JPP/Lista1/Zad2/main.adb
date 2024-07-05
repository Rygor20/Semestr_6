with Ada.Text_IO; use Ada.Text_IO;

with Library; use Library;

procedure Main is
	result_iter: Library.DiophantineResult;
	result_recur: Library.DiophantineResult;
begin
	Put_Line("Loops:");

	Put_Line("5! = " & Natural'Image(Library.Factorial(5)));
	Put_Line("NWD(24, 36) = " & Natural'Image(Library.GCD(24, 36)));

	result_iter := Library.Diophantine(3, 5, -3);
	if result_iter.valid then
		Put_Line("Rozwiązanie równania 3x + 5y = -3: x = " & Integer'Image(result_iter.x) & ", y = " & Integer'Image(result_iter.y));
	else
		Put_Line("Rozwiązanie równania 3x + 5y = -3 nie istnieje");
	end if;

	Put_Line("");
	Put_Line("Recursion");

	Put_Line("5! = " & Natural'Image(Library.FactorialRecursion(5)));
	Put_Line("NWD(24, 36) = " & Natural'Image(Library.GCDRecursion(24, 36)));

	result_recur := Library.DiophantineRecursion(3, 5, -3);
	if result_recur.valid then
		Put_Line("Rozwiązanie równania 3x + 5y = -3: x = " & Integer'Image(result_recur.x) & ", y = " & Integer'Image(result_recur.y));
	else
		Put_Line("Rozwiązanie równania 3x + 5y = -3 nie istnieje");
	end if;
end Main;