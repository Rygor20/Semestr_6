with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h; use x86_64_linux_gnu_bits_types_h;
with library_h; use library_h;


procedure Main is
	result_iter: library_h.DiophantineResult;
	result_recur: library_h.DiophantineResult;
begin
	Put_Line("Loops:");

	Put_Line("5! = " & uu_uint64_t'Image(library_h.Factorial(5)));
	Put_Line("NWD(24, 36) = " & uu_uint64_t'Image(library_h.GCD(24, 36)));

	result_iter := library_h.Diophantine(3, 5, -3);
	if result_iter.valid then
		Put_Line("Rozwiązanie równania 3x + 5y = -3: x = " & uu_int64_t'Image(result_iter.x) & ", y = " & uu_int64_t'Image(result_iter.y));
	else
		Put_Line("Rozwiązanie równania 3x + 5y = -3 nie istnieje");
	end if;

	Put_Line("");
	Put_Line("Recursion");

	Put_Line("5! = " & uu_uint64_t'Image(library_h.Factorial_Recursion(5)));
	Put_Line("NWD(24, 36) = " & uu_uint64_t'Image(library_h.GCD_Recursion(24, 36)));

	result_recur := library_h.Diophantine_Recursion(3, 5, -3);
	if result_recur.valid then
		Put_Line("Rozwiązanie równania 3x + 5y = -3: x = " & uu_int64_t'Image(result_recur.x) & ", y = " & uu_int64_t'Image(result_recur.y));
	else
		Put_Line("Rozwiązanie równania 3x + 5y = -3 nie istnieje");
	end if;
end Main;