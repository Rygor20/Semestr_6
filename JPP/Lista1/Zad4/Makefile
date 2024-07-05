all: test

test: test.o library.o
	gcc -Wall -Wextra -o $@ $^ -lgnat

test.o: test.c
	gcc -Wall -Wextra -c $^

library.o: library.adb
	gnatmake -c $^

clean:
	rm -f test *.o *.ali