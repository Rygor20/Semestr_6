all: test

test: test.c library.c
	gcc -Wall -Wextra -g -o $@ $^ -lm

clean:
	rm -f test