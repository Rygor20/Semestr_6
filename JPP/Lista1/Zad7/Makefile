all: library test

library:
	go build -buildmode=c-archive library.go

test:
	gcc -pthread test.c ./library.a -o test

clean:
	rm -f *.a *.h test