all: Wrapper.gpr main
	gprbuild -p -P Wrapper.gpr

main:
	cd src && gcc -c -fdump-ada-spec -C ./library.h -lm && cd ..

clean:
	rm -rf main src/build src/*.ads