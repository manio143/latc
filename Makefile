all: src/native_runtime/runtime.o
	ghc --make -isrc/parser:src/frontend:src/backend src/Main.hs

src/native_runtime/runtime.o: src/native_runtime/runtime.h src/native_runtime/runtime.c
	gcc -O2 -c src/native_runtime/runtime.c -o src/native_runtime/runtime.o
	cp src/native_runtime/runtime.o runtime
	cp src/native_runtime/asm_externs runtime.ext

src/native_runtime/test : src/native_runtime/runtime.o src/native_runtime/runtime_test.c
	gcc -g -c src/native_runtime/runtime_test.c -o src/native_runtime/runtime_test.o
	gcc src/native_runtime/runtime_test.o src/native_runtime/runtime.o -lunistring -o src/native_runtime/test