all: src/native_runtime/runtime.o
	ghc --make -isrc/parser:src/frontend:src/backend src/Main.hs

src/native_runtime/runtime.o: src/native_runtime/runtime.h src/native_runtime/runtime.c
	gcc -c src/native_runtime/runtime.c