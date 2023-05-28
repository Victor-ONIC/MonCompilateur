MAKEFLAGS += --silent

# Build the Pascal program's executable with `make`.
all: test

tokeniser.cpp: tokeniser.l
	flex++ -d -o tokeniser.cpp tokeniser.l

tokeniser.o: tokeniser.cpp
	g++ -c tokeniser.cpp

# Build the compiler executable with `make compilateur`.
compilateur: tokeniser.o main.cpp Compiler.cpp Variable.cpp Subroutine.cpp
	g++ -o compilateur tokeniser.o main.cpp Compiler.cpp Variable.cpp Subroutine.cpp

# Build the Pascal program's executable with `make test`.
test: compilateur test.p
	./compilateur < test.p > test.s
	gcc -ggdb -no-pie -fno-pie test.s -o test

# Reassemble the assembly file if needed with `make asm`.
asm: test.s
	gcc -ggdb -no-pie -fno-pie test.s -o test

# Leaves ./compilateur & ./test
clean:
	rm *.o *.s
	rm tokeniser.cpp

# Used to leave only what needs to be pushed.
cleangit: clean
	rm compilateur
	rm test
