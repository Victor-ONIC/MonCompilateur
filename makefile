MAKEFLAGS += --silent

all: test

go: compilateur test

clean:
	rm *.o *.s
	rm tokeniser.cpp

cleangit: clean
	rm compilateur
	rm test

tokeniser.cpp: tokeniser.l
	flex++ -d -o tokeniser.cpp tokeniser.l

tokeniser.o: tokeniser.cpp
	g++ -c tokeniser.cpp

compilateur: compilateur.cpp tokeniser.o
	g++ -ggdb -o compilateur compilateur.cpp tokeniser.o

test: compilateur test.p
	./compilateur < test.p > test.s
	gcc -ggdb -no-pie -fno-pie test.s -o test

asm:
	gcc -ggdb -no-pie -fno-pie test.s -o test
