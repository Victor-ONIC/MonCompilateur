#include "main.h"
#include "Compiler.h"

// FormatString
std::map<std::string, bool> FS = {
	{"LLU", false}, {"F", false}, {"C", false}, {"B", false}, {"S", false},
	{"LLUln", false}, {"Fln", false}, {"Cln", false}, {"Bln", false}, {"Sln", false}
};

int main(int argc, char** argv) {
    Compiler compilateur;
    compilateur.run();
    return 0;
}

// flex++ -d -o tokeniser.cpp tokeniser.l
// g++ -c tokeniser.cpp
// g++ -c main.cpp
// g++ -c Compiler.cpp
// g++ -c Variable.cpp
// g++ -c Subroutine.cpp
// g++ -o compilateur tokeniser.o main.cpp Compiler.cpp Variable.cpp Subroutine.cpp

// ./compilateur < test.p > test.s
// gcc -ggdb -no-pie -fno-pie test.s -o test
