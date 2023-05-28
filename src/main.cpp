#include "Compiler.h"

int main(int argc, char** argv) {
    Compiler* compilateur = new Compiler();
    compilateur->run();
    delete compilateur;
    return 0;
}
