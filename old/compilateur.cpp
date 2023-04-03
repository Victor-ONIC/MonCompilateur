//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"



#include <string>
#include <iostream>
#include <cstdlib>

// Current character.
char current;

// Read character and skip spaces until non space character is read.
void ReadChar() {
	while (std::cin.get(current) && (current == ' ' || current == '\t' || current == '\n')) {
	   	std::cin.get(current);
	}
}

// Cette fonction affiche un message dans la sortie d'erreur et quitte
// le programme, laissant la compilatiion inachevée.
void Error(std::string message) {
	std::cerr << message << std::endl;
	exit(-1);
}

// GRAMMAIRE DU LANGAGE
// <Digit> ::= "0" .. "9"
// <AdditiveOperator> ::= "+" | "-"
// <Term> ::= <Digit> | "(" <ArithmeticExpression> ")"
// <ArithmeticExpression> ::= <Term> { <AdditiveOperator> <Term> }
// <Expression> ::= <ArithmeticExpression> | <ArithmeticExpression> <RelationalOperator> <ArithmeticExpression>
// <RelationalOperator> ::= "=" | "<>" | "<" | "<=" | ">=" | ">"

// <Digit> ::= "0" | .. | "9"
void Digit() {
	if (current >= '0' & current <= '9') {
		std::cout << "push $" << current << std::endl;  // lire un chiffre => l'empiler
		ReadChar();
	} else {
		Error("Digit: Chiffre attendu. ('1', ..., '9')");
	}
}

// <AdditiveOperator> ::= "+" | "-"
void AdditiveOperator() {
	if (current == '+' || current == '-') {
		ReadChar();
	}
	else {
		Error("AdditiveOperator: Opérateur additif attendu. ('+', '-')");
	}
}

// <Term> ::= <Digit> | "(" <ArithmeticExpression> ")"
void ArithmeticExpression();  // Déclaration.
void Term() {
	if (current >= '0' && current <= '9') {
		Digit();
	}
	else if (current == '(') {
		ReadChar();
		ArithmeticExpression();
		if (current == ')')
			ReadChar();
		else
			Error("Term: ')' était attendu.");
	}
	else {
		Error("Term: '(' était attendu.");
	}
}

// <ArithmeticExpression> ::= <Term> { <AdditiveOperator> <Term> }
void ArithmeticExpression() {
	char op;

	Term();
	while (current == '+' || current == '-') {
		op = current;  // On sauvegarde l'opérateur utilisé.

		AdditiveOperator();
		Term();

		std::cout << "pop %rbx" << std::endl;  // Opérande 1.
		std::cout << "pop %rax" << std::endl;  // Opérande 2.

		if (op == '+') {
			std::cout << "addq %rbx, %rax" << std::endl;  // Faire la somme.
		} else {
			std::cout << "subq %rbx, %rax" << std::endl;  // Faire la différence.
		}

		std::cout << "push %rax" << std::endl;  // Stocker le résultat.
	}
}

// <RelationalOperator> ::= "=" | "<>" | "<" | "<=" | ">=" | ">"
void RelationalOperator() {
	char prev = current;
	ReadChar();

	if (current == '=' ||
		current == '<' ||
		current == '>' ||
		(prev == '<' && current == '>') ||
		(prev == '<' && current == '=') ||
		(prev == '>' && current == '=')) {
			ReadChar();
	} else {
		Error("RelationalOperator: opérateur attendu ('=', '<>', '<', '>', '<=', '>=')");
	}
}

// <Expression> ::= <ArithmeticExpression> | <ArithmeticExpression> <RelationalOperator> <ArithmeticExpression>
void Expression() {
	ArithmeticExpression();
	// ???
}



// First version : Source code on standard input and assembly code on standard output.
int main() {
	// Header for gcc assembler / linker.
	std::cout << "# This code was produced by the CERI Compiler.\n" << std::endl;
	std::cout << ".text # The following lines contain the program." << std::endl;
	std::cout << ".globl main # The main function must be visible from outside." << std::endl;
	std::cout << "main: # The main function body:" << std::endl;
	std::cout << "movq %rsp, %rbp # Save the position of the top of the stack." << std::endl;

	// Let's proceed to the analysis and code production.
	ReadChar();
	ArithmeticExpression();
	ReadChar();

	// Trailer for the gcc assembler / linker.
	std::cout << "movq %rbp, %rsp # Restore the position of the top of the stack." << std::endl;
	std::cout << "ret # Return from main function." << std::endl;

	// Unexpected characters at the end of program.
	if (std::cin.get(current)) {
		std::cerr << "Caractères en trop à la fin du programme : [" << current << "]";
		Error(".");  
	}
}
