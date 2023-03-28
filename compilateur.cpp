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
void ReadChar()
{
	while (std::cin.get(current) && (current == ' ' || current == '\t' || current == '\n'))
	{
	   	std::cin.get(current);
	}
}

// Cette fonction affiche un message dans la sortie d'erreur et quitte
// le programme, laissant la compilatiion inachevée.
void Error(std::string message)
{
	std::cerr << message << std::endl;
	exit(-1);
}



//////////////////////////////////////////////////////////////////////////
// GRAMMAIRE DU LANGAGE

// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
void Digit();

// AdditiveOperator := "+" | "-"
void AdditiveOperator();

// Term := Digit | "(" ArithmeticExpression ")"
void Term();

// ArithmeticExpression := Term {AdditiveOperator Term}
void ArithmeticExpression();



void Digit()
{
	if (current < '0' || current > '9')
	{
		Error("Digit: Chiffre attendu.");  // Digit expected.
	}
	else
	{
		std::cout << "\tpush\t$" << current << std::endl;
		ReadChar();
	}
}

void AdditiveOperator()
{
	if (current == '+' || current == '-')
	{
		ReadChar();
	}
	else
	{
		Error("AdditiveOperator: Opérateur additif attendu.");  // Additive operator expected.
	}
}

void Term()
{
	if (current == '(')
	{
		ReadChar();
		ArithmeticExpression();
		if (current != ')')
		{
			Error("Term: ')' était attendu.");  // Après une expression on attend ')'.
		}
		else
		{
			ReadChar();
		}
	}
	else
	{
		if (current >= '0' && current <= '9')
		{
			Digit();
		}
	    else
		{
			Error("Term: '(' ou chiffre attendu.");
		}
	}
}

void ArithmeticExpression()
{
	char adop;
	Term();
	while (current == '+' || current == '-')
	{
		// Save operator in local variable.
		adop = current;  

		AdditiveOperator();
		Term();

		std::cout << "\tpop\t\t%rbx" << std::endl;  // Get first operand.
		std::cout << "\tpop\t\t%rax" << std::endl;  // Get second operand.

		if(adop == '+')
		{
			std::cout << "\taddq\t%rbx, %rax" << std::endl;  // Add both operands.
		}
		else
		{
			std::cout << "\tsubq\t%rbx, %rax" << std::endl;  // Substract both operands.
		}

		std::cout << "\tpush\t%rax" << std::endl;  // Store result.
	}
}

// First version : Source code on standard input and assembly code on standard output.
int main()
{
	// Header for gcc assembler / linker.
	std::cout << "# This code was produced by the CERI Compiler.\n"             	      << std::endl;
	std::cout << "\t.text\t\t\t\t\t# The following lines contain the program."            << std::endl;
	std::cout << "\t.globl main\t\t\t\t# The main function must be visible from outside." << std::endl;
	std::cout << "main:\t\t\t\t\t\t# The main function body:"                             << std::endl;
	std::cout << "\tmovq\t%rsp, %rbp\t\t# Save the position of the top of the stack."     << std::endl;

	// Let's proceed to the analysis and code production.
	ReadChar();
	ArithmeticExpression();
	ReadChar();

	// Trailer for the gcc assembler / linker.
	std::cout << "\tmovq\t%rbp, %rsp\t\t# Restore the position of the top of the stack." << std::endl;
	std::cout << "\tret\t\t\t\t\t\t# Return from main function."                         << std::endl;

	// Unexpected characters at the end of program.
	if(std::cin.get(current))
	{
		std::cerr << "Caractères en trop à la fin du programme : [" << current << "]";
		Error(".");  
	}
}
