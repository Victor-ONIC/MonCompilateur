
// Build with "make compilateur"

/// GRAMMAIRE DU LANGAGE (?)
//
// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Letter {"," Letter} "]"
// StatementPart := Statement {";" Statement} "."
//
// Statement := AssignmentStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
// AssignmentStatement := Letter "=" Expression
// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
// WhileStatement := "WHILE" Expression "DO" Statement
// ForStatement := "FOR" AssignmentStatement "TO" Expression "DO" Statement
// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
//
// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}
//
// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"

#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>

enum OPREL { EQU, DIFF, INF, SUP, INFE, SUPE, WTFR };
enum OPADD { ADD, SUB, OR, WTFA };
enum OPMUL { MUL, DIV, MOD, AND, WTFM };

// Type du token actuellement sous la tête de lecture (id, keyword, ...).
TOKEN current;

// lexer->yylex() avance la tête de lecture et donne le type du token.
// lexer->YYText() donne le texte associé au token lu sous forme d'un c-string.
FlexLexer* lexer = new yyFlexLexer;

// Stocke les noms des variables du programme Pascal.
std::set<std::string> DeclaredVariables;

// Valeur incrémentale qui permet d'avoir plusieurs étiquettes du même nom.
unsigned long TagNumber = 0;



/// Retourne vrai si la variable a déjà été déclarée.
bool IsDeclared(const char *id) {
	return DeclaredVariables.find(id) != DeclaredVariables.end();
}

/// Arrêt total du programme, laissant la compilation inachevée.
void Error(std::string s){
	std::cerr << "Ligne n°" << lexer->lineno() << ", lu: '" << lexer->YYText() << "' (" << current << "), mais... " << std::endl;
	std::cerr << s << std::endl;
	exit(-1);
}



void Identifier() {
	std::cout << "\tpush " << lexer->YYText() << std::endl;
	current = (TOKEN)lexer->yylex();
}

void Number() {
	std::cout << "\tpush $" << atoi(lexer->YYText()) << std::endl;
	current = (TOKEN)lexer->yylex();
}

void Expression();			// Called by Term() and calls Term()

void Factor(){
	if(current==RPARENT){
		current=(TOKEN) lexer->yylex();
		Expression();
		if(current!=LPARENT)
			Error("')' était attendu");		// ")" expected
		else
			current=(TOKEN) lexer->yylex();
	}
	else 
		if (current==NUMBER)
			Number();
	    else
			if(current==ID)
				Identifier();
			else
				Error("'(' ou chiffre ou lettre attendue");
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(){
	OPMUL opmul;
	if(strcmp(lexer->YYText(),"*")==0)
		opmul=MUL;
	else if(strcmp(lexer->YYText(),"/")==0)
		opmul=DIV;
	else if(strcmp(lexer->YYText(),"%")==0)
		opmul=MOD;
	else if(strcmp(lexer->YYText(),"&&")==0)
		opmul=AND;
	else opmul=WTFM;
	current=(TOKEN) lexer->yylex();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
void Term(){
	OPMUL mulop;
	Factor();
	while(current==MULOP){
		mulop=MultiplicativeOperator();		// Save operator in local variable
		Factor();
		std::cout << "\tpop %rbx"<<std::endl;	// get first operand
		std::cout << "\tpop %rax"<<std::endl;	// get second operand
		switch(mulop){
			case AND:
				std::cout << "\tmulq	%rbx"<<std::endl;	// a * b -> %rdx:%rax
				std::cout << "\tpush %rax\t# AND"<<std::endl;	// store result
				break;
			case MUL:
				std::cout << "\tmulq	%rbx"<<std::endl;	// a * b -> %rdx:%rax
				std::cout << "\tpush %rax\t# MUL"<<std::endl;	// store result
				break;
			case DIV:
				std::cout << "\tmovq $0, %rdx"<<std::endl; 	// Higher part of numerator  
				std::cout << "\tdiv %rbx"<<std::endl;			// quotient goes to %rax
				std::cout << "\tpush %rax\t# DIV"<<std::endl;		// store result
				break;
			case MOD:
				std::cout << "\tmovq $0, %rdx"<<std::endl; 	// Higher part of numerator  
				std::cout << "\tdiv %rbx"<<std::endl;			// remainder goes to %rdx
				std::cout << "\tpush %rdx\t# MOD"<<std::endl;		// store result
				break;
			default:
				Error("opérateur multiplicatif attendu");
		}
	}
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(){
	OPADD opadd;
	if(strcmp(lexer->YYText(),"+")==0)
		opadd=ADD;
	else if(strcmp(lexer->YYText(),"-")==0)
		opadd=SUB;
	else if(strcmp(lexer->YYText(),"||")==0)
		opadd=OR;
	else opadd=WTFA;
	current=(TOKEN) lexer->yylex();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
void SimpleExpression(){
	OPADD adop;
	Term();
	while(current==ADDOP){
		adop=AdditiveOperator();		// Save operator in local variable
		Term();
		std::cout << "\tpop %rbx"<<std::endl;	// get first operand
		std::cout << "\tpop %rax"<<std::endl;	// get second operand
		switch(adop){
			case OR:
				std::cout << "\taddq	%rbx, %rax\t# OR"<<std::endl;// operand1 OR operand2
				break;			
			case ADD:
				std::cout << "\taddq	%rbx, %rax\t# ADD"<<std::endl;	// add both operands
				break;			
			case SUB:	
				std::cout << "\tsubq	%rbx, %rax\t# SUB"<<std::endl;	// substract both operands
				break;
			default:
				Error("opérateur additif inconnu");
		}
		std::cout << "\tpush %rax"<<std::endl;			// store result
	}

}

// DeclarationPart := "[" Ident {"," Ident} "]"
void DeclarationPart(){
	if(current!=RBRACKET)
		Error("caractère '[' attendu");
	std::cout << "\t.data"<<std::endl;
	std::cout << "\t.align 8"<<std::endl;
	
	current=(TOKEN) lexer->yylex();
	if(current!=ID)
		Error("Un identificater était attendu");
	std::cout << lexer->YYText() << ":\t.quad 0"<<std::endl;
	DeclaredVariables.insert(lexer->YYText());
	current=(TOKEN) lexer->yylex();
	while(current==COMMA){
		current=(TOKEN) lexer->yylex();
		if(current!=ID)
			Error("Un identificateur était attendu");
		std::cout << lexer->YYText() << ":\t.quad 0"<<std::endl;
		DeclaredVariables.insert(lexer->YYText());
		current=(TOKEN) lexer->yylex();
	}
	if(current!=LBRACKET)
		Error("caractère ']' attendu");
	current=(TOKEN) lexer->yylex();
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator(){
	OPREL oprel;
	if(strcmp(lexer->YYText(),"==")==0)
		oprel=EQU;
	else if(strcmp(lexer->YYText(),"!=")==0)
		oprel=DIFF;
	else if(strcmp(lexer->YYText(),"<")==0)
		oprel=INF;
	else if(strcmp(lexer->YYText(),">")==0)
		oprel=SUP;
	else if(strcmp(lexer->YYText(),"<=")==0)
		oprel=INFE;
	else if(strcmp(lexer->YYText(),">=")==0)
		oprel=SUPE;
	else oprel=WTFR;
	current=(TOKEN) lexer->yylex();
	return oprel;
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
void Expression(){
	// Produit le code pour calculer l'expression et placer au sommet de la pile le résultat.
	OPREL oprel;
	SimpleExpression();
	if(current==RELOP){
		oprel=RelationalOperator();
		SimpleExpression();
		std::cout << "\tpop %rax"<<std::endl;
		std::cout << "\tpop %rbx"<<std::endl;
		std::cout << "\tcmpq %rax, %rbx"<<std::endl;
		switch(oprel){
			case EQU:
				std::cout << "\tje Vrai"<<++TagNumber<<"\t# If equal"<<std::endl;
				break;
			case DIFF:
				std::cout << "\tjne Vrai"<<++TagNumber<<"\t# If different"<<std::endl;
				break;
			case SUPE:
				std::cout << "\tjae Vrai"<<++TagNumber<<"\t# If above or equal"<<std::endl;
				break;
			case INFE:
				std::cout << "\tjbe Vrai"<<++TagNumber<<"\t# If below or equal"<<std::endl;
				break;
			case INF:
				std::cout << "\tjb Vrai"<<++TagNumber<<"\t# If below"<<std::endl;
				break;
			case SUP:
				std::cout << "\tja Vrai"<<++TagNumber<<"\t# If above"<<std::endl;
				break;
			default:
				Error("Opérateur de comparaison inconnu");
		}
		std::cout << "\tpush $0\t\t# False"<<std::endl;
		std::cout << "\tjmp Suite"<<TagNumber<<std::endl;
		std::cout << "Vrai"<<TagNumber<<":" << std::endl;
		std::cout << "\tpush $0xFFFFFFFFFFFFFFFF\t\t# True"<<std::endl;	
		std::cout << "Suite"<<TagNumber<<":"<<std::endl;
	}
}

// AssignmentStatement := Identifier ":=" Expression
void AssignmentStatement(){
	std::string variable;
	if(current!=ID)
		Error("Identificateur attendu");
	if(!IsDeclared(lexer->YYText())){
		std::cerr << "Erreur : Variable '"<<lexer->YYText()<<"' non déclarée"<<std::endl;
		exit(-1);
	}
	variable=lexer->YYText();
	current=(TOKEN) lexer->yylex();
	if(current!=ASSIGN)
		Error("caractères ':=' attendus");
	current=(TOKEN) lexer->yylex();
	Expression();
	std::cout << "\tpop "<<variable<<std::endl;
}

void Statement();

// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
void IfStatement() {
	// Sauvegarder un certain numéro d'étiquette.
	unsigned long nb = ++TagNumber;

	if (current != KEYWORD) {
		Error("IfStatement: KEYWORD attendu!");
	}
	if (strcmp("IF", lexer->YYText()) != 0) {
		Error("IfStatement: IF keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître IF

	Expression();  // reconnaître Expression

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje Else" << nb << std::endl;  // si l'expression a retourné 0

	if (current != KEYWORD) {
		Error("IfStatement: KEYWORD attendu");
	}
	if (strcmp("THEN", lexer->YYText()) != 0) {
		Error("IfStatement: THEN keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître THEN

	// IF récursif possible grâce à cette ligne
	Statement();  // reconnaître Statement

	std::cout << "\tjmp EndIf" << nb << std::endl;

	// ELSE optionnel (=> pas d'erreur)
	std::cout << "Else" << nb << ":" << std::endl;

	if (current == KEYWORD && strcmp("ELSE", lexer->YYText()) == 0) {
		current = (TOKEN)lexer->yylex();  // reconnaître ELSE
		Statement();  // reconnaître Statement
	}

	std::cout << "EndIf" << nb << ":" << std::endl;
}

// WhileStatement := "WHILE" Expression "DO" Statement
void WhileStatement() {
	unsigned long nb = ++TagNumber;

	if (current != KEYWORD) {
		Error("WhileStatement: KEYWORD attendu!");
	}
	if (strcmp("WHILE", lexer->YYText()) != 0) {
		Error("WhileStatement: WHILE keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître WHILE

	std::cout << "TestWhile" << nb << ":" << std::endl;

	Expression();  // reconnaître Expression

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje EndWhile" << nb << std::endl;  // si l'expression a retourné 0

	if (current != KEYWORD) {
		Error("WhileStatement: KEYWORD attendu");
	}
	if (strcmp("DO", lexer->YYText()) != 0) {
		Error("WhileStatement: DO keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître DO

	Statement();  // reconnaître Statement

	std::cout << "\tjmp TestWhile" << nb << std::endl;

	std::cout << "EndWhile" << nb << ":" << std::endl;
}

// ForStatement := "FOR" AssignmentStatement "TO" Expression "DO" Statement
void ForStatement() {
	unsigned long nb = ++TagNumber;

	if (current != KEYWORD) {
		Error("ForStatement: KEYWORD attendu!");
	}
	if (strcmp("FOR", lexer->YYText()) != 0) {
		Error("ForStatement: FOR keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître FOR
	std::string var = lexer->YYText();  // nom de la variable utilisée lors de l'incrémentation (prochain token)

	AssignmentStatement();  // reconnaître AssignmentStatement

	if (current != KEYWORD) {
		Error("ForStatement: KEYWORD attendu!");
	}
	if (strcmp("TO", lexer->YYText()) != 0) {
		Error("ForStatement: TO keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître TO

	std::cout << "TestFor" << nb << ":" << std::endl;

	Expression();  // reconnaître Expression

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq " << "%rax, " << var << std::endl;
	std::cout << "\tja EndFor" << nb << std::endl;  // si l'expression est supérieure à "var"

	std::cout << "\taddq $1, " << var << std::endl;  // incrémenter
	
	if (current != KEYWORD) {
		Error("ForStatement: KEYWORD attendu!");
	}
	if (strcmp("DO", lexer->YYText()) != 0) {
		Error("ForStatement: DO keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître DO

	Statement();  // reconnaître Statement

	std::cout << "\tjmp TestFor" << nb << std::endl;

	std::cout << "EndFor" << nb << ":" << std::endl;
}

// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
void BlockStatement() {
	if (current != KEYWORD) {
		Error("BlockStatement: KEYWORD attendu!");
	}
	if (strcmp("BEGIN", lexer->YYText()) != 0) {
		Error("BlockStatement: BEGIN keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître BEGIN

	Statement();  // reconnaître Statement

	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();  // reconnaître ";"

		Statement();  // reconnaître Statement
	}

	if (current != KEYWORD) {
		Error("BlockStatement: KEYWORD attendu!");
	}
	if (strcmp("END", lexer->YYText()) != 0) {
		Error("BlockStatement: END keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître END
}

// Statement := AssignmentStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
void Statement(){
	if (current == ID) {
		AssignmentStatement();
	} else if (current == KEYWORD) {
		if (strcmp("IF", lexer->YYText()) == 0) {
			IfStatement();
		} else if (strcmp("WHILE", lexer->YYText()) == 0) {
			WhileStatement();
		} else if (strcmp("FOR", lexer->YYText()) == 0) {
			ForStatement();
		} else if (strcmp("BEGIN", lexer->YYText()) == 0) {
			BlockStatement();
		}
	} else {
		Error("Statement: ID ou KEYWORD attendu!");
	}
}

// StatementPart := Statement {";" Statement} "."
void StatementPart(){
	std::cout << "\t.text\t\t# The following lines contain the program"<<std::endl;
	std::cout << "\t.globl main\t# The main function must be visible from outside"<<std::endl;
	std::cout << "main:\t\t\t# The main function body :"<<std::endl;
	std::cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top"<<std::endl;
	Statement();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	if(current!=DOT)
		Error("caractère '.' attendu");
	current=(TOKEN) lexer->yylex();
}

// Program := [DeclarationPart] StatementPart
void Program(){
	if(current==RBRACKET)
		DeclarationPart();  // partie déclaration optionnelle, mais si on la met pas, pas de variable => pas d'instruction (ici dans ce cas là)
	StatementPart();  // partie instruction obligatoire.
}

int main(){	// First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	std::cout << "\t\t\t# This code was produced by the CERI Compiler"<<std::endl;
	// Let's proceed to the analysis and code production
	current=(TOKEN) lexer->yylex();
	Program();
	// Trailer for the gcc assembler / linker
	std::cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top"<<std::endl;
	std::cout << "\tret\t\t\t# Return from main function"<<std::endl;
	if(current!=FEOF){
		std::cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}

}


/*

Notes:

Pour IfStatement:
Si pas de ELSE, on aura deux étiquettes qui pointent vers la même adresse.
Ce n'est pas un problème.



Questions:

Comment lit-on un caractère ?
lexer->yylex() donne le TOKEN et avance la tête de lecture au prochain token
lexer->YYText() donne un cstring

Expression VS Statement (Expression VS Instruction) ?
Une expression ça finit toujours par être une valeur.
Une instruction (statement) c'est quelque chose qui est exécuté par le langage.

RPARENT et LPARENT,
RBRACKET et LBRACKET sont inversés, non ?

Rajouter des étiquettes inutiles pour aider à la compréhension du code assembleur ?

*/

/*

On lit le fichier source token par token.

Exemple: FOR i := 0 TO 10 DO i := i + 1.

lexer->yylex() ---> KEYWORD
lexer->YYText() --> "FOR"
FOR i := 0 TO 10 DO i := i + 1.
 ^

lexer->yylex() ---> ID
lexer->YYtext() --> "i"
FOR i := 0 TO 10 DO i := i + 1.
    ^

lexer->yylex() ---> ASSIGN
lexer->YYText() --> ":="
FOR i := 0 TO 10 DO i := i + 1.
       ^

lexer->yylex() ---> NUMBER
lexer->YYText() --> "0"
FOR i := 0 TO 10 DO i := i + 1.
         ^

lexer->yylex() ---> KEYWORD
lexer->YYText() --> "TO"
FOR i := 0 TO 10 DO i := i + 1.
		    ^

lexer->yylex() ---> NUMBER
lexer->YYText() --> "10"
FOR i := 0 TO 10 DO i := i + 1.
			   ^

lexer->yylex() ---> KEYWORD
lexer->YYText() --> "DO"
FOR i := 0 TO 10 DO i := i + 1.
				  ^

lexer->yylex() ---> ID
lexer->YYText() --> "i"
FOR i := 0 TO 10 DO i := i + 1.
					^

lexer->yylex() ---> ASSIGN
lexer->YYText() --> ":="
FOR i := 0 TO 10 DO i := i + 1.
					   ^

lexer->yylex() ---> ID
lexer->YYText() --> "i"
FOR i := 0 TO 10 DO i := i + 1.
						 ^

lexer->yylex() ---> ADDOP
lexer->YYText() --> "+"
FOR i := 0 TO 10 DO i := i + 1.
						   ^

lexer->yylex() ---> NUMBER
lexer->YYText() --> "1"
FOR i := 0 TO 10 DO i := i + 1.
						     ^

lexer->yylex() ---> DOT
lexer->YYText() --> "."
FOR i := 0 TO 10 DO i := i + 1.
							  ^

lexer->yylex() ---> FEOF
lexer->YYText() --> ""
FOR i := 0 TO 10 DO i := i + 1.
								  ^

*/
