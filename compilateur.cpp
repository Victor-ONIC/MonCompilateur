
/// GRAMMAIRE DU LANGAGE
//
// cf tokeniser.l
// 
// Letter := "a"|...|"z"|"A"|...|"Z"
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// AdditiveOperator := "+" | "-" | "||"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
//
// Number := Digit {Digit}
// Identifier := Letter {Letter | Digit}
// Factor := Number | Identifier | "(" Expression ")" | "!" Factor
// Term := Factor {MultiplicativeOperator Factor}
// SimpleExpression := Term {AdditiveOperator Term}
// Expression := SimpleExpression [RelationalOperator SimpleExpression]
//
// AssignmentStatement := Identifier ":=" Expression
// IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
// WhileStatement := "WHILE" Expression "DO" Statement
// ForStatement := "FOR" AssignmentStatement "TO" Expression "DO" Statement
// BlockStatement := "BEGIN" Statement {";" Statement} "END"
// DisplayStatement := "DISPLAY" Expression
// Statement := AssignmentStatement | IfStatement | WhileStatement | ForStatement | BlockStatement | DisplayStatement
//
// StatementPart := Statement {";" Statement} "."
// DeclarationPart := "[" Identifier {"," Identifier} "]"
// Program := [DeclarationPart] StatementPart
//
// Type := "INTEGER" | "BOOLEAN"
// VarDeclaration := Identifier {"," Identifier} ":" Type
// DeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
//
///

#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>
#include <vector>

enum OPREL { EQU, DIFF, INF, SUP, INFE, SUPE, WTFR };
enum OPADD { ADD, SUB, OR, WTFA };
enum OPMUL { MUL, DIV, MOD, AND, WTFM };
enum TYPE { INTEGER, BOOLEAN, WTFT };

// Traduction de ENUM à STRING pour afficher des erreurs plus lisibles.
std::string tokenString[] = {
	"FEOF", "UNKNOWN", "NUMBER", "ID", "STRINGCONST", "LBRACKET", "RBRACKET",
	"LPARENT", "RPARENT", "COMMA", "SEMICOLON", "DOT", "ADDOP", "MULOP",
	"RELOP", "NOT", "ASSIGN", "KEYWORD"
};
std::string typeString[] = {
	"UNSIGNED INT", "BOOLEAN"
};

// Les variables sont stockés sous la forme d'une struct.
struct Variable {
	std::string name;
	TYPE type;
};
// On doit surcharger l'opérateur < pour pouvoir faire une recherche
// dans DeclaredVariables.
bool operator<(const Variable& left, const Variable& right) {
	return left.name < right.name;
}

// Type du token actuellement sous la tête de lecture (id, keyword, ...).
TOKEN current;

// lexer->yylex() avance la tête de lecture et donne le type du token.
// lexer->YYText() donne le texte associé au token lu, sous forme d'un c-string.
FlexLexer* lexer = new yyFlexLexer;

// Stocke les noms des variables du programme Pascal.
std::set<Variable> DeclaredVariables;

// Valeur incrémentale qui permet d'avoir plusieurs étiquettes du même nom.
unsigned long long TagNumber = 0;



/// Retourne vrai si la variable a déjà été déclarée.
bool IsDeclared(const char *id) {
	return DeclaredVariables.find({id}) != DeclaredVariables.end();
}

/// Arrêt total du programme, laissant la compilation inachevée.
void Error(std::string s) {
	std::cerr << "Ligne n°" << lexer->lineno() << ", lu: '" << lexer->YYText() << "' (" << tokenString[current] << ")." << std::endl;
	std::cerr << s << std::endl;
	exit(-1);
}



TYPE Expression();  // Called by Term and calls Term
void Statement();  // Cross references between statement parts

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator() {
	OPMUL opmul;
	if (strcmp(lexer->YYText(), "*") == 0) {
		opmul = MUL;
	} else if (strcmp(lexer->YYText(), "/") == 0) {
		opmul = DIV;
	} else if (strcmp(lexer->YYText(), "%") == 0) {
		opmul = MOD;
	} else if (strcmp(lexer->YYText(), "&&") == 0) {
		opmul = AND;
	} else {
		opmul = WTFM;
	}
	current = (TOKEN)lexer->yylex();  // reconnaître l'opérateur
	return opmul;
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator() {
	OPADD opadd;
	if (strcmp(lexer->YYText(), "+") == 0) {
		opadd=ADD;
	} else if (strcmp(lexer->YYText(), "-") == 0) {
		opadd=SUB;
	} else if (strcmp(lexer->YYText(), "||") == 0) {
		opadd=OR;
	} else {
		opadd=WTFA;
	}
	current = (TOKEN)lexer->yylex();  // reconnaître l'opérateur
	return opadd;
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator() {
	OPREL oprel;
	if(strcmp(lexer->YYText(),"==") == 0) {
		oprel = EQU;
	} else if(strcmp(lexer->YYText(),"!=") == 0) {
		oprel = DIFF;
	} else if(strcmp(lexer->YYText(),"<") == 0) {
		oprel = INF;
	} else if(strcmp(lexer->YYText(),">") == 0) {
		oprel = SUP;
	} else if(strcmp(lexer->YYText(),"<=") == 0) {
		oprel = INFE;
	} else if(strcmp(lexer->YYText(),">=") == 0) {
		oprel = SUPE;
	} else {
		oprel = WTFR;
	}
	current = (TOKEN)lexer->yylex();  // reconnaître l'opérateur
	return oprel;
}

// Identifier := Letter {Letter | Digit}
TYPE Identifier() {
	if (current != ID) {
		Error("Identifier: ID attendu!");
	}

	std::string varName = lexer->YYText();
	if (!IsDeclared(varName.c_str())) {
		Error("Identifier: variable non déclarée!");
	}

	std::cout << "\tpush " << varName << std::endl;
	current = (TOKEN)lexer->yylex();  // reconnaître l'ID
	
	return DeclaredVariables.find({varName})->type;
}

// Number := Digit {Digit}
TYPE Number() {
	if (current != NUMBER) {
		Error("Number: NUMBER attendu!");
	}

	std::cout << "\tpush $" << atoi(lexer->YYText()) << std::endl;
	current = (TOKEN)lexer->yylex();  // reconnaître le NUMBER
	return INTEGER;
}

// Factor := Number | Identifier | "(" Expression ")" | "!" Factor | "TRUE" | "FALSE"
TYPE Factor() {
	TYPE returnType;

	if (current == NUMBER) {
		returnType = Number();  // reconnaître Number
	} else if (current == ID) {
		returnType = Identifier();  // reconnaître Identifier
	} else if (current == LPARENT) {

		current = (TOKEN)lexer->yylex();  // reconnaître "("
		returnType = Expression();  // reconnaître Expression
		if (current != RPARENT) {
			Error("Factor: ')' attendue!");
		}
		current = (TOKEN)lexer->yylex();  // reconnaître ")"

	} else if (current == NOT) {
		unsigned long long nb = ++TagNumber;

		current = (TOKEN)lexer->yylex();  // reconnaître "!"

		TYPE factorType = Factor();  // reconnaître Factor
		if (factorType != BOOLEAN) {
			Error("Factor: TYPE ERROR: BOOLEAN attendu ! (" + typeString[factorType] + ")");
		}

		std::cout << "\tpop %rax" << "\t# NOT" << std::endl;
		std::cout << "\tcmpq $0, %rax" << std::endl;
		std::cout << "\tjne Faux" << nb << std::endl;

		std::cout << "\tpush $0xFFFFFFFFFFFFFFFF" << std::endl;
		std::cout << "\tjmp Suite" << nb << std::endl;
		std::cout << "Faux" << nb << ":" << std::endl;
		std::cout << "\tpush $0" << std::endl;
		std::cout << "Suite" << nb << ":" << std::endl;

		returnType = BOOLEAN;
	} else if (current == KEYWORD) {
		if (strcmp("TRUE", lexer->YYText()) != 0 && strcmp("FALSE", lexer->YYText()) != 0) {
			Error("Factor: Unexpected KEYWORD!");
		}

		if (strcmp("TRUE", lexer->YYText()) == 0) {
			std::cout << "\tpush $0xFFFFFFFFFFFFFFFF" << std::endl;
		} else {
			std::cout << "\tpush $0x0" << std::endl;
		}

		current = (TOKEN)lexer->yylex();  // reconnaître TRUE ou FALSE

		returnType = BOOLEAN;
	} else {
		Error("Factor: NUMBER ou ID ou '(' attendus!");
	}

	return returnType;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term() {
	// Le type de retour de Term est le type de chaque opérande.
	TYPE returnType = Factor();  // reconnaître Facteur

	while (current == MULOP) {
		// Sauvegarder l'opérateur utilisé.
		OPMUL mulop = MultiplicativeOperator();  // reconnaître MultiplicativeOperator

		TYPE operandType = Factor();  // reconnaître Facteur
		if (returnType != operandType) {
			Error("Term: TYPE ERROR! (" + typeString[returnType] + " and " + typeString[operandType] + ")");
		}

		std::cout << "\tpop %rbx" << std::endl;	// get first operand
		std::cout << "\tpop %rax" << std::endl;	// get second operand

		switch(mulop) {
			case AND:
				std::cout << "\tandq %rbx, %rax" << std::endl;  // %rax and %rbx => %rax
				std::cout << "\tpush %rax\t# AND" << std::endl;  // store result
				break;
			case MUL:
				std::cout << "\tmulq %rbx" << std::endl;  // %rbx * %rax => %rdx:%rax
				std::cout << "\tpush %rax\t# MUL" << std::endl;  // store result   ^
				break;
			case DIV:
				std::cout << "\tmovq $0, %rdx" << std::endl;  // Higher part of numerator  
				std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
				std::cout << "\tpush %rax\t# DIV" << std::endl;  // store result    ^
				break;
			case MOD:
				std::cout << "\tmovq $0, %rdx" << std::endl;  // Higher part of numerator  
				std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
				std::cout << "\tpush %rdx\t# MOD" << std::endl;  // store result           ^
				break;
			default:
				Error("Term: Opérateur multiplicatif attendu!");
		}
	}
	return returnType;
}

// SimpleExpression := Term {AdditiveOperator Term}
TYPE SimpleExpression(){
	// Le type de retour de SimpleExpression est le type de chaque opérande.
	TYPE returnType = Term();  // reconnaître Term

	while(current == ADDOP) {
		// Sauvegarder l'opérateur utilisé.
		OPADD adop = AdditiveOperator();  // reconnaître AdditiveOperator

		TYPE operandType = Term();  // reconnaître Term
		if (returnType != operandType) {
			Error("SimpleExpression: TYPE ERROR! (" + typeString[returnType] + " and " + typeString[operandType] + ")");
		}

		std::cout << "\tpop %rbx"<<std::endl;  // get first operand
		std::cout << "\tpop %rax"<<std::endl;  // get second operand

		switch(adop) {
			case OR:
				std::cout << "\torq %rbx, %rax\t# OR" << std::endl;  // %rax or %rbx => %rax
				break;
			case ADD:
				std::cout << "\taddq %rbx, %rax\t# ADD" << std::endl;  // %rax + %rbx => %rax
				break;
			case SUB:	
				std::cout << "\tsubq %rbx, %rax\t# SUB" << std::endl;  // %rax - %rbx => %rax
				break;
			default:
				Error("SimpleExpression: Opérateur additif inconnu!");
		}
		std::cout << "\tpush %rax" << std::endl;  // store result
	}
	return returnType;
}

/// Produit le code pour calculer l'expression, et place le résultat au sommet de la pile.
// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPE Expression() {
	// Le type de retour de Expression est:
	//    - type des SimpleExpressions
	//    - booléen s'il y a un opérateur relationnel
	TYPE returnType = SimpleExpression();  // reconnaître SimpleExpression

	if (current == RELOP) {
		unsigned long long nb = ++TagNumber;

		// Sauvegarder l'opérateur utilisé.
		OPREL oprel = RelationalOperator();  // reconnaître RelationalOperator

		TYPE operandType = SimpleExpression();  // reconnaître SimpleExpression
		if (returnType != operandType) {
			Error("Expression: TYPE ERROR! (" + typeString[returnType] + " and " + typeString[operandType] + ")");
		}

		std::cout << "\tpop %rax" << std::endl;
		std::cout << "\tpop %rbx" << std::endl;
		std::cout << "\tcmpq %rax, %rbx" << std::endl;

		switch (oprel) {
			case EQU:
				std::cout << "\tje Vrai" << nb << "\t# If equal" << std::endl;
				break;
			case DIFF:
				std::cout << "\tjne Vrai" << nb << "\t# If not equal" << std::endl;
				break;
			case SUPE:
				std::cout << "\tjae Vrai" << nb << "\t# If above or equal" << std::endl;
				break;
			case INFE:
				std::cout << "\tjbe Vrai" << nb << "\t# If below or equal" << std::endl;
				break;
			case INF:
				std::cout << "\tjb Vrai" << nb << "\t# If below" << std::endl;
				break;
			case SUP:
				std::cout << "\tja Vrai" << nb << "\t# If above" << std::endl;
				break;
			default:
				Error("Expression: Opérateur de comparaison inconnu!");
		}

		std::cout << "\tpush $0\t# False" << std::endl;
		std::cout << "\tjmp Suite" << nb << std::endl;
		std::cout << "Vrai" << nb << ":" << std::endl;
		std::cout << "\tpush $0xFFFFFFFFFFFFFFFF\t# True" << std::endl;	
		std::cout << "Suite" << nb << ":" << std::endl;

		return BOOLEAN;
	}
	return returnType;
}

// AssignmentStatement := Identifier ":=" Expression
void AssignmentStatement() {
	if (current != ID) {
		Error("AssignmentStatement: ID attendu!");
	}
	if (!IsDeclared(lexer->YYText())) {
		std::cerr << "Erreur: Variable '" << lexer->YYText() << "' non déclarée!" << std::endl;
		exit(-1);
	}

	std::string variable = lexer->YYText();  // nom de la variable
	TYPE variableType = DeclaredVariables.find({variable})->type;  // type de la variable
	current = (TOKEN)lexer->yylex();  // reconnaître ID

	if (current != ASSIGN) {
		Error("AssignmentStatement: Caractères ':=' attendus!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":="

	TYPE exprType = Expression();  // reconnaître Expression

	if (variableType != exprType) {
		Error("AssignmentStatement: TYPE ERROR! (" + typeString[variableType] + " and " + typeString[exprType] + ")");
	}

	std::cout << "\tpop " << variable << "\t# Assign" << std::endl;
}

// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
void IfStatement() {
	unsigned long long nb = ++TagNumber;

	if (current != KEYWORD) {
		Error("IfStatement: KEYWORD attendu!");
	}
	if (strcmp("IF", lexer->YYText()) != 0) {
		Error("IfStatement: IF keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître IF
	std::cout << "If" << nb << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != BOOLEAN) {
		Error("IfStatement: TYPE ERROR: BOOLEAN attendu ! (" + typeString[exprType] + ")");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje Else" << nb << std::endl;

	if (current != KEYWORD) {
		Error("IfStatement: KEYWORD attendu");
	}
	if (strcmp("THEN", lexer->YYText()) != 0) {
		Error("IfStatement: THEN keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître THEN

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
	unsigned long long nb = ++TagNumber;

	if (current != KEYWORD) {
		Error("WhileStatement: KEYWORD attendu!");
	}
	if (strcmp("WHILE", lexer->YYText()) != 0) {
		Error("WhileStatement: WHILE keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître WHILE
	std::cout << "TestWhile" << nb << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != BOOLEAN) {
		Error("WhileStatement: TYPE ERROR: BOOLEAN attendu ! (" + typeString[exprType] + ")");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje EndWhile" << nb << std::endl;

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
	unsigned long long nb = ++TagNumber;

	if (current != KEYWORD) {
		Error("ForStatement: KEYWORD attendu!");
	}
	if (strcmp("FOR", lexer->YYText()) != 0) {
		Error("ForStatement: FOR keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître FOR
	std::string var = lexer->YYText();  // nom de la variable utilisée lors de l'incrémentation (prochain token)
	std::cout << "For" << nb << ":" << std::endl;

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
	std::cout << "\tcmpq %rax, " << var << std::endl;
	std::cout << "\tja EndFor" << nb << std::endl;
	
	if (current != KEYWORD) {
		Error("ForStatement: KEYWORD attendu!");
	}
	if (strcmp("DO", lexer->YYText()) != 0) {
		Error("ForStatement: DO keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître DO

	Statement();  // reconnaître Statement

	std::cout << "\taddq $1, " << var << std::endl;  // incrémenter
	std::cout << "\tjmp TestFor" << nb << std::endl;

	std::cout << "EndFor" << nb << ":" << std::endl;
}

// BlockStatement := "BEGIN" Statement {";" Statement} "END"
void BlockStatement() {
	unsigned long long nb = ++TagNumber;

	if (current != KEYWORD) {
		Error("BlockStatement: KEYWORD attendu!");
	}
	if (strcmp("BEGIN", lexer->YYText()) != 0) {
		Error("BlockStatement: BEGIN keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître BEGIN
	std::cout << "Begin" << nb << ":" << std::endl;

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
	std::cout << "End" << nb << ":" << std::endl;
}

// DisplayStatement := "DISPLAY" Expression
void DisplayStatement() {
	if (current != KEYWORD) {
		Error("DisplayStatement: KEYWORD attendu!");
	}
	if (strcmp("DISPLAY", lexer->YYText()) != 0) {
		Error("DisplayStatement: DISPLAY keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître DISPLAY

	TYPE exprType = Expression();  // reconnaître Expression

	if (exprType != INTEGER) {
		Error("DisplayStatement: TYPE ERROR: INTEGER attendu! (" + typeString[exprType] + ")");
	}

	std::cout << "\tpop %rdx" << std::endl;
	std::cout << "\tmovq $FormatStringLLU" << ", %rsi" << std::endl;
	std::cout << "\tmovl $1, %edi" << std::endl;
	std::cout << "\tmovl $0, %eax" << std::endl;
	std::cout << "\tcall __printf_chk@PLT" << std::endl;
}

// Statement := AssignmentStatement | IfStatement | WhileStatement | ForStatement | BlockStatement | DisplayStatement
void Statement() {
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
		} else if (strcmp("DISPLAY", lexer->YYText()) == 0) {
			DisplayStatement();
		} else {
			Error("Statement: Unexpected KEYWORD!");
		}
	} else {
		Error("Statement: ID ou KEYWORD attendu!");
	}
}

// StatementPart := Statement {";" Statement} "."
void StatementPart() {
	std::cout << "\t.align 8" << std::endl;
	std::cout << "\t.text" << std::endl;
	std::cout << "\t.globl main\t# The main function must be visible from outside" << std::endl;
	std::cout << "main:" << std::endl;
	std::cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top" << std::endl;
	std::cout << std::endl;

	Statement();  // reconnaître Statement

	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();  // reconnaître ";"
		Statement();  // reconnaître Statement
	}

	if (current != DOT) {
		Error("StatementPart: '.' attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître "."
}

// Type := "INTEGER" | "BOOLEAN"
TYPE Type() {
	TYPE type;
	if (strcmp(lexer->YYText(), "INTEGER") == 0) {
		type = INTEGER;
	} else if (strcmp(lexer->YYText(), "BOOLEAN") == 0) {
		type = BOOLEAN;
	} else {
		type = WTFT;
	}
	current = (TOKEN)lexer->yylex();  // reconnaître le type.
	return type;
}

// VarDeclaration := Identifier {"," Identifier} ":" Type
void VarDeclaration() {
	// Ensemble de noms des variables déclarées ici.
	std::set<std::string> variables;

	if (current != ID) {
		Error("VarDeclaration: ID attendu!");
	}

	variables.insert(lexer->YYText());
	current = (TOKEN)lexer->yylex();  // reconnaître Identifier

	while(current == COMMA) {
		current = (TOKEN)lexer->yylex();  // reconnaître ","

		variables.insert(lexer->YYText());
		current = (TOKEN)lexer->yylex();  // reconnaître Identifier
	}

	if (current != COLON) {
		Error("VarDeclaration: COLON attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	TYPE varType = Type();  // reconnaître Type

	// 1. Déclarer et 2. Créer les variables lues.
	switch (varType) {
		case INTEGER:
			// .quad.
			for (std::string variable : variables) {
				DeclaredVariables.insert({variable, INTEGER});  // déclarer
				std::cout << variable << ":\t.quad 0" << std::endl;  // créer
			}
			break;
		case BOOLEAN:
			// .byte OU .quad également, on n'est pas à ça près.
			for (std::string variable : variables) {
				DeclaredVariables.insert({variable, BOOLEAN});  // déclarer
				std::cout << variable << ":\t.quad 0" << std::endl;  // créer
			}
			break;
		default:
			Error("VarDeclaration: Type inconnu.");
	}
}

// DeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
void DeclarationPart() {
	if (current != KEYWORD) {
		Error("DeclarationPart: KEYWORD attendu!");
	}
	if (strcmp("VAR", lexer->YYText()) != 0) {
		Error("DeclarationPart: VAR keyword attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître "VAR"

	VarDeclaration();  // reconnaître VarDeclaration

	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();  // reconnaître ";"
		VarDeclaration();  // reconnaître VarDeclaration
	}

	if (current != DOT) {
		Error("DeclarationPart: '.' attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître "."

	std::cout << std::endl;
}

// Program := [DeclarationPart] StatementPart
void Program() {
	if (current == KEYWORD) {
		// Partie déclaration optionnelle.
		// Mais si pas de variable, alors pas d'instructions...
		DeclarationPart();
	}
	StatementPart();  // partie instruction obligatoire
}

/// First version: Source code on standard input and assembly code on standard output.
int main() {
	// Header for gcc assembler / linker
	std::cout << "# This code was produced by VICTOR's Vcompiler. <3" << std::endl;
	std::cout << std::endl;
	std::cout << "\t.data" << std::endl;
	std::cout << "FormatStringLLU:\t.string \"%llu\\n\"" << std::endl;

	// Let's proceed to the analysis and code production
	current = (TOKEN) lexer->yylex();  // déplacer la tête de lecture sur le premier token
	Program();  // reconnaître le PROGRAMME

	// Trailer for the gcc assembler / linker
	std::cout << std::endl;
	std::cout << "\tmovq %rbp, %rsp\t# Restore the position of the stack's top" << std::endl;
	std::cout << "\tret\t# Return from main function" << std::endl;

	if (current != FEOF) {
		Error("Fin du programme attendue.");  // unexpected characters at the end of program
	}
	
	return 0;
}

/*

Notes:

POUR CHAR
a:	.byte '6'
...
movq $0, %rax
movb a, %al
push %rax
...
pop %rax
... %al

Questions:


*/

/* EXEMPLE

On lit le fichier source token par token.
(Les espaces, retours à la ligne, et tabulations sont ignorés)

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
