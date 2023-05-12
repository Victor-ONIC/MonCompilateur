
/// GRAMMAIRE DU LANGAGE
//
// cf tokeniser.l
//    Identifier
//    Number
//    Float
//    Character
// 
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// AdditiveOperator := "+" | "-" | "||"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
//
// Factor := Number | Identifier | Float | Character | "(" Expression ")" | "!" Factor | "TRUE" | "FALSE"
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
// CaseLabelList := Constant {"," Constant}
// CaseListElement := CaseLabelList ":" Statement
// CaseStatement := "CASE" Expression "OF" CaseListElement {";" CaseListElement} "END"
// Statement := AssignmentStatement | IfStatement | WhileStatement | ForStatement | BlockStatement | DisplayStatement | CaseStatement
//
// StatementPart := Statement {";" Statement} "."
// Type := "INTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR"
// VarDeclaration := Identifier {"," Identifier} ":" Type
// DeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
// Program := [DeclarationPart] StatementPart
//
///

#include <cstdlib>
#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include <vector>
#include <FlexLexer.h>
#include "tokeniser.h"

enum OPREL { EQU, DIFF, INF, SUP, INFE, SUPE, WTFR };
enum OPADD { ADD, SUB, OR, WTFA };
enum OPMUL { MUL, DIV, MOD, AND, WTFM };
enum TYPE { INTEGER, BOOLEAN, DOUBLE, CHAR, WTFT };

// Traduction de ENUM à STRING pour afficher des erreurs plus lisibles.
std::string tokenString[] = {
	"FEOF", "UNKNOWN", "NUMBER", "ID", "STRINGCONST", "LBRACKET", "RBRACKET",
	"LPARENT", "RPARENT", "COMMA", "SEMICOLON", "DOT", "ADDOP", "MULOP",
	"RELOP", "NOT", "ASSIGN", "KEYWORD", "COLON", "FLOATCONST", "CHARCONST"
};
std::string typeString[] = {
	"INTEGER", "BOOLEAN", "DOUBLE", "CHAR"
};

// Les variables sont stockées sous la forme d'une struct, afin d'avoir
// un nombre variable d'attributs.
struct Variable {
	std::string name;
	TYPE type;
};
// On doit surcharger l'opérateur < pour pouvoir faire une recherche
// dans DeclaredVariables.
// Pour comparer deux struct Variable on ne regarde que leur nom.
bool operator<(const Variable& left, const Variable& right) {
	return left.name < right.name;
}

// Stocke les variables du programme.
std::set<Variable> DeclaredVariables;

// Type du token actuellement sous la tête de lecture (id, keyword, ...).
TOKEN current;

// lexer->yylex() avance la tête de lecture et donne le type du token.
// lexer->YYText() donne le texte associé au token lu, sous forme d'un c-string.
FlexLexer* lexer = new yyFlexLexer;

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

void ReadKeyword(std::string keyword) {
	if (current != KEYWORD) {
		Error("KEYWORD attendu!");
	}

	if (strcmp(keyword.c_str(), lexer->YYText()) != 0) {
		Error("Mot clé " + keyword + " attendu!");
	}

	current = (TOKEN)lexer->yylex();
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

TYPE Number() {
	if (current != NUMBER) {
		Error("Number: Nombre entier attendu!");
	}

	std::cout << "\tpush $" << atoi(lexer->YYText()) << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le NUMBER

	return INTEGER;
}

TYPE Float() {
	if (current != FLOATCONST) {
		Error("Float: Constante flottante attendue!");
	}

	// Interpréter le flottant 64 bits comme un entier 64 bits.
	double f = atof(lexer->YYText());
	unsigned long long* i = (unsigned long long*)&f;

	// On ne peut pas directement empiler, donc on passe par un registre.
	std::cout << "\tmovq $" << *i << ", %rax" << std::endl;
	std::cout << "\tpush %rax # Empile " << f << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le FLOATCONST

	return DOUBLE;
}

TYPE Character() {
	if (current != CHARCONST) {
		Error("Character: Constante caractère attendue!");
	}

	char c = lexer->YYText()[1];  // sera toujours de la forme 'x'
	std::cout << "\tmovq $0, %rax" << std::endl;
	std::cout << "\tmovb $" << (int)c << ", %al" << std::endl;
	std::cout << "\tpush %rax" << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le CHARCONST

	return CHAR;
}

// Factor := Number | Identifier | Float | Character | "(" Expression ")" | "!" Factor | "TRUE" | "FALSE"
TYPE Factor() {
	TYPE returnType;

	if (current == NUMBER) {
		returnType = Number();  // reconnaître Number
	} else if (current == ID) {
		returnType = Identifier();  // reconnaître Identifier
	} else if (current == FLOATCONST) {
		returnType = Float();  // reconnaître Float
	} else if (current == CHARCONST) {
		returnType = Character();  // reconnaître Character
	} else if (current == LPARENT) {
		current = (TOKEN)lexer->yylex();  // reconnaître "("

		returnType = Expression();  // reconnaître Expression
		if (current != RPARENT) {
			Error("Factor: ')' attendue!");
		}

		current = (TOKEN)lexer->yylex();  // reconnaître ")"
	} else if (current == NOT) {
		current = (TOKEN)lexer->yylex();  // reconnaître "!"

		TYPE factorType = Factor();  // reconnaître Factor
		if (factorType != BOOLEAN) {
			Error("Factor: TYPE ERROR: BOOLEAN attendu ! (" + typeString[factorType] + ")");
		}

		std::cout << "\tnotq (%rsp)" << std::endl;

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
		Error("Factor: Facteur inconnu!");
	}

	return returnType;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term() {
	TYPE returnType = Factor();  // reconnaître Facteur

	while (current == MULOP) {
		OPMUL mulop = MultiplicativeOperator();  // reconnaître MultiplicativeOperator

		TYPE operandType = Factor();  // reconnaître Facteur
		if (returnType != operandType) {
			Error("Term: TYPE ERROR! (" + typeString[returnType] + " and " + typeString[operandType] + ")");
		}

		// Obtenir les opérandes.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			std::cout << "\tpop %rbx" << std::endl;
			std::cout << "\tpop %rax" << std::endl;
		}

		switch(mulop) {
			case AND:
				if (returnType != BOOLEAN) {
					Error("Term: Le type de l'expression doit être booléen!");
				}
				std::cout << "\tandq %rbx, %rax" << std::endl;  // %rax and %rbx => %rax
				std::cout << "\tpush %rax\t# AND" << std::endl;  // store result      ^
				break;
			case MUL:
				if (returnType != INTEGER && returnType != DOUBLE) {
					Error("Term: Le type de l'expression doit être entier ou flottant!");
				}

				if (returnType == INTEGER) {
					std::cout << "\tmulq %rbx" << std::endl;  // %rbx * %rax => %rdx:%rax
					std::cout << "\tpush %rax\t# MUL" << std::endl;  // store result:  ^
				} else {
					std::cout << "\tfmulp %st(0), %st(1)\t# MUL" << std::endl;  // %st(1) * %st(0) => %st(0)
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				}
				break;
			case DIV:
				if (returnType != INTEGER && returnType != DOUBLE) {
					Error("Term: Le type de l'expression doit être entier ou flottant!");
				}

				if (returnType == INTEGER) {
					std::cout << "\tmovq $0, %rdx" << std::endl;  // Higher part of numerator  
					std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
					std::cout << "\tpush %rax\t# DIV" << std::endl;  // store result:   ^
				} else {
					std::cout << "\tfdivp %st(0), %st(1)\t# DIV" << std::endl;  // %st(1) / %st(0) => %st(0)
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				}
				break;
			case MOD:
				if (returnType != INTEGER) {
					Error("Term: Le type de l'expression doit être entier!");
				}
				std::cout << "\tmovq $0, %rdx" << std::endl;  // Higher part of numerator  
				std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
				std::cout << "\tpush %rdx\t# MOD" << std::endl;  // store result:          ^
				break;
			default:
				Error("Term: Opérateur multiplicatif inconnu!");
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

		// Obtenir les opérandes.
		if (returnType == DOUBLE) {
			// DOUBLE
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			// INTEGER, BOOLEAN, CHAR
			std::cout << "\tpop %rbx" << std::endl;
			std::cout << "\tpop %rax" << std::endl;
		}

		switch(adop) {
			case OR:
				if (returnType != BOOLEAN) {
					Error("SimpleExpression: Le type de l'expression doit être booléen!");
				}
				std::cout << "\torq %rbx, %rax\t# OR" << std::endl;  // %rax or %rbx => %rax
				std::cout << "\tpush %rax" << std::endl;  // store result                 ^
				break;
			case ADD:
				if (returnType != INTEGER && returnType != DOUBLE) {
					Error("SimpleExpression: Le type de l'expression doit être entier ou flottant!");
				}

				if (returnType == INTEGER) {
					std::cout << "\taddq %rbx, %rax\t# ADD" << std::endl;  // %rax + %rbx => %rax
					std::cout << "\tpush %rax" << std::endl;  // store result                  ^
				} else {
					std::cout << "\tfaddp %st(0), %st(1)\t# ADD" << std::endl;  // %st(1) / %st(0) => %st(0)
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				}
				break;
			case SUB:
				if (returnType != INTEGER && returnType != DOUBLE) {
					Error("SimpleExpression: Le type de l'expression doit être entier ou flottant!");
				}

				if (returnType == INTEGER) {
					std::cout << "\tsubq %rbx, %rax\t# SUB" << std::endl;  // %rax - %rbx => %rax
					std::cout << "\tpush %rax" << std::endl;  // store result                  ^
				} else {
					std::cout << "\tfsubp %st(0), %st(1)\t# SUB" << std::endl;  // %st(1) / %st(0) => %st(0)
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				}
				break;
			default:
				Error("SimpleExpression: Opérateur additif inconnu!");
		}
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

		// Obtenir les opérandes.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
			std::cout << "\tfcomi %st(1)" << std::endl;
		} else {
			std::cout << "\tpop %rax" << std::endl;
			std::cout << "\tpop %rbx" << std::endl;
			std::cout << "\tcmpq %rax, %rbx" << std::endl;
		}

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

	ReadKeyword("IF");

	std::cout << "If" << nb << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != BOOLEAN) {
		Error("IfStatement: TYPE ERROR: BOOLEAN attendu ! (" + typeString[exprType] + ")");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje Else" << nb << std::endl;

	ReadKeyword("THEN");

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

	ReadKeyword("WHILE");

	std::cout << "TestWhile" << nb << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != BOOLEAN) {
		Error("WhileStatement: TYPE ERROR: BOOLEAN attendu ! (" + typeString[exprType] + ")");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje EndWhile" << nb << std::endl;

	ReadKeyword("DO");

	Statement();  // reconnaître Statement

	std::cout << "\tjmp TestWhile" << nb << std::endl;
	std::cout << "EndWhile" << nb << ":" << std::endl;
}

// ForStatement := "FOR" AssignmentStatement ("TO" | "DOWNTO") Expression "DO" Statement
void ForStatement() {
	unsigned long long nb = ++TagNumber;

	ReadKeyword("FOR");

	std::string var = lexer->YYText();  // nom de la variable utilisée lors de l'incrémentation (prochain token)
	std::cout << "For" << nb << ":" << std::endl;

	AssignmentStatement();  // reconnaître AssignmentStatement

	if (current != KEYWORD) {
		Error("KEYWORD attendu!");
	}
	if (strcmp("TO", lexer->YYText()) != 0 && strcmp("DOWNTO", lexer->YYText()) != 0) {
		Error("Mots clés 'TO' ou 'DOWNTO' attendus!");
	}

	std::string jump;
	std::string increment;
	if (strcmp("TO", lexer->YYText()) == 0) {
		jump = "\tja EndFor";
		increment = "\taddq $1, ";
	} else {
		jump = "\tjb EndFor";
		increment = "\tsubq $1, ";
	}
	current = (TOKEN)lexer->yylex();

	std::cout << "TestFor" << nb << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != INTEGER) {
		Error("ForStatement: L'incrément doit être entier!");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq %rax, " << var << std::endl;
	std::cout << jump << nb << std::endl;
	
	ReadKeyword("DO");

	Statement();  // reconnaître Statement

	std::cout << increment << var << std::endl;  // incrémenter l'entier
	std::cout << "\tjmp TestFor" << nb << std::endl;
	std::cout << "EndFor" << nb << ":" << std::endl;
}

// BlockStatement := "BEGIN" Statement {";" Statement} "END"
void BlockStatement() {
	unsigned long long nb = ++TagNumber;

	ReadKeyword("BEGIN");

	std::cout << "Begin" << nb << ":" << std::endl;

	Statement();  // reconnaître Statement

	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();  // reconnaître ";"
		Statement();  // reconnaître Statement
	}

	ReadKeyword("END");

	std::cout << "End" << nb << ":" << std::endl;
}

// DisplayStatement := "DISPLAY" Expression
void DisplayStatement() {
	ReadKeyword("DISPLAY");

	TYPE exprType = Expression();  // reconnaître Expression

	switch (exprType) {
		case INTEGER:
			std::cout << "\tpop %rdx"                    << std::endl;
			std::cout << "\tmovq $FormatStringLLU, %rsi" << std::endl;
			std::cout << "\tmovl $0, %eax"               << std::endl;
			std::cout << "\tmovl $1, %edi"               << std::endl;
			std::cout << "\tcall __printf_chk@PLT"       << std::endl;
			break;
		case BOOLEAN: {
			unsigned long long tag = ++TagNumber;
			std::cout << "\tpop %rdx"                << std::endl;
			std::cout << "\tcmpq $0, %rdx"           << std::endl;
			std::cout << "\tje False" << tag         << std::endl;
			std::cout << "\tmovq $TrueString, %rsi"  << std::endl;
			std::cout << "\tjmp Suite" << tag        << std::endl;
			std::cout << "False" << tag << ":"       << std::endl;
			std::cout << "\tmovq $FalseString, %rsi" << std::endl;
			std::cout << "Suite" << tag << ":"       << std::endl;
			std::cout << "\tmovl $0, %eax"           << std::endl;
			std::cout << "\tmovl $1, %edi"           << std::endl;
			std::cout << "\tcall __printf_chk@PLT"   << std::endl;
			break;
		}
		case DOUBLE:
			std::cout << "\tmovsd (%rsp), %xmm0"       << std::endl;
			std::cout << "\tmovq $FormatStringF, %rsi" << std::endl;
			std::cout << "\tmovl $1, %eax"             << std::endl;
			std::cout << "\tmovl $1, %edi"             << std::endl;
			std::cout << "\tcall __printf_chk@PLT"     << std::endl;
			std::cout << "\taddq $8, %rsp"             << std::endl;
			break;
		case CHAR:
			std::cout << "\tpop %rdx"                  << std::endl;
			std::cout << "\tmovq $FormatStringC, %rsi" << std::endl;
			std::cout << "\tmovl $0, %eax"             << std::endl;
			std::cout << "\tmovl $1, %edi"             << std::endl;
			std::cout << "\tcall __printf_chk@PLT"     << std::endl;
			break;
		default:
			Error("DisplayStatement: TYPE inconnu!");
	}
}

// Constant := Number | Float | Character
TYPE Constant() {
	TYPE returnType;

	switch (current) {
		case NUMBER:
			returnType = Number();
			break;
		case FLOATCONST:
			returnType = Float();
			break;
		case CHARCONST:
			returnType = Character();
			break;
		default:
			Error("Constant: Constante attendue!");
	}

	return returnType;
}

// CaseLabelList := Constant {"," Constant}
TYPE CaseLabelList() {
	unsigned long long tag = TagNumber;  // numéro d'étiquette du Case actuel

	TYPE returnType = Constant();  // reconnaître la constante

	std::cout << "\tpop %rax" << std::endl;
	std::cout << "\tcmpq (%rsp), %rax" << std::endl;
	std::cout << "\tje Statement" << tag << std::endl;

	while (current == COMMA) {
		current = (TOKEN)lexer->yylex();  // reconnaître ","
		TYPE constType = Constant();  // reconnaître la constante

		if (returnType != constType) {
			Error("CaseLabelList: Les constantes doivent avoir le même type!");
		}

		std::cout << "\tpop %rax" << std::endl;
		std::cout << "\tcmpq (%rsp), %rax" << std::endl;
		std::cout << "\tje Statement" << tag << std::endl;
	}

	return returnType;
}

// CaseListElement := CaseLabelList ":" Statement
// Paramètre - endTagNumber: tous les cas ont le même numéro d'étiquette pour la fin du CASE.
TYPE CaseListElement(unsigned long long endTagNumber) {
	unsigned long long tag = TagNumber;  // numéro d'étiquette du cas actuel
	// unsigned long long nextTag = tag + 1;  // numéro du prochain cas

	TYPE labelType = CaseLabelList();  // reconnaître CaseLabelList

	if (current != COLON) {
		Error("CaseListElement: COLON attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	std::cout << "\tjmp Case" << tag+1 << std::endl;
	std::cout << "Statement" << tag << ":" << std::endl;

	Statement();  // reconnaître Statement

	std::cout << "\tjmp EndCase" << endTagNumber << std::endl;

	// Cette étiquette doit correspondre avec le jump juste au dessus.
	std::cout << "Case" << tag+1 << ":" << std::endl;

	return labelType;
}

// CaseStatement := "CASE" Expression "OF" CaseListElement {";" CaseListElement} [";" "ELSE" Statement] "END"
void CaseStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("CASE");

	std::cout << "Case" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression

	ReadKeyword("OF");

	TYPE labelType = CaseListElement(tag);  // reconnaître CaseListElement

	if (exprType != labelType) {
		Error("CaseStatement: Types incompatibles: " + typeString[exprType] + " et " + typeString[labelType]);
	}

	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();  // reconnaître ";"

		if (current == KEYWORD && strcmp("ELSE", lexer->YYText()) == 0) {
			current = (TOKEN)lexer->yylex();
			Statement();
			break;
		}
	
		TYPE labelType = CaseListElement(tag);  // reconnaître CaseListElement

		if (exprType != labelType) {
			Error("CaseStatement: Types incompatibles: " + typeString[exprType] + " et " + typeString[labelType]);
		}
	}

	ReadKeyword("END");

	std::cout << "EndCase" << tag << ":" << std::endl;
	std::cout << "\taddq $8, %rsp" << std::endl;
}

// Statement := AssignmentStatement | IfStatement | WhileStatement | ForStatement | BlockStatement | DisplayStatement | CaseStatement
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
		} else if (strcmp("CASE", lexer->YYText()) == 0) {
			CaseStatement();
		} else {
			Error("Statement: Unexpected KEYWORD!");
		}
	} else {
		Error("Statement: ID ou KEYWORD attendu!");
	}
}

// StatementPart := Statement {";" Statement} "."
void StatementPart() {
	std::cout << "\t.text"           << std::endl;
	std::cout << "\t.globl main"     << std::endl;
	// std::cout << "\t.align 8"     << std::endl;
	std::cout << "main:"             << std::endl;
	std::cout << "\tmovq %rsp, %rbp" << std::endl;
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

// Type := "INTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR"
TYPE Type() {
	if (current != KEYWORD) {
		Error("Type: KEYWORD de type attendu!");
	}

	TYPE type;
	if (strcmp(lexer->YYText(), "INTEGER") == 0) {
		type = INTEGER;
	} else if (strcmp(lexer->YYText(), "BOOLEAN") == 0) {
		type = BOOLEAN;
	} else if (strcmp(lexer->YYText(), "DOUBLE") == 0) {
		type = DOUBLE;
	} else if (strcmp(lexer->YYText(), "CHAR") == 0) {
		type = CHAR;
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

	// Vérification
	if (IsDeclared(lexer->YYText())) {
		Error("VarDeclaration: Variable '" + std::string(lexer->YYText()) + "' déjà déclarée!");
	}
	variables.insert(lexer->YYText());
	current = (TOKEN)lexer->yylex();  // reconnaître Identifier

	while(current == COMMA) {
		current = (TOKEN)lexer->yylex();  // reconnaître ","

		// Vérification
		if (IsDeclared(lexer->YYText())) {
			Error("VarDeclaration: Variable '" + std::string(lexer->YYText()) + "' déjà déclarée!");
		}
		variables.insert(lexer->YYText());
		current = (TOKEN)lexer->yylex();  // reconnaître Identifier
	}

	if (current != COLON) {
		Error("VarDeclaration: COLON attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	TYPE varType = Type();  // reconnaître Type

	// 1. Déclarer et 2. Créer les variables lues.
	// Toutes les variables sont initialisées avec la valeur 0 (0.0, false).
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
		case DOUBLE:
			// .double
			for (std::string variable : variables) {
				DeclaredVariables.insert({variable, DOUBLE});  // déclarer
				std::cout << variable << ":\t.double 0.0" << std::endl;  // créer
			}
			break;
		case CHAR:
			// .byte
			for (std::string variable : variables) {
				DeclaredVariables.insert({variable, CHAR});  // déclarer
				std::cout << variable << ":\t.byte 0" << std::endl;  // créer
			}
			break;
		default:
			Error("VarDeclaration: Type inconnu.");
	}
}

// DeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
void DeclarationPart() {
	ReadKeyword("VAR");

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
	// Header for gcc assembler / linker.
	std::cout << "# This code was produced by VICTOR's Vcompiler. <3" << std::endl;
	std::cout << std::endl;
	std::cout << "\t.data" << std::endl;
	std::cout << "FormatStringLLU:\t.string \"%llu\\n\"" << std::endl;
	std::cout << "FormatStringF:\t.string \"%lf\\n\"" << std::endl;
	std::cout << "FormatStringC:\t.string \"%c\\n\"" << std::endl;
	std::cout << "TrueString:\t.string \"TRUE\\n\"" << std::endl;
	std::cout << "FalseString:\t.string \"FALSE\\n\"" << std::endl;

	// Let's proceed to the analysis and code production.
	current = (TOKEN) lexer->yylex();  // déplacer la tête de lecture sur le premier token
	Program();  // reconnaître le PROGRAMME entier.

	// Trailer for the gcc assembler / linker.
	std::cout << std::endl;
	std::cout << "\tmovq $0, %rax" << std::endl;  // valeur de retour de main égale à 0.
	std::cout << "\tmovq %rbp, %rsp\t# Restore the position of the stack's top" << std::endl;
	std::cout << "\tret\t# Return from main function" << std::endl;

	if (current != FEOF) {
		Error("Fin du programme attendue.");  // unexpected characters at the end of program
	}
	
	return 0;
}

/*

Notes:
	BOOLEAN .byte 0
		8 bits, 0 étendus à 64 pour être empilés
	CHAR .byte 0
		8 bits, 0 étendus à 64 pour être empilés
	INTEGER .quad 0
		64 bits
	DOUBLE .double 0.0
		64 bits

	BOOLEAN peut être stocké comme $1 et $0 sur un octet.
	De cette manière on peut faire NOT avec:
		movb $1, %al  # TRUE => %al : 0x00000001
		xorl $1, %al
			    0b 00000001 (TRUE)    &        0b 00000000 (FALSE)
			xor 0b 00000001                xor 0b 00000001
			  = 0b 00000000 (FALSE)          = 0b 00000001 (TRUE)
	Faire xor 1 équivaut à faire not.
	On ne veut toucher que le denrier bit donc on fait xor 1 (et non pas xor FF par exemple).
	Seul le dernier bit compte.

	Types compatibles ? ---> promotion de type d'expression
	TYPES EXISTANTS: INTEGER, BOOLEAN, DOUBLE, CHAR
		double int & int double
		double char & char double
		double bool & bool double
		int char & char int
		int bool & bool int
		bool char & char bool
	Expressions
		DOUBLE +,-,*,/ INTEGER -> DOUBLE   #
		INTEGER +,-,*,/ DOUBLE -> DOUBLE   # fild & fist
		DOUBLE +,-,*,/ CHAR -> DOUBLE      #
		CHAR +,-,*,/ DOUBLE -> DOUBLE      #
		DOUBLE +,-,*,/ BOOLEAN -> DOUBLE   #
		BOOLEAN +,-,*,/ DOUBLE -> DOUBLE   #
		INTEGER +,-,*,/,% CHAR -> INTEGER
		CHAR +,-,*,/,% INTEGER -> INTEGER
		INTEGER +,-,*,/,% BOOLEAN -> INTEGER
		BOOLEAN +,-,*,/,% INTEGER -> INTEGER
		BOOLEAN +,-,*,/,% CHAR -> INTEGER
		CHAR +,-,*,/,% BOOLEAN -> INTEGER
	Assignment
		INTEGER = BOOLEAN -> INTEGER
		INTEGER = DOUBLE -> INTEGER  # (#include <cmath> int y = (int)std::round(x);)
		INTEGER = CHAR -> INTEGER
		BOOLEAN = INTEGER -> BOOLEAN  # cmp $0
		BOOLEAN = DOUBLE -> BOOLEAN   # cmp flottante avec $0
		BOOLEAN = CHAR -> BOOLEAN     # cmp $0
		DOUBLE = INTEGER -> DOUBLE
		DOUBLE = BOOLEAN -> DOUBLE
		DOUBLE = CHAR -> DOUBLE
		CHAR = INTEGER -> CHAR
		CHAR = BOOLEAN -> CHAR
		CHAR = DOUBLE -> CHAR  # (#include <cmath> int y = (int)std::round(x);)


Questions:
	ordre des opérations fpu ?
	le résultat d'une opération est toujours empilé ? plutôt (dest op src => dest) ?

	est-ce qu'on a le droit à tous les documents lors de l'examen ?

*/

/*

TP à rendre avant l'examen écrit. Voir date limite du dépôt.

Faire un petit texte qui précise les différences avec la version finale du prof.
Ce qui est commun avec la version du prof (ex. à partir du TP1, puis après c'est moi).
Lister, résumer ce qu'on a fait globalement. Pas trop de détails.
Attirer l'attention sur ce qu'on a fait de différent, de mieux, de particulier.

Boucle FOR fonctionnelle c'est la moyenne: 10/20.

Pour avoir 20
Propre
Avoir fait quelque chose qui n'a pas été enseigné. Aller au delà du cours. Attaquer des problèmes non abordés. Qui apporte quelque chose.
Ex: les procédures avec des paramètres.
Ex: les RECORDs (structures, types définis par l'utilisateur).

À IMPLÉMENTER:
compatibilité des types
STRINGCONST
revoir le système d'erreurs
étiquettes redondantes qui aident à la lisibilité du code assembleur ? ex: Ifn

*/
