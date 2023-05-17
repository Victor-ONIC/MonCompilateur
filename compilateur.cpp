
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
// Type := "UINTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR"
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
enum TYPE { UINTEGER, BOOLEAN, DOUBLE, CHAR, WTFT };

// Traduction de ENUM à STRING pour afficher des erreurs plus lisibles.
std::string tokenString[] = {
	"FEOF", "UNKNOWN", "NUMBER", "ID", "STRINGCONST", "LBRACKET", "RBRACKET",
	"LPARENT", "RPARENT", "COMMA", "SEMICOLON", "DOT", "ADDOP", "MULOP",
	"RELOP", "NOT", "ASSIGN", "KEYWORD", "COLON", "FLOATCONST", "CHARCONST"
};
std::string typeString[] = {
	"UINTEGER", "BOOLEAN", "DOUBLE", "CHAR"
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
	std::cerr << "Ligne n°" << lexer->lineno() << ", lu: `" << lexer->YYText() << "` (" << tokenString[current] << ")." << std::endl;
	std::cerr << s << std::endl;
	exit(-1);
}

void ReadKeyword(std::string keyword) {
	if (current != KEYWORD) {
		Error("(ReadKeyword) Erreur: Mot clé attendu!");
	}

	if (strcmp(keyword.c_str(), lexer->YYText()) != 0) {
		Error("(ReadKeyword) Erreur: Mot clé " + keyword + " attendu!");
	}

	current = (TOKEN)lexer->yylex();
}

bool IsIntegral(TYPE type) {
	return type == UINTEGER || type == BOOLEAN || type == CHAR;
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
		Error("(Identifier) Erreur: Identifiant attendu!");
	}

	std::string varName = lexer->YYText();
	if (!IsDeclared(varName.c_str())) {
		Error("(Identifier) Erreur: Variable non déclarée!");
	}

	std::cout << "\tpush " << varName << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître l'ID
	
	return DeclaredVariables.find({varName})->type;
}

TYPE Number() {
	if (current != NUMBER) {
		Error("(Number) Erreur: Nombre entier attendu!");
	}

	std::cout << "\tpush $" << atoi(lexer->YYText()) << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le NUMBER

	return UINTEGER;
}

TYPE Boolean() {
	if (current != KEYWORD) {
		Error("(Boolean) Erreur: Mot clé `TRUE` ou `FALSE` attendu!");
	}

	if (strcmp("TRUE", lexer->YYText()) == 0) {
		std::cout << "\tpush $0xFFFFFFFFFFFFFFFF" << std::endl;
	} else {
		std::cout << "\tpush $0x0" << std::endl;
	}

	current = (TOKEN)lexer->yylex();

	return BOOLEAN;
}

TYPE Float() {
	if (current != FLOATCONST) {
		Error("(Float) Erreur: Constante flottante attendue!");
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
		Error("(Character) Erreur: Constante caractère attendue!");
	}

	std::cout << "\tmovq $0, %rax" << std::endl;
	std::cout << "\tmovb $" << lexer->YYText() << ", %al" << std::endl;
	std::cout << "\tpush %rax" << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le CHARCONST

	return CHAR;
}

TYPE String() {
	Error("(String) Attention: Pas encore implémenté.");
	return UINTEGER;
}

// Constant := Number | Boolean | Float | Character | String
TYPE Constant() {
	TYPE returnType;

	switch (current) {
		case NUMBER:
			returnType = Number();
			break;
		case KEYWORD:
			returnType = Boolean();
			break;
		case FLOATCONST:
			returnType = Float();
			break;
		case CHARCONST:
			returnType = Character();
			break;
		case STRINGCONST:
			returnType = String();
			break;
		default:
			Error("(Constant) Erreur: Constante inconnue!");
	}

	return returnType;
}

// Factor := "!" Factor | "(" Expression ")" | Identifier | Constant
TYPE Factor() {
	TYPE returnType;

	if (current == NOT) {
		unsigned long long tag = ++TagNumber;

		current = (TOKEN)lexer->yylex();

		TYPE factorType = Factor();

		std::cout << "\tcmpq $0, (%rsp)" 				<< std::endl;
		std::cout << "\tje Faux" << tag					<< std::endl;
		std::cout << "\tmovq $FFFFFFFFFFFFFFFF, %rax" 	<< std::endl;
		std::cout << "\tjmp Suite" << tag				<< std::endl;
		std::cout << "Faux" << tag << ":" 				<< std::endl;
		std::cout << "\tmovq $0, %rax" 					<< std::endl;
		std::cout << "Suite" << tag << ":" 				<< std::endl;
		std::cout << "\tnotq %rax" 						<< std::endl;
		std::cout << "\tpush %rax" 						<< std::endl;

		returnType = BOOLEAN;

	} else if (current == LPARENT) {
		current = (TOKEN)lexer->yylex();

		returnType = Expression();
		if (current != RPARENT) {
			Error("(Factor) Erreur: Caractère `)` attendu!");
		}

		current = (TOKEN)lexer->yylex();

	} else if (current == ID) {
		returnType = Identifier();
	} else {
		returnType = Constant();
	}

	return returnType;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term() {
	TYPE returnType = Factor();

	while (current == MULOP) {
		OPMUL mulop = MultiplicativeOperator();

		TYPE operandType = Factor();

		// Si au moins une opérande est DOUBLE, alors le type de l'expression est DOUBLE.
		// Sinon, le type de l'expression est UINTEGER.

		bool FPU = (returnType == DOUBLE || operandType == DOUBLE);

		// Opérande 2.
		if (operandType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfildl (%rsp)" << std::endl;
				std::cout << "\taddq $8, %rsp" << std::endl;
			} else {
				std::cout << "\tpop %rax" << std::endl;
			}
		}

		// Opérande 1.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfildl (%rsp)" << std::endl;
				std::cout << "\taddq $8, %rsp" << std::endl;
			} else {
				std::cout << "\tpop %rbx" << std::endl;
			}
		}

		unsigned long long tag = ++TagNumber;
		switch(mulop) {
			case AND:
				std::cout << "\tcmpq $0, %rax" 					<< std::endl;
				std::cout << "\tje Faux" << tag					<< std::endl;
				std::cout << "\tmovq $FFFFFFFFFFFFFFFF, %rax" 	<< std::endl;
				std::cout << "\tjmp Suite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq $0, %rax" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				tag = ++TagNumber;
				std::cout << "\tcmpq $0, %rbx" 					<< std::endl;
				std::cout << "\tje Faux" << tag					<< std::endl;
				std::cout << "\tmovq $FFFFFFFFFFFFFFFF, %rbx" 	<< std::endl;
				std::cout << "\tjmp Suite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq $0, %rbx" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				std::cout << "\tandq %rbx, %rax" << std::endl;  // %rax and %rbx => %rax
				std::cout << "\tpush %rax\t# AND" << std::endl;  // store result      ^
				break;
			case MUL:
				if (FPU) {
					std::cout << "\tfmulp %st(0), %st(1)\t# MUL" << std::endl;  // %st(0) * %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				} else {
					std::cout << "\tmulq %rbx" << std::endl;  // %rbx * %rax => %rdx:%rax
					std::cout << "\tpush %rax\t# MUL" << std::endl;  // store result:  ^
				}
				break;
			case DIV:
				if (FPU) {
					std::cout << "\tfdivp %st(0), %st(1)\t# DIV" << std::endl;  // %st(0) / %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				} else {
					std::cout << "\tmovq $0, %rdx" << std::endl;  // Partie haute du numérateur
					std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
					std::cout << "\tpush %rax\t# DIV" << std::endl;  // store result:   ^
				}
				break;
			case MOD:
				if (!IsIntegral(returnType)) {
					Error("(Term) Erreur: Le type de l'expression doit être entier! (" + typeString[returnType] + " lu)");
				}
				std::cout << "\tmovq $0, %rdx" << std::endl;  // Partie haute du numérateur
				std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
				std::cout << "\tpush %rdx\t# MOD" << std::endl;  // store result:          ^
				break;
			default:
				Error("(Term) Erreur: Opérateur multiplicatif inconnu!");
		}

		if (FPU) {
			returnType = DOUBLE;
		} else {
			returnType = UINTEGER;
		}
	}

	return returnType;
}

// SimpleExpression := Term {AdditiveOperator Term}
TYPE SimpleExpression(){
	TYPE returnType = Term();

	while(current == ADDOP) {
		OPADD adop = AdditiveOperator();

		TYPE operandType = Term();

		// Si au moins une opérande est DOUBLE, alors le type de l'expression est DOUBLE.
		// Sinon, le type de l'expression est UINTEGER.

		bool FPU = (returnType == DOUBLE || operandType == DOUBLE);

		// Opérande 2.
		if (operandType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfildl (%rsp)" << std::endl;
				std::cout << "\taddq $8, %rsp" << std::endl;
			} else {
				std::cout << "\tpop %rax" << std::endl;
			}
		}

		// Opérande 1.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfildl (%rsp)" << std::endl;
				std::cout << "\taddq $8, %rsp" << std::endl;
			} else {
				std::cout << "\tpop %rbx" << std::endl;
			}
		}

		unsigned long long tag = ++TagNumber;
		switch(adop) {
			case OR:
				std::cout << "\tcmpq $0, %rax" 					<< std::endl;
				std::cout << "\tje Faux" << tag					<< std::endl;
				std::cout << "\tmovq $FFFFFFFFFFFFFFFF, %rax" 	<< std::endl;
				std::cout << "\tjmp Suite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq $0, %rax" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				tag = ++TagNumber;
				std::cout << "\tcmpq $0, %rbx" 					<< std::endl;
				std::cout << "\tje Faux" << tag					<< std::endl;
				std::cout << "\tmovq $FFFFFFFFFFFFFFFF, %rbx" 	<< std::endl;
				std::cout << "\tjmp Suite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq $0, %rbx" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				std::cout << "\torq %rbx, %rax" << std::endl;  // %rax or %rbx => %rax
				std::cout << "\tpush %rax\t# OR" << std::endl;  // store result     ^
				break;
			case ADD:
				if (FPU) {
					std::cout << "\tfaddp %st(0), %st(1)\t# ADD" << std::endl;  // %st(0) + %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				} else {
					std::cout << "\taddq %rbx" << std::endl;  		 // %rbx + %rax => %rax
					std::cout << "\tpush %rax\t# ADD" << std::endl;  // store result:    ^
				}
				break;
			case SUB:
				if (FPU) {
					std::cout << "\tfsubp %st(0), %st(1)\t# DIV" << std::endl;  // %st(0) - %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;  // store result                        ^
				} else {
					std::cout << "\tsub %rbx, %rax" << std::endl;    // %rax - %rbx => %rax
					std::cout << "\tpush %rax\t# DIV" << std::endl;  // store result:    ^
				}
				break;
			default:
				Error("(SimpleExpression) Erreur: Opérateur additif inconnu!");
		}

		if (FPU) {
			returnType = DOUBLE;
		} else {
			returnType = UINTEGER;
		}
	}

	return returnType;
}

/// Produit le code pour calculer l'expression, et place le résultat au sommet de la pile.
// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPE Expression() {
	TYPE returnType = SimpleExpression();

	if (current == RELOP) {
		unsigned long long tag = ++TagNumber;

		OPREL oprel = RelationalOperator();

		TYPE operandType = SimpleExpression();

		// Si au moins une opérande est DOUBLE, alors le type de l'expression est DOUBLE.
		// Sinon, le type de l'expression est UINTEGER.

		bool FPU = (returnType == DOUBLE || operandType == DOUBLE);

		// Opérande 2.
		if (operandType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfildl (%rsp)" << std::endl;
				std::cout << "\taddq $8, %rsp" << std::endl;
			} else {
				std::cout << "\tpop %rax" << std::endl;
			}
		}

		// Opérande 1.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl (%rsp)" << std::endl;
			std::cout << "\taddq $8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfildl (%rsp)" << std::endl;
				std::cout << "\taddq $8, %rsp" << std::endl;
			} else {
				std::cout << "\tpop %rbx" << std::endl;
			}
		}

		if (FPU) {
			std::cout << "\tfcomi %st(1)" << std::endl;
		} else {
			std::cout << "\tcmpq %rax, %rbx" << std::endl;
		}

		switch (oprel) {
			case EQU:
				std::cout << "\tje Vrai" << tag << "\t# If equal" << std::endl;
				break;
			case DIFF:
				std::cout << "\tjne Vrai" << tag << "\t# If not equal" << std::endl;
				break;
			case SUPE:
				std::cout << "\tjae Vrai" << tag << "\t# If above or equal" << std::endl;
				break;
			case INFE:
				std::cout << "\tjbe Vrai" << tag << "\t# If below or equal" << std::endl;
				break;
			case INF:
				std::cout << "\tjb Vrai" << tag << "\t# If below" << std::endl;
				break;
			case SUP:
				std::cout << "\tja Vrai" << tag << "\t# If above" << std::endl;
				break;
			default:
				Error("(Expression) Erreur: Opérateur de comparaison inconnu!");
		}

		std::cout << "\tpush $0\t# False" << std::endl;
		std::cout << "\tjmp Suite" << tag << std::endl;
		std::cout << "Vrai" << tag << ":" << std::endl;
		std::cout << "\tpush $0xFFFFFFFFFFFFFFFF\t# True" << std::endl;	
		std::cout << "Suite" << tag << ":" << std::endl;

		return BOOLEAN;
	}

	return returnType;
}

// AssignmentStatement := Identifier ":=" Expression
void AssignmentStatement() {
	if (current != ID) {
		Error("(AssignmentStatement) Erreur: Identifiant attendu!");
	}
	if (!IsDeclared(lexer->YYText())) {
		Error("(AssignmentStatement) Erreur: Variable `" + std::string(lexer->YYText()) + "` non déclarée!");
	}

	std::string variable = lexer->YYText();  // nom de la variable
	TYPE variableType = DeclaredVariables.find({variable})->type;  // type de la variable
	current = (TOKEN)lexer->yylex();  // reconnaître ID

	if (current != ASSIGN) {
		Error("(AssignmentStatement) Erreur: Symbole `:=` attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":="

	TYPE exprType = Expression();  // reconnaître Expression

	if (variableType != exprType) {
		Error("(AssignmentStatement) Erreur: Types incompatibles (" + typeString[variableType] + " et " + typeString[exprType] + ")");
	}

	std::cout << "\tpop " << variable << "\t# Assign" << std::endl;
}

// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
void IfStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("IF");

	std::cout << "If" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != BOOLEAN) {
		Error("(IfStatement) Erreur: Type booléen attendu ! (" + typeString[exprType] + " lu)");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje Else" << tag << std::endl;

	ReadKeyword("THEN");

	Statement();  // reconnaître Statement

	std::cout << "\tjmp EndIf" << tag << std::endl;

	// ELSE optionnel (=> pas d'erreur)
	std::cout << "Else" << tag << ":" << std::endl;
	if (current == KEYWORD && strcmp("ELSE", lexer->YYText()) == 0) {
		current = (TOKEN)lexer->yylex();  // reconnaître ELSE
		Statement();  // reconnaître Statement
	}

	std::cout << "EndIf" << tag << ":" << std::endl;
}

// WhileStatement := "WHILE" Expression "DO" Statement
void WhileStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("WHILE");

	std::cout << "TestWhile" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != BOOLEAN) {
		Error("(WhileStatement) Erreur: Type booléen attendu ! (" + typeString[exprType] + " lu)");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje EndWhile" << tag << std::endl;

	ReadKeyword("DO");

	Statement();  // reconnaître Statement

	std::cout << "\tjmp TestWhile" << tag << std::endl;
	std::cout << "EndWhile" << tag << ":" << std::endl;
}

// ForStatement := "FOR" AssignmentStatement ("TO" | "DOWNTO") Expression "DO" Statement
void ForStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("FOR");

	std::string var = lexer->YYText();  // nom de la variable utilisée lors de l'incrémentation (prochain token)
	std::cout << "For" << tag << ":" << std::endl;

	AssignmentStatement();  // reconnaître AssignmentStatement

	if (current != KEYWORD) {
		Error("(ForStatement) Erreur: Mot clé attendu!");
	}
	if (strcmp("TO", lexer->YYText()) != 0 && strcmp("DOWNTO", lexer->YYText()) != 0) {
		Error("(ForStatement) Erreur: Mots clés `TO` ou `DOWNTO` attendus!");
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

	std::cout << "TestFor" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != UINTEGER) {
		Error("(ForStatement) Erreur: L'incrément doit être entier!");
	}

	std::cout << "\tpop %rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq %rax, " << var << std::endl;
	std::cout << jump << tag << std::endl;
	
	ReadKeyword("DO");

	Statement();  // reconnaître Statement

	std::cout << increment << var << std::endl;  // incrémenter l'entier
	std::cout << "\tjmp TestFor" << tag << std::endl;
	std::cout << "EndFor" << tag << ":" << std::endl;
}

// BlockStatement := "BEGIN" Statement {";" Statement} "END"
void BlockStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("BEGIN");

	std::cout << "Begin" << tag << ":" << std::endl;

	Statement();  // reconnaître Statement

	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();  // reconnaître ";"
		Statement();  // reconnaître Statement
	}

	ReadKeyword("END");

	std::cout << "End" << tag << ":" << std::endl;
}

// DisplayStatement := "DISPLAY" Expression
void DisplayStatement() {
	ReadKeyword("DISPLAY");

	TYPE exprType = Expression();  // reconnaître Expression

	switch (exprType) {
		case UINTEGER:
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
			Error("(DisplayStatement) Erreur: Type inconnu!");
	}
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
			Error("(CaseLabelList) Erreur: Les constantes doivent avoir le même type! (" + typeString[constType] + " lu)");
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

	TYPE labelType = CaseLabelList();  // reconnaître CaseLabelList

	if (current != COLON) {
		Error("(CaseListElement) Erreur: Symbole `:` attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	std::cout << "\tjmp Case" << tag + 1 << std::endl;
	std::cout << "Statement" << tag << ":" << std::endl;

	Statement();  // reconnaître Statement

	std::cout << "\tjmp EndCase" << endTagNumber << std::endl;

	// Cette étiquette doit correspondre avec le jump juste au dessus.
	std::cout << "Case" << tag + 1 << ":" << std::endl;

	return labelType;
}

// CaseStatement := "CASE" Expression "OF" CaseListElement {";" CaseListElement} [";" "ELSE" Statement] "END"
void CaseStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("CASE");

	std::cout << "Case" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (!IsIntegral(exprType)) {
		Error("(CaseStatement) Erreur: Expression de type entier attendue! (" + typeString[exprType] + " lu)");
	}

	ReadKeyword("OF");

	TYPE labelType = CaseListElement(tag);  // reconnaître CaseListElement

	if (exprType != labelType) {
		Error("(CaseStatement) Erreur: Types incompatibles: " + typeString[exprType] + " et " + typeString[labelType]);
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
			Error("(CaseStatement) Erreur: Types incompatibles: " + typeString[exprType] + " et " + typeString[labelType]);
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
			Error("(Statement) Erreur: Mot clé inconnu!");
		}
	} else {
		Error("(Statement) Erreur: Identifiant ou mot clé attendu!");
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
		Error("(StatementPart) Erreur: `.` attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître "."
}

// Type := "UINTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR"
TYPE Type() {
	if (current != KEYWORD) {
		Error("(Type) Erreur: Nom de type attendu!");
	}

	TYPE type;
	if (strcmp(lexer->YYText(), "UINTEGER") == 0) {
		type = UINTEGER;
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
		Error("(VarDeclaration) Erreur: Identifiant attendu!");
	}

	// Vérification
	if (IsDeclared(lexer->YYText())) {
		Error("(VarDeclaration) Erreur: Variable `" + std::string(lexer->YYText()) + "` déjà déclarée!");
	}
	variables.insert(lexer->YYText());
	current = (TOKEN)lexer->yylex();  // reconnaître Identifier

	while(current == COMMA) {
		current = (TOKEN)lexer->yylex();  // reconnaître ","

		// Vérification
		if (IsDeclared(lexer->YYText())) {
			Error("(VarDeclaration) Erreur: Variable `" + std::string(lexer->YYText()) + "` déjà déclarée!");
		}
		variables.insert(lexer->YYText());
		current = (TOKEN)lexer->yylex();  // reconnaître Identifier
	}

	if (current != COLON) {
		Error("(VarDeclaration) Erreur: Symbole `:` attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	TYPE varType = Type();  // reconnaître Type

	// 1. Déclarer et 2. Créer les variables lues.
	// Toutes les variables sont initialisées avec la valeur 0 (0.0, false).
	switch (varType) {
		case UINTEGER:
			// .quad.
			for (std::string variable : variables) {
				DeclaredVariables.insert({variable, UINTEGER});  // déclarer
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
			Error("(VarDeclaration) Erreur: Type inconnu!");
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
		Error("(DeclarationPart) Erreur: `.` attendu!");
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
	std::cout << "# This code was produced by VICTOR's Vcompiler for Vascal. <3" << std::endl;
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
		Error("(main) Erreur: Fin du programme attendue!!!");  // unexpected characters at the end of program
	}
	
	return 0;
}

/*

Notes:
	Expressions
		DOUBLE +,-,*,/ UINTEGER -> DOUBLE   #
		UINTEGER +,-,*,/ DOUBLE -> DOUBLE   # fild & fist
		DOUBLE +,-,*,/ CHAR -> DOUBLE       #
		CHAR +,-,*,/ DOUBLE -> DOUBLE       #
		DOUBLE +,-,*,/ BOOLEAN -> DOUBLE    #
		BOOLEAN +,-,*,/ DOUBLE -> DOUBLE    #
		UINTEGER +,-,*,/,% CHAR -> UINTEGER
		CHAR +,-,*,/,% UINTEGER -> UINTEGER
		UINTEGER +,-,*,/,% BOOLEAN -> UINTEGER
		BOOLEAN +,-,*,/,% UINTEGER -> UINTEGER
		BOOLEAN +,-,*,/,% CHAR -> UINTEGER
		CHAR +,-,*,/,% BOOLEAN -> UINTEGER
	Assignment
		UINTEGER = BOOLEAN -> UINTEGER
		UINTEGER = DOUBLE -> UINTEGER  # (#include <cmath> int y = (int)std::round(x);)
		UINTEGER = CHAR -> UINTEGER
		BOOLEAN = UINTEGER -> BOOLEAN  # cmp $0
		BOOLEAN = DOUBLE -> BOOLEAN    # cmp flottante avec $0
		BOOLEAN = CHAR -> BOOLEAN      # cmp $0
		DOUBLE = UINTEGER -> DOUBLE
		DOUBLE = BOOLEAN -> DOUBLE
		DOUBLE = CHAR -> DOUBLE
		CHAR = UINTEGER -> CHAR
		CHAR = BOOLEAN -> CHAR
		CHAR = DOUBLE -> CHAR  # (#include <cmath> int y = (int)std::round(x);)


Questions:

*/

/*

TP à rendre avant l'examen écrit. Voir date limite du dépôt.

Faire un petit texte qui précise les différences avec la version finale du prof.
Ce qui est commun avec la version du prof (ex. à partir du TP1, puis après c'est moi).
Lister, résumer ce qu'on a fait globalement. Pas trop de détails.
Attirer l'attention sur ce qu'on a fait de différent, de mieux, de particulier.

Boucle FOR fonctionnelle c'est la moyenne: 10/20.
Boucle FOR et CASE fonctionnels c'est une "bonne note".

Pour avoir 20
Propre
Avoir fait quelque chose qui n'a pas été enseigné. Aller au delà du cours. Attaquer des problèmes non abordés. Qui apporte quelque chose.
Ex: les procédures avec des paramètres.
Ex: les RECORDs (structures, types définis par l'utilisateur).

À IMPLÉMENTER:
- cast explicite 
- write
- fonctions
- struct
- scope (stocker distance par rapport au sommet de la pile | 8 * position dans le scope)
- stringconst (retirer les \n des FormatStrings)
- restructurer tout !!! (retirer commentaires inutiles, commenter fonctions)
- entiers signés

*/

/*

struct Function {
	std::string name;
	TYPE returnType;
	std::ordered_set?<Variable> args;
};

// FunctionDeclaration := "FUNCTION" Identifier "(" [ArgumentList {"," ArgumentList}] ")" ":" Type ? BlockStatement "."
// ArgumentList := Identifier {"," Identifier} ":" Type

// Statement +:= ReturnStatement
// ReturnStatement := "RETURN" Expression

// Factor +:= FunctionCall
// FunctionCall := Identifier "(" [Expression {"," Expression}] ")"					// parenthèses = function identifier



PROTOTYPE: FORWARD?






void FunctionStatement(void){
    if(current != KEYWORD && strcmp(lexer->YYText(), "FUNCTION") != 0) {
        Error("FUNCTION keyword attendu");
    }
    current = (TOKEN)lexer->yylex();
    if(current != ID){
        Error("Identificateur attendu");
    }
    string function_identifier = lexer->YYText();

    if(DeclaredFunctions.find(function_identifier) != DeclaredFunctions.end()){
        Error("Fonction déjà déclarée");
    }
    current = (TOKEN)lexer->yylex();

    Function function;

    if(current != LPARENT){
        Error("Caractère '(' attendu");
    }

    current = (TOKEN)lexer->yylex();

    map<string, enum TYPES> arguments = map<string, enum TYPES>();

	do {
        current = (TOKEN)lexer->yylex(); ???
        if(current != ID){
            Error("Identificateur attendu");
        }
        string argument_identifier = lexer->YYText();
        current = (TOKEN)lexer->yylex();
        if(current != COLON){
            Error("Caractère ':' attendu");
        }
        TYPES argument_type = Type();
        arguments[argument_identifier] = argument_type;
        current = (TOKEN)lexer->yylex();
    } while (current == COMMA);

    function.arguments = arguments;

    if(current != RPARENT){
        Error("Caractère ')' attendu");
    }

    current = (TOKEN)lexer->yylex();

    if(current != COLON){
        Error("Caractère ':' attendu");
    }

    current = (TOKEN)lexer->yylex();

    function.return_type = Type();

    DeclaredFunctions[function_identifier] = function;

    BlockStatement();
}

*/
