#include <iostream>
#include <cstring>
#include <string>
#include <sstream>
#include <set>
#include <vector>
#include <map>
#include <algorithm>
#include <FlexLexer.h>
#include "tokeniser.h"

enum OPREL { EQU, DIFF, INF, SUP, INFE, SUPE, WTFR };
enum OPADD { ADD, SUB, OR, WTFA };
enum OPMUL { MUL, DIV, MOD, AND, WTFM };
enum TYPE { UINTEGER, BOOLEAN, DOUBLE, CHAR, STRING, WTFT };

// Traduction de ENUM à STRING pour afficher des erreurs plus lisibles.
std::string tokenString[] = {
	"FEOF", "UNKNOWN", "NUMBER", "ID", "STRINGCONST", "LBRACKET", "RBRACKET",
	"LPARENT", "RPARENT", "COMMA", "SEMICOLON", "DOT", "ADDOP", "MULOP",
	"RELOP", "NOT", "ASSIGN", "KEYWORD", "COLON", "FLOATCONST", "CHARCONST"
};
std::string typeString[] = {
	"UINTEGER", "BOOLEAN", "DOUBLE", "CHAR", "STRING"
};



struct Variable {
	std::string name;
	TYPE type;
};

std::set<Variable> DeclaredVariables;

bool operator<(const Variable& left, const Variable& right) {
	return left.name < right.name;
}
bool operator==(const Variable& left, const Variable& right) {
	return left.name == right.name;
}

bool IsVarDeclared(std::string name) {
	return DeclaredVariables.find({name}) != DeclaredVariables.end();
}



struct Subroutine {
	bool defined;
	TYPE returnType;
	std::vector<Variable> arguments;
	std::vector<Variable> local;
};

std::map<std::string, Subroutine> DeclaredSubroutines;

// DÉCLARATION
bool IsSubroutineDeclared(std::string name) {
	return DeclaredSubroutines.find(name) != DeclaredSubroutines.end();
}
// DÉFINITION
bool IsSubroutineDefined(std::string name) {
	return IsSubroutineDeclared(name) && DeclaredSubroutines[name].defined;
}

bool IsArgument(std::string name, std::string functionName) {
	std::vector<Variable>& v = DeclaredSubroutines[functionName].arguments;
	Variable vv = {name};
	return std::find(v.begin(), v.end(), vv) != v.end();
}

bool is_subroutine;
std::string currentSubroutine;



// Type du token actuellement sous la tête de lecture (id, keyword, ...).
TOKEN current;

// lexer->yylex() avance la tête de lecture et donne le type du token.
// lexer->YYText() donne le texte associé au token lu, sous forme d'un c-string.
FlexLexer* lexer = new yyFlexLexer;

// Valeur incrémentale qui permet d'avoir plusieurs étiquettes du même nom.
unsigned long long TagNumber = 0;

// Stocker les directives de données internes (strings).
std::stringstream internalData;
// Pour chaque FormatString, doit-on l'afficher ?
std::map<std::string, bool> FS = {
	{"LLU", false}, {"F", false}, {"C", false}, {"B", false}, {"S", false},
	{"LLUln", false}, {"Fln", false}, {"Cln", false}, {"Bln", false}, {"Sln", false}
};


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
TYPE FunctionCall(std::string);  // To keep nice ordering (called by Identifier)

TYPE Identifier() {
	if (current != ID) {
		Error("(Identifier) Erreur: Identifiant attendu!");
	}
	std::string id = lexer->YYText();
	current = (TOKEN)lexer->yylex();

	// Appel de routine dans une expression.
	if (current == LPARENT) {
		if (!IsSubroutineDeclared(id)) {
			Error("(Identifier) Erreur: Routine non définie!");
		}
		if (DeclaredSubroutines[id].returnType == WTFT) {
			Error("(Identifier) Erreur: Procédure n'a pas de valeur de retour!");
		}
		TYPE returnType = FunctionCall(id);
		std::cout << "\tpushq\t%rax" << std::endl;
		return returnType;
	}

	if (is_subroutine) {
		std::vector<Variable> argumentsVector = DeclaredSubroutines[currentSubroutine].arguments;
		for (int i = 0; i < argumentsVector.size(); i++) {
			if (argumentsVector[i].name == id) {
				std::cout << "\tpushq\t" << 16 + 8 * i << "(%rbp)" << std::endl;
				return argumentsVector[i].type;
			}
		}

		std::vector<Variable> localVector = DeclaredSubroutines[currentSubroutine].local;
		for (int i = 0; i < localVector.size(); i++) {
			if (localVector[i].name == id) {
				std::cout << "\tpushq\t" << -8 - 8 * i << "(%rbp)" << std::endl;
				return localVector[i].type;
			}
		}
	}

	if (!IsVarDeclared(id)) {
		Error("(Identifier) Erreur: Variable non déclarée!");
	}
	std::cout << "\tpushq\t" << id << std::endl;
	return DeclaredVariables.find({id})->type;
}

TYPE Number() {
	if (current != NUMBER) {
		Error("(Number) Erreur: Nombre entier attendu!");
	}

	std::cout << "\tpushq\t$" << atoi(lexer->YYText()) << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le NUMBER

	return UINTEGER;
}

TYPE Boolean() {
	if (current != KEYWORD) {
		Error("(Boolean) Erreur: Mot clé `TRUE` ou `FALSE` attendu!");
	}

	if (strcmp("TRUE", lexer->YYText()) == 0) {
		std::cout << "\tpushq\t$0xFFFFFFFFFFFFFFFF" << std::endl;
	} else {
		std::cout << "\tpushq\t$0x0" << std::endl;
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
	std::cout << "\tmovq\t$" << *i << ", %rax" << std::endl;
	std::cout << "\tpushq\t%rax # Empile " << f << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le FLOATCONST

	return DOUBLE;
}

TYPE Character() {
	if (current != CHARCONST) {
		Error("(Character) Erreur: Constante caractère attendue!");
	}

	std::cout << "\tmovq\t$0, %rax" << std::endl;
	std::cout << "\tmovb\t$" << lexer->YYText() << ", %al" << std::endl;
	std::cout << "\tpushq\t%rax" << std::endl;

	current = (TOKEN)lexer->yylex();  // reconnaître le CHARCONST

	return CHAR;
}

TYPE String() {
	unsigned long long tag = ++TagNumber;

	if (current != STRINGCONST) {
		Error("(String) Erreur: Constante string attendue!");
	}

	internalData << "S" << tag << ":" << std::endl;
	internalData << "\t.string " << lexer->YYText() << std::endl;

	std::cout << "\tpushq\t$S" << tag << std::endl;

	current = (TOKEN)lexer->yylex();

	return STRING;
}

// Type := "UINTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR" | "STRING"
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
	} else if (strcmp("STRING", lexer->YYText()) == 0) {
		type = STRING;
	} else {
		type = WTFT;
	}

	current = (TOKEN)lexer->yylex();
	return type;
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

// FunctionCall := Identifier "(" [Expression {"," Expression}] ")"
TYPE FunctionCall(std::string functionName) {
	if (current != LPARENT) {
		Error("(FunctionCall) Erreur: Symbole `(` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	unsigned long long count = 0;
	if (current != RPARENT) {
		TYPE exprType;
		
		exprType = Expression();
		switch (DeclaredSubroutines[functionName].arguments[count++].type) {
			case UINTEGER:
			case BOOLEAN:
			case CHAR:
				if (exprType == DOUBLE) {
					// Arrondir à l'entier le plus proche.
					std::cout << "\tfldl\t(%rsp)" << std::endl;
					std::cout << "\tfistpq\t(%rsp)" << std::endl;
				}
				break;
			case DOUBLE:
				if (IsIntegral(exprType)) {
					// Convertir d'entier à flottant 64 bits.
					std::cout << "\tfild\t(%rsp)" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				}
				break;
		}
		if (count > DeclaredSubroutines[functionName].arguments.size()) {
			Error("(FunctionCall) Erreur: Trop d'arguments dans l'appel de `" + functionName + "`!");
		}

		while (current == COMMA) {
			current = (TOKEN)lexer->yylex();
			exprType = Expression();
			switch (DeclaredSubroutines[functionName].arguments[count++].type) {
				case UINTEGER:
				case BOOLEAN:
				case CHAR:
					if (exprType == DOUBLE) {
						// Arrondir à l'entier le plus proche.
						std::cout << "\tfldl\t(%rsp)" << std::endl;
						std::cout << "\tfistpq\t(%rsp)" << std::endl;
					}
					break;
				case DOUBLE:
					if (IsIntegral(exprType)) {
						// Convertir d'entier à flottant 64 bits.
						std::cout << "\tfild\t(%rsp)" << std::endl;
						std::cout << "\tfstpl\t(%rsp)" << std::endl;
					}
					break;
			}
			if (count > DeclaredSubroutines[functionName].arguments.size()) {
				Error("(FunctionCall) Erreur: Trop d'arguments dans l'appel de `" + functionName + "`!");
			}
		}
	}
	if (count < DeclaredSubroutines[functionName].arguments.size()) {
		Error("(FunctionCall) Erreur: Trop peu d'arguments dans l'appel de `" + functionName + "`!");
	}

	if (current != RPARENT) {
		Error("(FunctionCall) Erreur: Symbole `)` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	std::cout << "\tcall\t" << functionName << std::endl;
	std::cout << "\taddq\t$" << 8 * DeclaredSubroutines[functionName].arguments.size() << ", %rsp" << std::endl;

	return DeclaredSubroutines[functionName].returnType;
}

// ProcedureCall := Identifier "(" [Expression {"," Expression}] ")"
void ProcedureCall(std::string procedureName) {
	if (current != LPARENT) {
		Error("(ProcedureCall) Erreur: Symbole `(` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	unsigned long long count = 0;
	if (current != RPARENT) {
		TYPE exprType;
		
		exprType = Expression();
		switch (DeclaredSubroutines[procedureName].arguments[count++].type) {
			case UINTEGER:
			case BOOLEAN:
			case CHAR:
				if (exprType == DOUBLE) {
					// Arrondir à l'entier le plus proche.
					std::cout << "\tfldl\t(%rsp)" << std::endl;
					std::cout << "\tfistpq\t(%rsp)" << std::endl;
				}
				break;
			case DOUBLE:
				if (IsIntegral(exprType)) {
					// Convertir d'entier à flottant 64 bits.
					std::cout << "\tfild\t(%rsp)" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				}
				break;
		}
		if (count > DeclaredSubroutines[procedureName].arguments.size()) {
			Error("(ProcedureCall) Erreur: Trop d'arguments dans l'appel de `" + procedureName + "`!");
		}

		while (current == COMMA) {
			current = (TOKEN)lexer->yylex();
			exprType = Expression();
			switch (DeclaredSubroutines[procedureName].arguments[count++].type) {
				case UINTEGER:
				case BOOLEAN:
				case CHAR:
					if (exprType == DOUBLE) {
						// Arrondir à l'entier le plus proche.
						std::cout << "\tfldl\t(%rsp)" << std::endl;
						std::cout << "\tfistpq\t(%rsp)" << std::endl;
					}
					break;
				case DOUBLE:
					if (IsIntegral(exprType)) {
						// Convertir d'entier à flottant 64 bits.
						std::cout << "\tfild\t(%rsp)" << std::endl;
						std::cout << "\tfstpl\t(%rsp)" << std::endl;
					}
					break;
			}
			if (count > DeclaredSubroutines[procedureName].arguments.size()) {
				Error("(ProcedureCall) Erreur: Trop d'arguments dans l'appel de `" + procedureName + "`!");
			}
		}
	}
	if (count < DeclaredSubroutines[procedureName].arguments.size()) {
		Error("(ProcedureCall) Erreur: Trop peu d'arguments dans l'appel de `" + procedureName + "`!");
	}

	if (current != RPARENT) {
		Error("(ProcedureCall) Erreur: Symbole `)` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	std::cout << "\tcall\t" << procedureName << std::endl;
	std::cout << "\taddq\t$" << 8 * DeclaredSubroutines[procedureName].arguments.size() << ", %rsp" << std::endl;
}

// Factor := "!" Factor | "(" Expression ")" | Identifier | Constant
TYPE Factor() {
	TYPE returnType;

	if (current == NOT) {
		unsigned long long tag = ++TagNumber;

		current = (TOKEN)lexer->yylex();

		TYPE factorType = Factor();

		std::cout << "\tcmpq\t$0, (%rsp)" 			   << std::endl;
		std::cout << "\tje\tFaux" << tag               << std::endl;
		std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rax" << std::endl;
		std::cout << "\tjmp\tSuite" << tag			   << std::endl;
		std::cout << "Faux" << tag << ":" 			   << std::endl;
		std::cout << "\tmovq\t$0, %rax"                << std::endl;
		std::cout << "Suite" << tag << ":" 			   << std::endl;
		std::cout << "\tnotq\t%rax" 	               << std::endl;
		std::cout << "\tpushq\t%rax" 	               << std::endl;

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
	current = (TOKEN)lexer->yylex();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term() {
	TYPE returnType = Factor();

	while (current == MULOP) {
		OPMUL mulop = MultiplicativeOperator();

		TYPE operandType = Factor();

		if (operandType == STRING) {
			Error("(Term) Opération sur string impossible!");
		}

		// Si au moins une opérande est DOUBLE, alors le type de l'expression est DOUBLE.
		// Sinon, le type de l'expression est UINTEGER.

		bool FPU = (returnType == DOUBLE || operandType == DOUBLE);

		// Opérande 2.
		if (operandType == DOUBLE) {
			std::cout << "\tfldl\t(%rsp)" << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)" << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rbx" << std::endl;
			}
		}

		// Opérande 1.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl\t(%rsp)" << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)" << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rax" << std::endl;
			}
		}

		unsigned long long tag = ++TagNumber;
		switch(mulop) {
			case AND:
				std::cout << "\tcmpq\t$0, %rax" 					<< std::endl;
				std::cout << "\tje\tFaux" << tag					<< std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rax" 	<< std::endl;
				std::cout << "\tjmp\tSuite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq\t$0, %rax" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				tag = ++TagNumber;
				std::cout << "\tcmpq\t$0, %rbx" 					<< std::endl;
				std::cout << "\tje\tFaux" << tag					<< std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rbx" 	<< std::endl;
				std::cout << "\tjmp\tSuite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq\t$0, %rbx" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				std::cout << "\tandq\t%rbx, %rax" << std::endl;  // %rax and %rbx => %rax
				std::cout << "\tpushq\t%rax" << std::endl;
				break;
			case MUL:
				if (FPU) {
					std::cout << "\tfmulp\t%st(0), %st(1)" << std::endl;  // %st(0) * %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				} else {
					std::cout << "\tmulq\t%rbx" << std::endl;  // %rbx * %rax => %rdx:%rax
					std::cout << "\tpushq\t%rax" << std::endl;
				}
				break;
			case DIV:
				if (FPU) {
					std::cout << "\tfdivp\t%st(0), %st(1)" << std::endl;  // %st(0) / %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				} else {
					std::cout << "\tmovq\t$0, %rdx" << std::endl;  // Partie haute du numérateur
					std::cout << "\tdiv\t%rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
					std::cout << "\tpushq\t%rax" << std::endl;
				}
				break;
			case MOD:
				if (!IsIntegral(returnType)) {
					Error("(Term) Erreur: Le type de l'expression doit être entier! (" + typeString[returnType] + " lu)");
				}
				std::cout << "\tmovq\t$0, %rdx" << std::endl;  // Partie haute du numérateur
				std::cout << "\tdiv\t%rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
				std::cout << "\tpushq\t%rdx" << std::endl;
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

// SimpleExpression := Term {AdditiveOperator Term}
TYPE SimpleExpression(){
	TYPE returnType = Term();

	while(current == ADDOP) {
		OPADD adop = AdditiveOperator();

		TYPE operandType = Term();

		bool concat = false;
		if (returnType == STRING || operandType == STRING) {
			if (returnType != operandType || adop != ADD) {
				Error("(SimpleExpression) Erreur: Opération sur string impossible!");
			}
			// Deux string, addition -> OK
			concat = true;
		}  // TODO opérandes, opération de concaténation

		// Si au moins une opérande est DOUBLE, alors le type de l'expression est DOUBLE.
		// Sinon, le type de l'expression est UINTEGER.

		bool FPU = (returnType == DOUBLE || operandType == DOUBLE);

		// Opérande 2.
		if (operandType == DOUBLE) {
			std::cout << "\tfldl\t(%rsp)" << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)" << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rbx" << std::endl;
			}
		}

		// Opérande 1.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl\t(%rsp)" << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)" << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rax" << std::endl;
			}
		}

		unsigned long long tag = ++TagNumber;
		switch(adop) {
			case OR:
				std::cout << "\tcmpq\t$0, %rax" 					<< std::endl;
				std::cout << "\tje\tFaux" << tag					<< std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rax" 	<< std::endl;
				std::cout << "\tjmp\tSuite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq\t$0, %rax" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				tag = ++TagNumber;
				std::cout << "\tcmpq\t$0, %rbx" 					<< std::endl;
				std::cout << "\tje\tFaux" << tag					<< std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rbx" 	<< std::endl;
				std::cout << "\tjmp\tSuite" << tag				<< std::endl;
				std::cout << "Faux" << tag << ":" 				<< std::endl;
				std::cout << "\tmovq\t$0, %rbx" 					<< std::endl;
				std::cout << "Suite" << tag << ":" 				<< std::endl;

				std::cout << "\torq\t%rbx, %rax" << std::endl;  // %rax or %rbx => %rax
				std::cout << "\tpushq\t%rax" << std::endl;
				break;
			case ADD:
				if (concat) {

				}
				if (FPU) {
					std::cout << "\tfaddp\t%st(0), %st(1)" << std::endl;  // %st(0) + %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				} else {
					std::cout << "\taddq\t%rbx, %rax" << std::endl;   // %rbx + %rax => %rax
					std::cout << "\tpushq\t%rax" << std::endl;
				}
				break;
			case SUB:
				if (FPU) {
					std::cout << "\tfsubp\t%st(0), %st(1)" << std::endl;  // %st(0) - %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				} else {
					std::cout << "\tsubq\t%rbx, %rax" << std::endl;    // %rax - %rbx => %rax
					std::cout << "\tpushq\t%rax" << std::endl;
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

/// Produit le code pour calculer l'expression, et place le résultat au sommet de la pile.
// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPE Expression() {
	TYPE returnType = SimpleExpression();

	if (current == RELOP) {
		unsigned long long tag = ++TagNumber;

		OPREL oprel = RelationalOperator();

		TYPE operandType = SimpleExpression();

		if (operandType == STRING) {
			Error("(Expression) Erreur: Opération sur string impossible!");
		}

		// Si au moins une opérande est DOUBLE, alors le type de l'expression est DOUBLE.
		// Sinon, le type de l'expression est UINTEGER.

		bool FPU = (returnType == DOUBLE || operandType == DOUBLE);

		// Opérande 2.
		if (operandType == DOUBLE) {
			std::cout << "\tfldl\t(%rsp)" << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)" << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rax" << std::endl;
			}
		}

		// Opérande 1.
		if (returnType == DOUBLE) {
			std::cout << "\tfldl\t(%rsp)" << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)" << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rbx" << std::endl;
			}
		}

		if (FPU) {
			std::cout << "\tfcomi\t%st(1)" << std::endl;
		} else {
			std::cout << "\tcmpq\t%rax, %rbx" << std::endl;
		}

		switch (oprel) {
			case EQU:
				std::cout << "\tje\tVrai" << tag << "\t# If equal" << std::endl;
				break;
			case DIFF:
				std::cout << "\tjne\tVrai" << tag << "\t# If not equal" << std::endl;
				break;
			case SUPE:
				std::cout << "\tjae\tVrai" << tag << "\t# If above or equal" << std::endl;
				break;
			case INFE:
				std::cout << "\tjbe\tVrai" << tag << "\t# If below or equal" << std::endl;
				break;
			case INF:
				std::cout << "\tjb\tVrai" << tag << "\t# If below" << std::endl;
				break;
			case SUP:
				std::cout << "\tja\tVrai" << tag << "\t# If above" << std::endl;
				break;
			default:
				Error("(Expression) Erreur: Opérateur de comparaison inconnu!");
		}

		std::cout << "\tpushq\t$0\t# False" << std::endl;
		std::cout << "\tjmp\tSuite" << tag << std::endl;
		std::cout << "Vrai" << tag << ":" << std::endl;
		std::cout << "\tpushq\t$0xFFFFFFFFFFFFFFFF\t# True" << std::endl;	
		std::cout << "Suite" << tag << ":" << std::endl;

		return BOOLEAN;
	}

	return returnType;
}

// BlockStatement := "BEGIN" Statement {";" Statement} "END"
void BlockStatement() {
	ReadKeyword("BEGIN");
	Statement();
	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();
		Statement();
	}
	ReadKeyword("END");
}

// AssignmentStatement := Identifier ":=" Expression
void AssignmentStatement() {
	if (current != ID) {
		Error("(AssignmentStatement) Erreur: Identifiant attendu!");
	}
	std::string id = lexer->YYText();
	current = (TOKEN)lexer->yylex();

	// Appel de routine comme instruction après avoir lu un identifiant.
	if (current == LPARENT) {
		if (!IsSubroutineDeclared(id)) {
			Error("(AssignmentStatement) Erreur: Routine non déclarée!");
		}
		if (DeclaredSubroutines[id].returnType == WTFT) {
			ProcedureCall(id);
		} else {
			FunctionCall(id);
		}
		return;
	}

	TYPE type;
	bool found = false;
	std::string output;
	if (is_subroutine) {
		if (currentSubroutine == id && DeclaredSubroutines[currentSubroutine].returnType != WTFT) {
			// Cas spécial: assignement de la valeur de retour d'une fonction (pas de procédures donc).
			found = true;
			output = "\tpopq\t%rax";
			type = DeclaredSubroutines[currentSubroutine].returnType;
		} else {
			// Paramètres.
			std::vector<Variable> argumentsVector = DeclaredSubroutines[currentSubroutine].arguments;
			for (int i = 0; i < argumentsVector.size(); i++) {
				if (argumentsVector[i].name == id) {
					found = true;
					output = "\tpopq\t" + std::to_string(16 + 8 *i) + "(%rbp)";
					type = DeclaredSubroutines[currentSubroutine].returnType;
				}
			}
			// Variables locales.
			std::vector<Variable> localVector = DeclaredSubroutines[currentSubroutine].local;
			for (int i = 0; i < localVector.size(); i++) {
				if (localVector[i].name == id) {
					found = true;
					output = "\tpopq\t" + std::to_string(-8 - 8 * i) + "(%rbp)";
					type = localVector[i].type;
				}
			}
		}
	}
	if (!found) {
		// Variables globales.
		if (!IsVarDeclared(id)) {
			Error("(AssignmentStatement) Erreur: Variable `" + id + "` non déclarée!");
		}
		type = DeclaredVariables.find({id})->type;
		output = "\tpopq\t" + id;
	}

	if (current != ASSIGN) {
		Error("(AssignmentStatement) Erreur: Symbole `:=` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	TYPE exprType = Expression();
	switch (type) {
		case UINTEGER:
		case BOOLEAN:
		case CHAR:
			if (exprType == DOUBLE) {
				// Arrondir à l'entier le plus proche.
				std::cout << "\tfldl\t(%rsp)" << std::endl;
				std::cout << "\tfistpq\t(%rsp)" << std::endl;
			}
			break;
		case DOUBLE:
			if (IsIntegral(exprType)) {
				// Convertir d'entier à flottant 64 bits.
				std::cout << "\tfild\t(%rsp)" << std::endl;
				std::cout << "\tfstpl\t(%rsp)" << std::endl;
			}
		case STRING:
			if (exprType != STRING) {
				Error("(AssignmentStatement) Erreur: Impossible d'assigner le type `" + typeString[exprType] + "` au type `STRING`!");
			}
			break;
	}

	std::cout << output << std::endl;
}

// IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
void IfStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("IF");

	std::cout << "If" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != BOOLEAN) {
		Error("(IfStatement) Erreur: Type booléen attendu ! (" + typeString[exprType] + " lu)");
	}

	std::cout << "\tpopq\t%rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq\t$0, %rax" << std::endl;
	std::cout << "\tje\tElse" << tag << std::endl;

	ReadKeyword("THEN");

	Statement();  // reconnaître Statement

	std::cout << "\tjmp\tEndIf" << tag << std::endl;

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

	TYPE exprType = Expression();
	if (exprType != BOOLEAN) {
		Error("(WhileStatement) Erreur: Expression booléenne attendue ! (" + typeString[exprType] + " lue)");
	}

	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t$0, %rax" << std::endl;
	std::cout << "\tje\tEndWhile" << tag << std::endl;

	ReadKeyword("DO");

	Statement();

	std::cout << "\tjmp\tTestWhile" << tag << std::endl;
	std::cout << "EndWhile" << tag << ":" << std::endl;
}

// RepeatStatement := "REPEAT" Statement "UNTIL" Expression
void RepeatStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("REPEAT");
	
	std::cout << "Repeat" << tag << ":" << std::endl;

	Statement();

	ReadKeyword("UNTIL");

	TYPE exprType = Expression();
	if (exprType != BOOLEAN) {
		Error("(RepeatStatement) Erreur: Expression booléenne attendue ! (" + typeString[exprType] + " lue)");
	}

	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t$0, %rax" << std::endl;
	std::cout << "\tjne\tRepeat" << tag << std::endl;
}

// ForStatement := "FOR" AssignmentStatement ("TO" | "DOWNTO") Expression "DO" Statement
void ForStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("FOR");

	bool found = false;
	std::string position;
	std::string id = lexer->YYText();  // nom de la variable utilisée lors de l'incrémentation (prochain token)
	if (is_subroutine) {
		// arguments
		std::vector<Variable> argumentsVector = DeclaredSubroutines[currentSubroutine].arguments;
		for (int i = 0; i < argumentsVector.size(); i++) {
			if (argumentsVector[i].name == id) {
				position = std::to_string(16 + 8 * i) + "(%rbp)";
				found = true;
			}
		}
		// varibales locales
		std::vector<Variable> localVector = DeclaredSubroutines[currentSubroutine].local;
		for (int i = 0; i < localVector.size(); i++) {
			if (localVector[i].name == id) {
				position = std::to_string(-8 - 8 * i) + "(%rbp)";
				found = true;
			}
		}
	}
	if (!found) {
		// varibales globales
		position = id;
	}

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
		jump = "\tja\tEndFor";
		increment = "\taddq\t$1, ";
	} else {
		jump = "\tjb\tEndFor";
		increment = "\tsubq\t$1, ";
	}
	current = (TOKEN)lexer->yylex();

	std::cout << "TestFor" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (exprType != UINTEGER) {
		Error("(ForStatement) Erreur: L'incrément doit être entier!");
	}

	std::cout << "\tpopq\t%rax" << std::endl;  // %rax contient le résultat
	std::cout << "\tcmpq\t%rax, " << position << std::endl;
	std::cout << jump << tag << std::endl;
	
	ReadKeyword("DO");

	Statement();  // reconnaître Statement

	std::cout << increment << position << std::endl;  // incrémenter l'entier
	std::cout << "\tjmp\tTestFor" << tag << std::endl;
	std::cout << "EndFor" << tag << ":" << std::endl;
}

// DisplayStatement := "DISPLAY" Expression
void DisplayStatement() {
	ReadKeyword("DISPLAY");

	TYPE exprType = Expression();  // reconnaître Expression

	switch (exprType) {
		case UINTEGER:
			if (!FS["LLU"]) {
				FS["LLU"] = true;
				internalData << "FSLLU:\n\t.string \"%llu\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSLLU, %rsi"     << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case BOOLEAN: {
			if (!FS["B"]) {
				FS["B"] = true;
				internalData << "FSTRUE:\n\t.string \"TRUE\"" << std::endl;
				internalData << "FSFALSE:\n\t.string \"FALSE\"" << std::endl;
			}
			unsigned long long tag = ++TagNumber;
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tcmpq\t$0, %rdx"         << std::endl;
			std::cout << "\tje\tFalse" << tag       << std::endl;
			std::cout << "\tmovq\t$FSTRUE, %rsi"    << std::endl;
			std::cout << "\tjmp\tSuite" << tag      << std::endl;
			std::cout << "False" << tag << ":"      << std::endl;
			std::cout << "\tmovq\t$FSFALSE, %rsi"   << std::endl;
			std::cout << "Suite" << tag << ":"      << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		}
		case DOUBLE:
			if (!FS["F"]) {
				FS["F"] = true;
				internalData << "FSF:\n\t.string \"%lf\"" << std::endl;
			}
			std::cout << "\tmovsd\t(%rsp), %xmm0"   << std::endl;
			std::cout << "\tmovq\t$FSF, %rsi"       << std::endl;
			std::cout << "\tmovl\t$1, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			std::cout << "\taddq\t$8, %rsp"         << std::endl;
			break;
		case CHAR:
			if (!FS["C"]) {
				FS["C"] = true;
				internalData << "FSC:\n\t.string \"%c\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSC, %rsi"       << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case STRING:
			if (!FS["S"]) {
				FS["S"] = true;
				internalData << "FSS:\n\t.string \"%s\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSS, %rsi"       << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		default:
			Error("(DisplayStatement) Erreur: Type inconnu!");
	}
}

// DisplaylnStatement := "DISPLAYLN" Expression
void DisplaylnStatement() {
	ReadKeyword("DISPLAYLN");

	TYPE exprType = Expression();  // reconnaître Expression

	switch (exprType) {
		case UINTEGER:
			if (!FS["LLUln"]) {
				FS["LLUln"] = true;
				internalData << "FSLLUln:\n\t.string \"%llu\\n\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSLLUln, %rsi"   << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case BOOLEAN: {
			if (!FS["Bln"]) {
				FS["Bln"] = true;
				internalData << "FSTRUEln:\n\t.string \"TRUE\\n\"" << std::endl;
				internalData << "FSFALSEln:\n\t.string \"FALSE\\n\"" << std::endl;
			}
			unsigned long long tag = ++TagNumber;
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tcmpq\t$0, %rdx"         << std::endl;
			std::cout << "\tje\tFalse" << tag       << std::endl;
			std::cout << "\tmovq\t$FSTRUEln, %rsi"  << std::endl;
			std::cout << "\tjmp\tSuite" << tag      << std::endl;
			std::cout << "False" << tag << ":"      << std::endl;
			std::cout << "\tmovq\t$FSFALSEln, %rsi" << std::endl;
			std::cout << "Suite" << tag << ":"      << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		}
		case DOUBLE:
			if (!FS["Fln"]) {
				FS["Fln"] = true;
				internalData << "FSFln:\n\t.string \"%lf\\n\"" << std::endl;
			}
			std::cout << "\tmovsd\t(%rsp), %xmm0"   << std::endl;
			std::cout << "\tmovq\t$FSFln, %rsi"     << std::endl;
			std::cout << "\tmovl\t$1, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			std::cout << "\taddq\t$8, %rsp"         << std::endl;
			break;
		case CHAR:
			if (!FS["Cln"]) {
				FS["Cln"] = true;
				internalData << "FSCln:\n\t.string \"%c\\n\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSCln, %rsi"     << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case STRING:
			if (!FS["Sln"]) {
				FS["Sln"] = true;
				internalData << "FSSln:\n\t.string \"%s\\n\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSSln, %rsi"     << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		default:
			Error("(DisplayStatement) Erreur: Type inconnu!");
	}
}

// CaseLabelList := Constant {"," Constant}
TYPE CaseLabelList() {
	unsigned long long tag = TagNumber;  // numéro d'étiquette du Case actuel

	TYPE returnType = Constant();  // reconnaître la constante

	if (returnType == STRING) {
		Error("(CaseLabelList) Erreur: Constante ne peut pas être string!");
	}

	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t(%rsp), %rax" << std::endl;
	std::cout << "\tje\tStatement" << tag << std::endl;

	while (current == COMMA) {
		current = (TOKEN)lexer->yylex();  // reconnaître ","
		TYPE constType = Constant();  // reconnaître la constante

		if (returnType != constType) {
			Error("(CaseLabelList) Erreur: Les constantes doivent avoir le même type! (" + typeString[constType] + " lu)");
		}

		std::cout << "\tpopq\t%rax" << std::endl;
		std::cout << "\tcmpq\t(%rsp), %rax" << std::endl;
		std::cout << "\tje\tStatement" << tag << std::endl;
	}

	return returnType;
}

// CaseElement := CaseLabelList ":" Statement
// Paramètre - endTagNumber: tous les cas ont le même numéro d'étiquette pour la fin du CASE.
TYPE CaseElement(unsigned long long endTagNumber) {
	unsigned long long tag = TagNumber;  // numéro d'étiquette du cas actuel

	TYPE labelType = CaseLabelList();  // reconnaître CaseLabelList

	if (current != COLON) {
		Error("(CaseElement) Erreur: Symbole `:` attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	std::cout << "\tjmp\tCase" << tag + 1 << std::endl;
	std::cout << "Statement" << tag << ":" << std::endl;

	Statement();  // reconnaître Statement

	std::cout << "\tjmp\tEndCase" << endTagNumber << std::endl;

	// Cette étiquette doit correspondre avec le jump juste au dessus.
	std::cout << "Case" << tag + 1 << ":" << std::endl;
	TagNumber++;

	return labelType;
}

// CaseStatement := "CASE" Expression "OF" CaseElement {";" CaseElement} [";" "ELSE" Statement] "END"
void CaseStatement() {
	unsigned long long tag = ++TagNumber;

	ReadKeyword("CASE");

	std::cout << "Case" << tag << ":" << std::endl;

	TYPE exprType = Expression();  // reconnaître Expression
	if (!IsIntegral(exprType)) {
		Error("(CaseStatement) Erreur: Expression de type entier attendue! (" + typeString[exprType] + " lu)");
	}

	ReadKeyword("OF");

	TYPE labelType = CaseElement(tag);  // reconnaître CaseElement

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
	
		TYPE labelType = CaseElement(tag);  // reconnaître CaseElement

		if (exprType != labelType) {
			Error("(CaseStatement) Erreur: Types incompatibles: " + typeString[exprType] + " et " + typeString[labelType]);
		}
	}

	ReadKeyword("END");

	std::cout << "EndCase" << tag << ":" << std::endl;
	std::cout << "\taddq\t$8, %rsp" << std::endl;
}

// Statement := AssignmentStatement | IfStatement | WhileStatement | RepeatStatement | ForStatement | BlockStatement | DisplayStatement | DisplaylnStatement | CaseStatement
void Statement() {
	if (current == ID) {
		AssignmentStatement();
	} else if (current == KEYWORD) {
		if (strcmp("IF", lexer->YYText()) == 0) {
			IfStatement();
		} else if (strcmp("WHILE", lexer->YYText()) == 0) {
			WhileStatement();
		} else if (strcmp("REPEAT", lexer->YYText()) == 0) {
			RepeatStatement();
		} else if (strcmp("FOR", lexer->YYText()) == 0) {
			ForStatement();
		} else if (strcmp("BEGIN", lexer->YYText()) == 0) {
			BlockStatement();
		} else if (strcmp("DISPLAY", lexer->YYText()) == 0) {
			DisplayStatement();
		} else if (strcmp("DISPLAYLN", lexer->YYText()) == 0) {
			DisplaylnStatement();
		} else if (strcmp("CASE", lexer->YYText()) == 0) {
			CaseStatement();
		} else {
			Error("(Statement) Erreur: Instruction inconnue!");
		}
	} else {
		Error("(Statement) Erreur: Identifiant ou mot clé attendu!");
	}
}

// VarDeclaration := Identifier {"," Identifier} ":" Type
void VarDeclaration() {
	if (current != ID) {
		Error("(VarDeclaration) Erreur: Identifiant attendu!");
	}

	// Ensemble de noms des variables déclarées ici.
	std::set<std::string> variables;

	// Vérification
	if (IsVarDeclared(lexer->YYText())) {
		Error("(VarDeclaration) Erreur: Variable `" + std::string(lexer->YYText()) + "` déjà déclarée!");
	}
	variables.insert(lexer->YYText());
	current = (TOKEN)lexer->yylex();  // reconnaître Identifier

	while(current == COMMA) {
		current = (TOKEN)lexer->yylex();  // reconnaître ","

		// Vérification
		if (IsVarDeclared(lexer->YYText())) {
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
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, UINTEGER});  // déclarer
				std::cout << variableName << ":\n\t.quad 0" << std::endl;  // créer
			}
			break;
		case BOOLEAN:
			// .byte OU .quad également, on n'est pas à ça près.
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, BOOLEAN});
				std::cout << variableName << ":\n\t.quad 0" << std::endl;
			}
			break;
		case DOUBLE:
			// .double
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, DOUBLE});
				std::cout << variableName << ":\n\t.double 0.0" << std::endl;
			}
			break;
		case CHAR:
			// .byte
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, CHAR});
				std::cout << variableName << ":\n\t.byte 0" << std::endl;
			}
			break;
		case STRING:
			// .quad adresse
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, STRING});
				std::cout << variableName << ":\n\t.quad 0" << std::endl;
			}
			break;
		default:
			Error("(VarDeclaration) Erreur: Type inconnu!");
	}
}

// VarSection := "VAR" VarDeclaration {";" VarDeclaration}
void VarSection() {
	ReadKeyword("VAR");
	VarDeclaration();
	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();
		VarDeclaration();
	}
}

// ArgumentList := Identifier {"," Identifier} ":" Type
void ArgumentList(std::string functionName, std::vector<Variable>& arguments) {
	if (current != ID) {
		Error("(ArgumentList) Erreur: Identifiant attendu!");
	}

	std::set<std::string> liste;

	if (liste.find(lexer->YYText()) != liste.end()) {
		Error("(ArgumentList) Erreur: Argument `" + std::string(lexer->YYText()) + "` déjà déclaré!");
	} else if (lexer->YYText() == functionName) {
		Error("(ArgumentList) Erreur: Un argument ne peut pas avoir le même nom que sa fonction!");
	}
	liste.insert(lexer->YYText());
	current = (TOKEN)lexer->yylex();

	while(current == COMMA) {
		current = (TOKEN)lexer->yylex();
		if (liste.find(lexer->YYText()) != liste.end()) {
			Error("(ArgumentList) Erreur: Argument `" + std::string(lexer->YYText()) + "` déjà déclaré!");
		} else if (lexer->YYText() == functionName) {
			Error("(ArgumentList) Erreur: Un argument ne peut pas avoir le même nom que sa fonction!");
		}
		liste.insert(lexer->YYText());
		current = (TOKEN)lexer->yylex();
	}

	if (current != COLON) {
		Error("(ArgumentList) Erreur: Symbole `:` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	TYPE varType = Type();
	if (varType == WTFT) {
		Error("(ArgumentList) Erreur: Type inconnu!");
	}
	for (std::string variableName : liste) {
		arguments.push_back({variableName, varType});
	}
}

// LocalVarDeclaration := Identifier {"," Identifier} ":" TYPE
void LocalVarDeclaration(std::string functionName) {
	if (current != ID) {
		Error("(LocalVarDeclaration) Erreur: Identifiant attendu!");
	}

	std::vector<Variable>& v = DeclaredSubroutines[functionName].arguments;
	std::set<std::string> liste;

	std::string id = lexer->YYText();
	if (liste.find(id) != liste.end()) {
		Error("(LocalVarDeclaration) Erreur: Argument `" + id + "` déjà déclaré!");
	} else if (id == functionName) {
		Error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom que sa fonction!");
	} else if (IsArgument(id, functionName)) {
		Error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom qu'un argument!");
	}
	liste.insert(id);
	current = (TOKEN)lexer->yylex();

	while(current == COMMA) {
		current = (TOKEN)lexer->yylex();
		id = lexer->YYText();
		if (liste.find(id) != liste.end()) {
			Error("(LocalVarDeclaration) Erreur: Argument `" + id + "` déjà déclaré!");
		} else if (id == functionName) {
			Error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom que sa fonction!");
		} else if (IsArgument(id, functionName)) {
			Error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom qu'un argument!");
		}
		liste.insert(id);
		current = (TOKEN)lexer->yylex();
	}

	if (current != COLON) {
		Error("(ArgumentList) Erreur: Symbole `:` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	TYPE varType = Type();
	if (varType == WTFT) {
		Error("(ArgumentList) Erreur: Type inconnu!");
	}
	for (std::string varName : liste) {
		DeclaredSubroutines[functionName].local.push_back({varName, varType});
	}
}

// LocalVarSection := "VAR" LocalVarDeclaration {";" LocalVarDeclaration}
void LocalVarSection(std::string functionName) {
	ReadKeyword("VAR");
	LocalVarDeclaration(functionName);
	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();
		LocalVarDeclaration(functionName);
	}
}

// Function := "FUNCTION" Identifier "(" [ArgumentList {";" ArgumentList}] ")" ":" Type [[LocalVarSection] BlockStatement]
void Function() {
	ReadKeyword("FUNCTION");

	if (current != ID) {
		Error("(Function) Erreur: Identifiant attendu!");
	}
	std::string functionName = lexer->YYText();
	current = (TOKEN)lexer->yylex();

	if (current != LPARENT) {
		Error("(Function) Erreur: Symbole `(` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	std::vector<Variable> arguments;  // on ne touche pas à la struct car il pourrait déjà y avoir une déclaration
	if (current == ID) {
		ArgumentList(functionName, arguments);
		while (current == SEMICOLON) {
			current = (TOKEN)lexer->yylex();
			ArgumentList(functionName, arguments);
		}
	}
	std::reverse(arguments.begin(), arguments.end());

	if (current != RPARENT) {
		Error("(Function) Erreur: Symbole `)` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	if (current != COLON) {
		Error("(Function) Erreur: Symbole `:` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	TYPE returnType = Type();
	if (returnType == WTFT) {
		Error("(Function) Erreur: Type de retour inconnu!");
	}

	if (IsSubroutineDeclared(functionName)) {
		if (arguments != DeclaredSubroutines[functionName].arguments || returnType != DeclaredSubroutines[functionName].returnType) {
			Error("(Function) Erreur: Déclaration de la fonction `" + functionName + "` incompatible avec une précédente déclaration!");
		}
	} else {
		Subroutine S = {
			false,
			returnType,
			arguments
		};
		DeclaredSubroutines[functionName] = S;
	}

	if (current == KEYWORD) {
		if(IsSubroutineDefined(functionName)) {
			Error("(Function) Erreur: Fonction `" + functionName + "` déjà définie!");
		}

		if (strcmp("VAR", lexer->YYText()) == 0) {
			LocalVarSection(functionName);
		}

		std::cout << functionName << ":" << std::endl;
		std::cout << "\tpushq\t%rbp" << std::endl;
		std::cout << "\tmovq\t%rsp, %rbp" << std::endl;

		unsigned long long size = DeclaredSubroutines[functionName].local.size();
		if (size > 0) {
			size *= 8;
			std::cout << "\tsubq\t$" << size << ", %rsp" << std::endl;
		}

		is_subroutine = true;
		currentSubroutine = functionName;
		BlockStatement();
		currentSubroutine = "";
		is_subroutine = false;

		DeclaredSubroutines[functionName].defined = true;

		if (DeclaredSubroutines[functionName].local.size() != 0) {
			std::cout << "\taddq\t$" << DeclaredSubroutines[functionName].local.size() * 8 << ", %rsp" << std::endl;
		}
		std::cout << "\tpopq\t%rbp" << std::endl;
		std::cout << "\tret" << std::endl;
	}
}

// FunctionSection := Function {";" Function}
void FunctionSection() {
	Function();
	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();
		Function();
	}
}

// Procedure := "PROCEDURE" Identifier "(" [ArgumentList {";" ArgumentList}] ")" [[LocalVarSection] BlockStatement]
void Procedure() {
	ReadKeyword("PROCEDURE");

	if (current != ID) {
		Error("(Procedure) Erreur: Identifiant attendu!");
	}
	std::string procedureName = lexer->YYText();
	current = (TOKEN)lexer->yylex();

	if (current != LPARENT) {
		Error("(Procedure) Erreur: Symbole `(` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	std::vector<Variable> arguments;  // on ne touche pas à la struct car il pourrait déjà y avoir une déclaration
	if (current == ID) {
		ArgumentList(procedureName, arguments);
		while (current == SEMICOLON) {
			current = (TOKEN)lexer->yylex();
			ArgumentList(procedureName, arguments);
		}
	}
	std::reverse(arguments.begin(), arguments.end());

	if (current != RPARENT) {
		Error("(Procedure) Erreur: Symbole `)` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	if (IsSubroutineDeclared(procedureName)) {
		if (arguments != DeclaredSubroutines[procedureName].arguments) {
			Error("(Procedure) Erreur: Déclaration de la procédure `" + procedureName + "` incompatible avec une précédente déclaration!");
		}
	} else {
		Subroutine S = {
			false,
			WTFT,
			arguments
		};
		DeclaredSubroutines[procedureName] = S;
	}

	if (current == KEYWORD) {
		if(IsSubroutineDefined(procedureName)) {
			Error("(Procedure) Erreur: Procédure `" + procedureName + "` déjà définie!");
		}

		if (strcmp("VAR", lexer->YYText()) == 0) {
			LocalVarSection(procedureName);
		}

		std::cout << procedureName << ":" << std::endl;
		std::cout << "\tpushq\t%rbp" << std::endl;
		std::cout << "\tmovq\t%rsp, %rbp" << std::endl;

		unsigned long long size = DeclaredSubroutines[procedureName].local.size();
		if (size > 0) {
			size *= 8;
			std::cout << "\tsubq\t$" << size << ", %rsp" << std::endl;
		}

		is_subroutine = true;
		currentSubroutine = procedureName;
		BlockStatement();
		currentSubroutine = "";
		is_subroutine = false;

		DeclaredSubroutines[procedureName].defined = true;

		std::cout << "\taddq\t$" << DeclaredSubroutines[procedureName].local.size() * 8 << ", %rsp" << std::endl;
		std::cout << "\tpopq\t%rbp" << std::endl;
		std::cout << "\tret" << std::endl;
	}
}

// ProcedureSection := Procedure {";" Procedure}
void ProcedureSection() {
	Procedure();
	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();
		Procedure();
	}
}

// Program := "PROGRAM" Identifier ";" [VarSection "."] [FunctionSection "."] [ProcedureSection "."] BlockStatement "."
void Program() {
	std::cout << "# This code was produced by VICTOR's Vcompiler for Vascal. <3" << std::endl;

	ReadKeyword("PROGRAM");

	if (current != ID) {
		Error("(Program) Erreur: Identifiant attendu!");
	}
	current = (TOKEN)lexer->yylex();

	if (current != SEMICOLON) {
		Error("(Program) Erreur: Symbole `;` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	if (current == KEYWORD && strcmp("VAR", lexer->YYText()) == 0) {
		std::cout << "\t.data" << std::endl;
		VarSection();
		if (current != DOT) {
			Error("(Program) Erreur: Symbole `.` attendu!");
		}
		current = (TOKEN)lexer->yylex();
	}

	std::cout << "\t.text" << std::endl;

	if (current == KEYWORD && strcmp("FUNCTION", lexer->YYText()) == 0) {
		FunctionSection();
		if (current != DOT) {
			Error("(Program) Erreur: Symbole `.` attendu!");
		}
		current = (TOKEN)lexer->yylex();
	}

	if (current == KEYWORD && strcmp("PROCEDURE", lexer->YYText()) == 0) {
		ProcedureSection();
		if (current != DOT) {
			Error("(Program) Erreur: Symbole `.` attendu!");
		}
		current = (TOKEN)lexer->yylex();
	}
	
	std::cout << "\t.globl main"     << std::endl;
	// std::cout << "\t.align 8"     << std::endl;
	std::cout << "main:"             << std::endl;
	std::cout << "\tmovq\t%rsp, %rbp" << std::endl;

	BlockStatement();

	if (current != DOT) {
		Error("(Program) Erreur: Symbole `.` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	// Trailer for the gcc assembler / linker.
	std::cout << "\tmovq\t$0, %rax" << std::endl;  // valeur de retour de main égale à 0.
	std::cout << "\tmovq\t%rbp, %rsp" << std::endl;
	std::cout << "\tret" << std::endl;

	if (!internalData.str().empty()) {
		std::cout << "\t.data" << std::endl;
		std::cout << internalData.str();
	}
}

/// First version: Source code on standard input and assembly code on standard output.
int main() {
	current = (TOKEN) lexer->yylex();  // déplacer la tête de lecture sur le premier token
	Program();
	if (current != FEOF) {
		Error("(main) Erreur: Fin du programme attendue!");
	}
	return 0;
}

// Consignes
/*
TP à rendre avant l'examen écrit. Voir date limite du dépôt.

Faire un petit texte qui précise les différences avec la version finale du prof.
Ce qui est commun avec la version du prof (ex. à partir du TP1, puis après c'est moi).
Puis lister ce qu'on a rajouté.
Lister, résumer ce qu'on a fait globalement. Pas trop de détails.
Attirer l'attention sur ce qu'on a fait de différent, de mieux, de particulier.

Boucle FOR fonctionnelle c'est la moyenne: 10/20.
Boucle FOR et CASE fonctionnels c'est une "bonne note".

Pour avoir 20
Propre
Avoir fait quelque chose qui n'a pas été enseigné. Aller au delà du cours. Attaquer des problèmes non abordés. Qui apporte quelque chose.
Ex: les procédures avec des paramètres.
Ex: les RECORDs (structures, types définis par l'utilisateur).
*/

// À IMPLÉMENTER
/*
-? DeclaredVariables as a map
- stringconst - concat (string & char)
- cast explicite
- return statement has to be last ? store value maybe ?

- void Warning(std::string s);  // conversions, fonctions vides, blocks vides...
	- conversions implicites
	- pas de valeur de retour
	- fonctions vides
	- blocks vides
	- case vide
- write + Error supprime le fichier
- struct = record
- restructurer tout !!! (retirer commentaires et erreurs inutiles, commenter les fonctions d'analyse elles-mêmes, REFERENCES là où c'est mieux)
- entiers signés
*/

// Nouvelle grammaire
/*
// cf tokeniser.l
//    Identifier
//    Number
//    Boolean
//    Float
//    Character
//    String
// Type := "UINTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR" | "STRING"
// Constant := Number | Boolean | Float | Character | String
// FunctionCall := Identifier "(" [Expression {"," Expression}] ")"
// ProcedureCall := Identifier "(" [Expression {"," Expression}] ")"
// Factor := "!" Factor | "(" Expression ")" | Identifier | Constant
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// Term := Factor {MultiplicativeOperator Factor}
// AdditiveOperator := "+" | "-" | "||".
// SimpleExpression := Term {AdditiveOperator Term}
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Expression := SimpleExpression [RelationalOperator SimpleExpression]
//
// BlockStatement := "BEGIN" Statement {";" Statement} "END"
// AssignmentStatement := Identifier ":=" Expression
// IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
// WhileStatement := "WHILE" Expression "DO" Statement
// RepeatStatement := "REPEAT" Statement "UNTIL" Expression
// ForStatement := "FOR" AssignmentStatement ("TO" | "DOWNTO") Expression "DO" Statement
// DisplayStatement := "DISPLAY" Expression
// DisplaylnStatement := "DISPLAYLN" Expression
// CaseLabelList := Constant {"," Constant}
// CaseElement := CaseLabelList ":" Statement
// CaseStatement := "CASE" Expression "OF" CaseElement {";" CaseElement} [";" "ELSE" Statement] "END"
// Statement := AssignmentStatement | IfStatement | WhileStatement | RepeatStatement | ForStatement | BlockStatement | DisplayStatement | DisplaylnStatement | CaseStatement
//
// VarDeclaration := Identifier {"," Identifier} ":" Type
// VarSection := "VAR" VarDeclaration {";" VarDeclaration}
// ArgumentList := Identifier {"," Identifier} ":" Type
// LocalVarDeclaration := Identifier {"," Identifier} ":" TYPE
// LocalVarSection := "VAR" LocalVarDeclaration {";" LocalVarDeclaration}
// Function := "FUNCTION" Identifier "(" [ArgumentList {";" ArgumentList}] ")" ":" Type [[LocalVarSection] BlockStatement]
// FunctionSection := Function {";" Function}
// Procedure := "PROCEDURE" Identifier "(" [ArgumentList {";" ArgumentList}] ")" [[LocalVarSection] BlockStatement]
// ProcedureSection := Procedure {";" Procedure}
// Program := "PROGRAM" Identifier ";" [VarSection "."] [FunctionSection "."] [ProcedureSection "."] BlockStatement "."
*/

// STRING
/*
.data
.align 8
format_string_int:    .string "%llu\n"
format_string_float:    .string "%f\n"
format_string_char:    .string "%c\n"
format_string_string:    .string "%s\n"
internal_string_0:    .string "hello"
internal_string_1:    .string " world"

s1:    .quad 0
s2:    .quad 0
s3:    .quad 0

.text
.globl main

main:
    movq %rsp, %rbp
    push $internal_string_0
    pop s1
    push $internal_string_1
    pop s2
    push s1
    push s2
    movq 16(%rsp), %rdi
    call strlen
    push %rax
    movq 16(%rsp), %rdi
    call strlen
    push %rax
    pop %rbx
    pop %rax
    addq %rbx, %rax
    addq $1, %rax
    movq %rax, %rdi
    call malloc
    push %rax
    mov (%rsp), %rdi
    mov 16(%rsp), %rsi
    call strcat
    mov (%rsp), %rdi
    mov 8(%rsp), %rsi
    call strcat
    pop %rax
    subq $16, %rsp
    push %rax
    pop s3
    push s3
    movq $format_string_string, %rdi
    movq (%rsp), %rsi
    movl    $0, %eax
    call    printf@PLT
    addq    $8, %rsp
    movq %rbp, %rsp        # Restore the position of the stack's top
    mov $0, %eax
    ret
*/

// README
/*
Instructions SSE découvertes et c'est franchement pas mal!
On différencie char et string par simple et double quotes.
Les format strings ont un \n à la fin car l'instruction DISPLAY sert à afficher une valeur en particulier, donc sur sa propre ligne.
Les variables globales sont automatiquement initialisées à 0.
On ne peut pas avoir de strings vides.
*/

// TESTS
/*
Un grand fichier de test avec un exemple de chaque fonctionnalité.
Un répertoire de fichiers de tests individuels avec plusieurs exemples pour chaque fonctionnalité, testant tous les cas imaginables.

1. Expressions
	Addition
	Soustraction
	Or
	Multiplication
	Division
	Modulo
	And
---> opérande 1 de chaque type, opérande 2 de chaque type, résultat de l'expression de chaque type

2. Instructions
	Block
	Assignment
	If
	While
	Repeat
	For
	Display
	Case

3. Subroutines
	Functions
		With(out) arguments
			1. & 2.
		With(out) local variables
			1. & 2.
	Procedures
		With(out) arguments
			1. & 2.
		With(out) local variables
			1. & 2.

4. Sections
	(Var)
	(Function)
	(Procedure)
	Main

5. Erreurs & Warnings

*/
