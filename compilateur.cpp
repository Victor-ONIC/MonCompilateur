#include <cstdlib>
#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include <vector>
#include <map>
#include <algorithm>
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

bool is_subroutine = false;
std::string currentSubroutine = "";



// Type du token actuellement sous la tête de lecture (id, keyword, ...).
TOKEN current;

// lexer->yylex() avance la tête de lecture et donne le type du token.
// lexer->YYText() donne le texte associé au token lu, sous forme d'un c-string.
FlexLexer* lexer = new yyFlexLexer;

// Valeur incrémentale qui permet d'avoir plusieurs étiquettes du même nom.
unsigned long long TagNumber = 0;



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
		std::cout << "\tpush %rax" << std::endl;
		return returnType;
	}

	if (is_subroutine) {
		std::vector<Variable> argumentsVector = DeclaredSubroutines[currentSubroutine].arguments;
		for (int i = 0; i < argumentsVector.size(); i++) {
			if (argumentsVector[i].name == id) {
				std::cout << "\tpush " << 16 + 8 * i << "(%rbp)" << std::endl;
				return argumentsVector[i].type;
			}
		}

		std::vector<Variable> localVector = DeclaredSubroutines[currentSubroutine].local;
		for (int i = 0; i < localVector.size(); i++) {
			if (localVector[i].name == id) {
				std::cout << "\tpush " << -8 - 8 * i << "(%rbp)" << std::endl;
				return localVector[i].type;
			}
		}
	}

	if (!IsVarDeclared(id)) {
		Error("(Identifier) Erreur: Variable non déclarée!");
	}
	std::cout << "\tpush " << id << std::endl;
	return DeclaredVariables.find({id})->type;
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

	current = (TOKEN)lexer->yylex();
	return type;
}

// Constant := Number | Boolean | Float | Character
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

	if (current != RPARENT) {
		unsigned long long i = 0;
		TYPE exprType;
		
		exprType = Expression();
		switch (DeclaredSubroutines[functionName].arguments[i++].type) {
			case UINTEGER:
			case BOOLEAN:
			case CHAR:
				if (exprType == DOUBLE) {
					// Arrondir à l'entier le plus proche.
					std::cout << "\tfldl (%rsp)" << std::endl;
					std::cout << "\tfistpq (%rsp)" << std::endl;
				}
				break;
			case DOUBLE:
				if (IsIntegral(exprType)) {
					// Convertir d'entier à flottant 64 bits.
					std::cout << "\tfild (%rsp)" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;
				}
				break;
		}
		while (current == COMMA) {
			current = (TOKEN)lexer->yylex();
			exprType = Expression();
			switch (DeclaredSubroutines[functionName].arguments[i++].type) {
				case UINTEGER:
				case BOOLEAN:
				case CHAR:
					if (exprType == DOUBLE) {
						// Arrondir à l'entier le plus proche.
						std::cout << "\tfldl (%rsp)" << std::endl;
						std::cout << "\tfistpq (%rsp)" << std::endl;
					}
					break;
				case DOUBLE:
					if (IsIntegral(exprType)) {
						// Convertir d'entier à flottant 64 bits.
						std::cout << "\tfild (%rsp)" << std::endl;
						std::cout << "\tfstpl (%rsp)" << std::endl;
					}
					break;
			}
		}
	}

	if (current != RPARENT) {
		Error("(FunctionCall) Erreur: Symbole `)` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	std::cout << "\tcall " << functionName << std::endl;
	std::cout << "\taddq $" << 8 * DeclaredSubroutines[functionName].arguments.size() << ", %rsp" << std::endl;

	return DeclaredSubroutines[functionName].returnType;
}

// ProcedureCall := Identifier "(" [Expression {"," Expression}] ")"
void ProcedureCall(std::string procedureName) {
	if (current != LPARENT) {
		Error("(ProcedureCall) Erreur: Symbole `(` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	if (current != RPARENT) {
		unsigned long long i = 0;
		TYPE exprType;
		
		exprType = Expression();
		switch (DeclaredSubroutines[procedureName].arguments[i++].type) {
			case UINTEGER:
			case BOOLEAN:
			case CHAR:
				if (exprType == DOUBLE) {
					// Arrondir à l'entier le plus proche.
					std::cout << "\tfldl (%rsp)" << std::endl;
					std::cout << "\tfistpq (%rsp)" << std::endl;
				}
				break;
			case DOUBLE:
				if (IsIntegral(exprType)) {
					// Convertir d'entier à flottant 64 bits.
					std::cout << "\tfild (%rsp)" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;
				}
				break;
		}
		while (current == COMMA) {
			current = (TOKEN)lexer->yylex();
			exprType = Expression();
			switch (DeclaredSubroutines[procedureName].arguments[i++].type) {
				case UINTEGER:
				case BOOLEAN:
				case CHAR:
					if (exprType == DOUBLE) {
						// Arrondir à l'entier le plus proche.
						std::cout << "\tfldl (%rsp)" << std::endl;
						std::cout << "\tfistpq (%rsp)" << std::endl;
					}
					break;
				case DOUBLE:
					if (IsIntegral(exprType)) {
						// Convertir d'entier à flottant 64 bits.
						std::cout << "\tfild (%rsp)" << std::endl;
						std::cout << "\tfstpl (%rsp)" << std::endl;
					}
					break;
			}
		}
	}

	if (current != RPARENT) {
		Error("(ProcedureCall) Erreur: Symbole `)` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	std::cout << "\tcall " << procedureName << std::endl;
	std::cout << "\taddq $" << 8 * DeclaredSubroutines[procedureName].arguments.size() << ", %rsp" << std::endl;
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
				std::cout << "\tpop %rbx" << std::endl;
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
				std::cout << "\tpop %rax" << std::endl;
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
				std::cout << "\tpush %rax" << std::endl;
				break;
			case MUL:
				if (FPU) {
					std::cout << "\tfmulp %st(0), %st(1)" << std::endl;  // %st(0) * %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;
				} else {
					std::cout << "\tmulq %rbx" << std::endl;  // %rbx * %rax => %rdx:%rax
					std::cout << "\tpush %rax" << std::endl;
				}
				break;
			case DIV:
				if (FPU) {
					std::cout << "\tfdivp %st(0), %st(1)" << std::endl;  // %st(0) / %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;
				} else {
					std::cout << "\tmovq $0, %rdx" << std::endl;  // Partie haute du numérateur
					std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
					std::cout << "\tpush %rax" << std::endl;
				}
				break;
			case MOD:
				if (!IsIntegral(returnType)) {
					Error("(Term) Erreur: Le type de l'expression doit être entier! (" + typeString[returnType] + " lu)");
				}
				std::cout << "\tmovq $0, %rdx" << std::endl;  // Partie haute du numérateur
				std::cout << "\tdiv %rbx" << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
				std::cout << "\tpush %rdx" << std::endl;
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
				std::cout << "\tpop %rbx" << std::endl;
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
				std::cout << "\tpop %rax" << std::endl;
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
				std::cout << "\tpush %rax" << std::endl;
				break;
			case ADD:
				if (FPU) {
					std::cout << "\tfaddp %st(0), %st(1)" << std::endl;  // %st(0) + %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;
				} else {
					std::cout << "\taddq %rbx, %rax" << std::endl;   // %rbx + %rax => %rax
					std::cout << "\tpush %rax" << std::endl;
				}
				break;
			case SUB:
				if (FPU) {
					std::cout << "\tfsubp %st(0), %st(1)" << std::endl;  // %st(0) - %st(1) => %st(1) puis pop
					std::cout << "\tsubq $8, %rsp" << std::endl;
					std::cout << "\tfstpl (%rsp)" << std::endl;
				} else {
					std::cout << "\tsub %rbx, %rax" << std::endl;    // %rax - %rbx => %rax
					std::cout << "\tpush %rax" << std::endl;
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

	// Appel de routine comme instruction.
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
			output = "\tpop %rax";
			type = DeclaredSubroutines[currentSubroutine].returnType;
		} else {
			// Paramètres.
			std::vector<Variable> argumentsVector = DeclaredSubroutines[currentSubroutine].arguments;
			for (int i = 0; i < argumentsVector.size(); i++) {
				if (argumentsVector[i].name == id) {
					found = true;
					output = "\tpop " + std::to_string(16 + 8 *i) + "(%rbp)";
					type = DeclaredSubroutines[currentSubroutine].returnType;
				}
			}
			// Variables locales.
			std::vector<Variable> localVector = DeclaredSubroutines[currentSubroutine].local;
			for (int i = 0; i < localVector.size(); i++) {
				if (localVector[i].name == id) {
					found = true;
					output = "\tpop " + std::to_string(-8 - 8 * i) + "(%rbp)";
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
		output = "\tpop " + id;
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
				std::cout << "\tfldl (%rsp)" << std::endl;
				std::cout << "\tfistpq (%rsp)" << std::endl;
			}
			break;
		case DOUBLE:
			if (IsIntegral(exprType)) {
				// Convertir d'entier à flottant 64 bits.
				std::cout << "\tfild (%rsp)" << std::endl;
				std::cout << "\tfstpl (%rsp)" << std::endl;
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

	TYPE exprType = Expression();
	if (exprType != BOOLEAN) {
		Error("(WhileStatement) Erreur: Expression booléenne attendue ! (" + typeString[exprType] + " lue)");
	}

	std::cout << "\tpop %rax" << std::endl;
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tje EndWhile" << tag << std::endl;

	ReadKeyword("DO");

	Statement();

	std::cout << "\tjmp TestWhile" << tag << std::endl;
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
	
	std::cout << "\tpop %rax" << std::endl;
	std::cout << "\tcmpq $0, %rax" << std::endl;
	std::cout << "\tjne Repeat" << tag << std::endl;
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

// CaseElement := CaseLabelList ":" Statement
// Paramètre - endTagNumber: tous les cas ont le même numéro d'étiquette pour la fin du CASE.
TYPE CaseElement(unsigned long long endTagNumber) {
	unsigned long long tag = TagNumber;  // numéro d'étiquette du cas actuel

	TYPE labelType = CaseLabelList();  // reconnaître CaseLabelList

	if (current != COLON) {
		Error("(CaseElement) Erreur: Symbole `:` attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	std::cout << "\tjmp Case" << tag + 1 << std::endl;
	std::cout << "Statement" << tag << ":" << std::endl;

	Statement();  // reconnaître Statement

	std::cout << "\tjmp EndCase" << endTagNumber << std::endl;

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
	std::cout << "\taddq $8, %rsp" << std::endl;
}

// Statement := AssignmentStatement | IfStatement | WhileStatement | RepeatStatement | ForStatement | BlockStatement | DisplayStatement | CaseStatement
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
				std::cout << variableName << ":\t.quad 0" << std::endl;  // créer
			}
			break;
		case BOOLEAN:
			// .byte OU .quad également, on n'est pas à ça près.
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, BOOLEAN});  // déclarer
				std::cout << variableName << ":\t.quad 0" << std::endl;  // créer
			}
			break;
		case DOUBLE:
			// .double
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, DOUBLE});  // déclarer
				std::cout << variableName << ":\t.double 0.0" << std::endl;  // créer
			}
			break;
		case CHAR:
			// .byte
			for (std::string variableName : variables) {
				DeclaredVariables.insert({variableName, CHAR});  // déclarer
				std::cout << variableName << ":\t.byte 0" << std::endl;  // créer
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
		std::cout << "\tpush %rbp" << std::endl;
		std::cout << "\tmovq %rsp, %rbp" << std::endl;

		unsigned long long size = DeclaredSubroutines[functionName].local.size();
		if (size > 0) {
			size *= 8;
			std::cout << "\tsubq $" << size << ", %rsp" << std::endl;
		}

		is_subroutine = true;
		currentSubroutine = functionName;
		BlockStatement();
		currentSubroutine = "";
		is_subroutine = false;

		DeclaredSubroutines[functionName].defined = true;

		std::cout << "\taddq $" << DeclaredSubroutines[functionName].local.size() * 8 << ", %rsp" << std::endl;
		std::cout << "\tpop %rbp" << std::endl;
		std::cout << "\tret" << std::endl;
	}
}

// FunctionSection := Function {";" Function}
void FunctionSection() {
	std::cout << std::endl;
	std::cout << "\t.text" << std::endl;
	Function();
	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();
		std::cout << std::endl;
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
		std::cout << "\tpush %rbp" << std::endl;
		std::cout << "\tmovq %rsp, %rbp" << std::endl;

		unsigned long long size = DeclaredSubroutines[procedureName].local.size();
		if (size > 0) {
			size *= 8;
			std::cout << "\tsubq $" << size << ", %rsp" << std::endl;
		}

		is_subroutine = true;
		currentSubroutine = procedureName;
		BlockStatement();
		currentSubroutine = "";
		is_subroutine = false;

		DeclaredSubroutines[procedureName].defined = true;

		std::cout << "\taddq $" << DeclaredSubroutines[procedureName].local.size() * 8 << ", %rsp" << std::endl;
		std::cout << "\tpop %rbp" << std::endl;
		std::cout << "\tret" << std::endl;
	}
}

// ProcedureSection := Procedure {";" Procedure}
void ProcedureSection() {
	std::cout << std::endl;
	std::cout << "\t.text" << std::endl;
	Procedure();
	while (current == SEMICOLON) {
		current = (TOKEN)lexer->yylex();
		std::cout << std::endl;
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

	// Header for gcc assembler / linker.
	std::cout << std::endl;
	std::cout << "\t.data" << std::endl;
	std::cout << "FormatStringLLU:\t.string \"%llu\\n\"" << std::endl;
	std::cout << "FormatStringF:\t.string \"%lf\\n\"" << std::endl;
	std::cout << "FormatStringC:\t.string \"%c\\n\"" << std::endl;
	std::cout << "TrueString:\t.string \"TRUE\\n\"" << std::endl;
	std::cout << "FalseString:\t.string \"FALSE\\n\"" << std::endl;

	if (current == KEYWORD && strcmp("VAR", lexer->YYText()) == 0) {
		VarSection();
		if (current != DOT) {
			Error("(Program) Erreur: Symbole `.` attendu!");
		}
		current = (TOKEN)lexer->yylex();
	}

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
	
	std::cout << std::endl;
	std::cout << "\t.text"           << std::endl;
	std::cout << "\t.globl main"     << std::endl;
	// std::cout << "\t.align 8"     << std::endl;
	std::cout << "main:"             << std::endl;
	std::cout << "\tmovq %rsp, %rbp" << std::endl;

	std::cout << std::endl;
	BlockStatement();
	std::cout << std::endl;

	if (current != DOT) {
		Error("(Program) Erreur: Symbole `.` attendu!");
	}
	current = (TOKEN)lexer->yylex();

	// Trailer for the gcc assembler / linker.
	std::cout << "\tmovq $0, %rax" << std::endl;  // valeur de retour de main égale à 0.
	std::cout << "\tmovq %rbp, %rsp" << std::endl;
	std::cout << "\tret" << std::endl;
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
- REPEAT
- stringconst (retirer les \n des FormatStrings)
- cast explicite

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
// Type := "UINTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR"
// Constant := Number | Boolean | Float | Character
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
// CaseLabelList := Constant {"," Constant}
// CaseElement := CaseLabelList ":" Statement
// CaseStatement := "CASE" Expression "OF" CaseElement {";" CaseElement} [";" "ELSE" Statement] "END"
// Statement := AssignmentStatement | IfStatement | WhileStatement | RepeatStatement | ForStatement | BlockStatement | DisplayStatement | CaseStatement
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
*/
