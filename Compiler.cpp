#include "Compiler.h"

void Compiler::run() {
    Program();
	if (current_token != Token::FEOF) {
		error("(run) Erreur: Fin du programme attendue!");
	}
}

Compiler::Compiler()
    : in_subroutine(false), current_subroutine(""), tag_number(0)
{
    lexer = new yyFlexLexer();
    read();
}

Compiler::~Compiler() {
    delete lexer;
}

Variable& Compiler::variable(const std::string& var_name) {
    return *std::find(variables.begin(), variables.end(), Variable(var_name));
}
const Variable& Compiler::variable(const std::string& var_name) const {
    return *std::find(variables.begin(), variables.end(), Variable(var_name));
}

Subroutine& Compiler::subroutine(const std::string& subroutine_name) {
    return *std::find(subroutines.begin(), subroutines.end(), Subroutine(subroutine_name));
}
const Subroutine& Compiler::subroutine(const std::string& subroutine_name) const {
    return *std::find(subroutines.begin(), subroutines.end(), Subroutine(subroutine_name));
}

void Compiler::error(const std::string& message) const {
	std::cerr << "Ligne n°" << lexer->lineno() << ", lu: `" << lexer->YYText() << "` (" << TOKEN_STRING[(int)current_token] << ")." << std::endl;
	std::cerr << message << std::endl;
	exit(-1);
}

void Compiler::read() {
    current_token = (Token)lexer->yylex();
}

void Compiler::read_keyword(const std::string& keyword) {
	if (current_token != Token::KEYWORD) {
		error("Erreur: Mot clé attendu!");
	}
    if (strcmp(keyword.c_str(), lexer->YYText()) != 0) {
        error("Erreur: Mot clé " + keyword + " attendu!");
    }
    read();
}

bool Compiler::is_integral(const Type& type) const {
    return type == Type::UINTEGER || type == Type::BOOLEAN || type == Type::CHAR;
}

bool Compiler::is_var_declared(const std::string& var_name) const {
    return std::find(variables.begin(), variables.end(), var_name) != variables.end();
}

bool Compiler::is_subroutine_declared(const std::string& subroutine_name) const {
    return std::find(subroutines.begin(), subroutines.end(), subroutine_name) != subroutines.end();
}

bool Compiler::is_subroutine_defined(const std::string& subroutine_name) const {
    return is_subroutine_declared(subroutine_name) && subroutine(subroutine_name).get_defined();
}

bool Compiler::is_argument(const std::string& var_name, const std::string& subroutine_name) const {
    if (!is_subroutine_declared(subroutine_name)) return false;
    const std::vector<Variable>& args = subroutine(subroutine_name).get_args();
    return std::find(args.begin(), args.end(), var_name) != args.end();
}



Type Compiler::Identifier() {
    if (current_token != Token::ID) {
        error("(Identifier) Erreur: Identifiant attendu!");
    }
    const std::string id = lexer->YYText();
    read();

    // Appel de routine dans une expression.
    if (current_token == Token::LPARENT) {
        if (!is_subroutine_declared(id)) {
            error("(Identifier) Erreur: Routine non définie!");
        }
        if (subroutine(id).get_return_type() == Type::WTFT) {
            error("(Identifier) Erreur: Procédure n'a pas de valeur de retour!");
        }
        Type return_type = FunctionCall(id);
		std::cout << "\tpushq\t%rax" << std::endl;
        return return_type;
    }

    if (in_subroutine) {
        const std::vector<Variable>& args = subroutine(current_subroutine).get_args();
        for (int i = 0; i < args.size(); i++) {
            if (args[i].get_name() == id) {
				std::cout << "\tpushq\t" << 16 + 8 * i << "(%rbp)" << std::endl;
                return args[i].get_type();
            }
        }

        const std::vector<Variable>& local = subroutine(current_subroutine).get_local();
        for (int i = 0; i < local.size(); i++) {
            if (local[i].get_name() == id) {
				std::cout << "\tpushq\t" << -8 - 8 * i << "(%rbp)" << std::endl;
                return local[i].get_type();
            }
        }
    }

    if (!is_var_declared(id)) {
        error("(Identifier) Variable non déclarée!");
    }
	std::cout << "\tpushq\t" << id << std::endl;
    return variable(id).get_type();
}

Type Compiler::Number() {
    if (current_token != Token::NUMBER) {
        error("(Number) Erreur: Nombre entier attendu!");
    }
	std::cout << "\tpushq\t$" << atoi(lexer->YYText()) << std::endl;
    read();
    return Type::UINTEGER;
}

Type Compiler::Boolean() {
    if (current_token != Token::KEYWORD) {
        error("(Boolean) Erreur: Mot clé `TRUE` ou `FALSE` attendu!");
    }
    if (strcmp("TRUE", lexer->YYText()) != 0) {
        std::cout << "\tpushq\t$0xFFFFFFFFFFFFFFFF" << std::endl;
    } else {
        std::cout << "\tpushq\t$0x0" << std::endl;
    }
    read();
    return Type::BOOLEAN;
}

Type Compiler::Float() {
    if (current_token != Token::FLOATCONST) {
        error("(Float) Erreur: Constante flottante attendue!");
    }
    double f = atof(lexer->YYText());
    unsigned long long* i = (unsigned long long*)&f;
	std::cout << "\tmovq\t$" << *i << ", %rax" << std::endl;
	std::cout << "\tpushq\t%rax # Empile " << f << std::endl;
    read();
    return Type::DOUBLE;
}

Type Compiler::Character() {
    if (current_token != Token::CHARCONST) {
        error("(Character) Erreur: Constante caractère attendue!");
    }
	std::cout << "\tmovq\t$0, %rax" << std::endl;
	std::cout << "\tmovb\t$" << lexer->YYText() << ", %al" << std::endl;
	std::cout << "\tpushq\t%rax" << std::endl;
    read();
    return Type::CHAR;
}

Type Compiler::String() {
    unsigned long long tag = ++tag_number;
    if (current_token != Token::STRINGCONST) {
        error("(String) Erreur: Constante string attendue!");
    }
	internal_data << "S" << tag << ":" << std::endl;
	internal_data << "\t.string " << lexer->YYText() << std::endl;
	std::cout << "\tpushq\t$S" << tag << std::endl;
    read();
    return Type::STRING;
}

// Typename := "UINTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR" | "STRING"
Type Compiler::Typename() {
    if (current_token != Token::KEYWORD) {
        error("(Type) Erreur: Nom de type attendu!");
    }
    Type type;
	if (strcmp("UINTEGER", lexer->YYText()) == 0) {
		type = Type::UINTEGER;
	} else if (strcmp("BOOLEAN", lexer->YYText()) == 0) {
		type = Type::BOOLEAN;
	} else if (strcmp("DOUBLE", lexer->YYText()) == 0) {
		type = Type::DOUBLE;
	} else if (strcmp("CHAR", lexer->YYText()) == 0) {
		type = Type::CHAR;
	} else if (strcmp("STRING", lexer->YYText()) == 0) {
		type = Type::STRING;
	} else {
		type = Type::WTFT;
	}
    read();
    return type;
}

// Constant := Number | Boolean | Float | Character | String
Type Compiler::Constant() {
    Type return_type;
	switch (current_token) {
		case Token::NUMBER:
			return_type = Number();
			break;
		case Token::KEYWORD:
			return_type = Boolean();
			break;
		case Token::FLOATCONST:
			return_type = Float();
			break;
		case Token::CHARCONST:
			return_type = Character();
			break;
		case Token::STRINGCONST:
			return_type = String();
			break;
		default:
			error("(Constant) Erreur: Constante inconnue!");
	}
    return return_type;
}

// FunctionCall := Identifier "(" [Expression {"," Expression}] ")"
Type Compiler::FunctionCall(const std::string& function_name) {
	if (current_token != Token::LPARENT) {
		error("(FunctionCall) Erreur: Symbole `(` attendu!");
	}
	read();

    const std::vector<Variable>& args = subroutine(function_name).get_args();
    std::size_t args_number = args.size();
	unsigned long long count = 0;
	if (current_token != Token::RPARENT) {
		Type expr_type;
		
		expr_type = Expression();
		switch (args[count++].get_type()) {
			case Type::UINTEGER:
			case Type::BOOLEAN:
			case Type::CHAR:
				if (expr_type == Type::DOUBLE) {
					// Arrondir à l'entier le plus proche.
					std::cout << "\tfldl\t(%rsp)" << std::endl;
					std::cout << "\tfistpq\t(%rsp)" << std::endl;
				}
				break;
			case Type::DOUBLE:
				if (is_integral(expr_type)) {
					// Convertir d'entier à flottant 64 bits.
					std::cout << "\tfild\t(%rsp)" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				}
				break;
		}
		if (count > args_number) {
			error("(FunctionCall) Erreur: Trop d'arguments dans l'appel de `" + function_name + "`!");
		}

		while (current_token == Token::COMMA) {
			read();
			expr_type = Expression();
			switch (args[count++].get_type()) {
				case Type::UINTEGER:
				case Type::BOOLEAN:
				case Type::CHAR:
					if (expr_type == Type::DOUBLE) {
						// Arrondir à l'entier le plus proche.
						std::cout << "\tfldl\t(%rsp)" << std::endl;
						std::cout << "\tfistpq\t(%rsp)" << std::endl;
					}
					break;
				case Type::DOUBLE:
					if (is_integral(expr_type)) {
						// Convertir d'entier à flottant 64 bits.
						std::cout << "\tfild\t(%rsp)" << std::endl;
						std::cout << "\tfstpl\t(%rsp)" << std::endl;
					}
					break;
			}
			if (count > args_number) {
				error("(FunctionCall) Erreur: Trop d'arguments dans l'appel de `" + function_name + "`!");
			}
		}
	}
	if (count < args_number) {
		error("(FunctionCall) Erreur: Trop peu d'arguments dans l'appel de `" + function_name + "`!");
	}

	if (current_token != Token::RPARENT) {
		error("(FunctionCall) Erreur: Symbole `)` attendu!");
	}
	read();
	std::cout << "\tcall\t" << function_name << std::endl;
	std::cout << "\taddq\t$" << 8 * args_number << ", %rsp" << std::endl;
	return subroutine(function_name).get_return_type();
}

// ProcedureCall := Identifier "(" [Expression {"," Expression}] ")"
void Compiler::ProcedureCall(const std::string& procedure_name) {
	if (current_token != Token::LPARENT) {
		error("(ProcedureCall) Erreur: Symbole `(` attendu!");
	}
	read();

    const std::vector<Variable>& args = subroutine(procedure_name).get_args();
    std::size_t args_number = args.size();
	unsigned long long count = 0;
	if (current_token != Token::RPARENT) {
		Type expr_type;
		
		expr_type = Expression();
		switch (args[count++].get_type()) {
			case Type::UINTEGER:
			case Type::BOOLEAN:
			case Type::CHAR:
				if (expr_type == Type::DOUBLE) {
					// Arrondir à l'entier le plus proche.
					std::cout << "\tfldl\t(%rsp)" << std::endl;
					std::cout << "\tfistpq\t(%rsp)" << std::endl;
				}
				break;
			case Type::DOUBLE:
				if (is_integral(expr_type)) {
					// Convertir d'entier à flottant 64 bits.
					std::cout << "\tfild\t(%rsp)" << std::endl;
					std::cout << "\tfstpl\t(%rsp)" << std::endl;
				}
				break;
		}
		if (count > args_number) {
			error("(ProcedureCall) Erreur: Trop d'arguments dans l'appel de `" + procedure_name + "`!");
		}

		while (current_token == Token::COMMA) {
			read();
			expr_type = Expression();
			switch (args[count++].get_type()) {
				case Type::UINTEGER:
				case Type::BOOLEAN:
				case Type::CHAR:
					if (expr_type == Type::DOUBLE) {
						// Arrondir à l'entier le plus proche.
						std::cout << "\tfldl\t(%rsp)" << std::endl;
						std::cout << "\tfistpq\t(%rsp)" << std::endl;
					}
					break;
				case Type::DOUBLE:
					if (is_integral(expr_type)) {
						// Convertir d'entier à flottant 64 bits.
						std::cout << "\tfild\t(%rsp)" << std::endl;
						std::cout << "\tfstpl\t(%rsp)" << std::endl;
					}
					break;
			}
			if (count > args_number) {
				error("(ProcedureCall) Erreur: Trop d'arguments dans l'appel de `" + procedure_name + "`!");
			}
		}
	}
	if (count < args_number) {
		error("(ProcedureCall) Erreur: Trop peu d'arguments dans l'appel de `" + procedure_name + "`!");
	}

	if (current_token != Token::RPARENT) {
		error("(ProcedureCall) Erreur: Symbole `)` attendu!");
	}
	read();
	std::cout << "\tcall\t" << procedure_name << std::endl;
	std::cout << "\taddq\t$" << 8 * args_number << ", %rsp" << std::endl;
}

// Factor := "!" Factor | "(" Expression ")" | Identifier | Constant
Type Compiler::Factor() {
	Type return_type;
	if (current_token == Token::NOT) {
		unsigned long long tag = ++tag_number;
		read();
		Factor();
		std::cout << "\tcmpq\t$0, (%rsp)" 			   << std::endl;
		std::cout << "\tje\tFaux" << tag               << std::endl;
		std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rax" << std::endl;
		std::cout << "\tjmp\tSuite" << tag			   << std::endl;
		std::cout << "Faux" << tag << ":" 			   << std::endl;
		std::cout << "\tmovq\t$0, %rax"                << std::endl;
		std::cout << "Suite" << tag << ":" 			   << std::endl;
		std::cout << "\tnotq\t%rax" 	               << std::endl;
		std::cout << "\tpushq\t%rax" 	               << std::endl;
		return_type = Type::BOOLEAN;
	} else if (current_token == Token::LPARENT) {
		read();
		return_type = Expression();
		if (current_token != Token::RPARENT) {
			error("(Factor) Erreur: Caractère `)` attendu!");
		}
		read();
	} else if (current_token == Token::ID) {
		return_type = Identifier();
	} else {
		return_type = Constant();
	}
	return return_type;
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
Opmul Compiler::MultiplicativeOperator() {
	Opmul opmul;
	if (strcmp("*", lexer->YYText()) == 0) {
		opmul = Opmul::MUL;
	} else if (strcmp("/", lexer->YYText()) == 0) {
		opmul = Opmul::DIV;
	} else if (strcmp("%", lexer->YYText()) == 0) {
		opmul = Opmul::MOD;
	} else if (strcmp("&&", lexer->YYText()) == 0) {
		opmul = Opmul::AND;
	} else {
		opmul = Opmul::WTFM;
	}
	read();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
Type Compiler::Term() {
	Type return_type = Factor();

	while (current_token == Token::MULOP) {
		Opmul mulop = MultiplicativeOperator();

		Type operand_type = Factor();

		if (operand_type == Type::STRING) {
			error("(Term) Opération sur string impossible!");
		}

		// Si au moins une opérande est Type::DOUBLE, alors le type de l'expression est Type::DOUBLE.
		// Sinon, le type de l'expression est Type::UINTEGER.

		bool FPU = (return_type == Type::DOUBLE || operand_type == Type::DOUBLE);

		// Opérande 2.
		if (operand_type == Type::DOUBLE) {
			std::cout << "\tfldl\t(%rsp)"   << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)"  << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rbx" << std::endl;
			}
		}

		// Opérande 1.
		if (return_type == Type::DOUBLE) {
			std::cout << "\tfldl\t(%rsp)"   << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)"  << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rax" << std::endl;
			}
		}

		unsigned long long tag = ++tag_number;
		switch(mulop) {
			case Opmul::AND:
				std::cout << "\tcmpq\t$0, %rax"                << std::endl;
				std::cout << "\tje\tFaux" << tag               << std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rax" << std::endl;
				std::cout << "\tjmp\tSuite" << tag             << std::endl;
				std::cout << "Faux" << tag << ":"              << std::endl;
				std::cout << "\tmovq\t$0, %rax"                << std::endl;
				std::cout << "Suite" << tag << ":"             << std::endl;

				tag = ++tag_number;
				std::cout << "\tcmpq\t$0, %rbx" 			   << std::endl;
				std::cout << "\tje\tFaux" << tag			   << std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rbx" << std::endl;
				std::cout << "\tjmp\tSuite" << tag			   << std::endl;
				std::cout << "Faux" << tag << ":" 			   << std::endl;
				std::cout << "\tmovq\t$0, %rbx" 			   << std::endl;
				std::cout << "Suite" << tag << ":" 			   << std::endl;

				std::cout << "\tandq\t%rbx, %rax" << std::endl;  // %rax and %rbx => %rax
				std::cout << "\tpushq\t%rax"      << std::endl;
				break;
			case Opmul::MUL:
				if (FPU) {
					std::cout << "\tfmulp\t%st(0), %st(1)" << std::endl;  // %st(0) * %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp"        << std::endl;
					std::cout << "\tfstpl\t(%rsp)"         << std::endl;
				} else {
					std::cout << "\tmulq\t%rbx"  << std::endl;  // %rbx * %rax => %rdx:%rax
					std::cout << "\tpushq\t%rax" << std::endl;
				}
				break;
			case Opmul::DIV:
				if (FPU) {
					std::cout << "\tfdivp\t%st(0), %st(1)" << std::endl;  // %st(0) / %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp"        << std::endl;
					std::cout << "\tfstpl\t(%rsp)"         << std::endl;
				} else {
					std::cout << "\tmovq\t$0, %rdx" << std::endl;  // Partie haute du numérateur
					std::cout << "\tdiv\t%rbx"      << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
					std::cout << "\tpushq\t%rax"    << std::endl;
				}
				break;
			case Opmul::MOD:
				if (!is_integral(return_type)) {
					error("(Term) Erreur: Le type de l'expression doit être entier! (" + TYPE_STRING[(int)return_type] + " lu)");
				}
				std::cout << "\tmovq\t$0, %rdx" << std::endl;  // Partie haute du numérateur
				std::cout << "\tdiv\t%rbx"      << std::endl;  // %rdx:%rax / %rbx => q:%rax r:%rdx
				std::cout << "\tpushq\t%rdx"    << std::endl;
				break;
			default:
				error("(Term) Erreur: Opérateur multiplicatif inconnu!");
		}

		if (FPU) {
			return_type = Type::DOUBLE;
		} else {
			return_type = Type::UINTEGER;
		}
	}

	return return_type;
}

// AdditiveOperator := "+" | "-" | "||".
Opadd Compiler::AdditiveOperator() {
	Opadd opadd;
	if (strcmp("+", lexer->YYText()) == 0) {
		opadd = Opadd::ADD;
	} else if (strcmp("-", lexer->YYText()) == 0) {
		opadd = Opadd::SUB;
	} else if (strcmp("||", lexer->YYText()) == 0) {
		opadd = Opadd::OR;
	} else {
		opadd = Opadd::WTFA;
	}
	read();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
Type Compiler::SimpleExpression(){
	Type return_type = Term();

	while(current_token == Token::ADDOP) {
		Opadd adop = AdditiveOperator();

		Type operand_type = Term();

		bool concat = false;
		if (return_type == Type::STRING || operand_type == Type::STRING) {
			if (return_type != operand_type || adop != Opadd::ADD) {
				error("(SimpleExpression) Erreur: Opération sur string impossible!");
			}
			// Deux string, addition -> OK
			concat = true;
		}  // TODO opérandes, opération de concaténation

		// Si au moins une opérande est Type::DOUBLE, alors le type de l'expression est Type::DOUBLE.
		// Sinon, le type de l'expression est Type::UINTEGER.

		bool FPU = (return_type == Type::DOUBLE || operand_type == Type::DOUBLE);

		// Opérande 2.
		if (operand_type == Type::DOUBLE) {
			std::cout << "\tfldl\t(%rsp)"   << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)"  << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rbx" << std::endl;
			}
		}

		// Opérande 1.
		if (return_type == Type::DOUBLE) {
			std::cout << "\tfldl\t(%rsp)"   << std::endl;
			std::cout << "\taddq\t$8, %rsp" << std::endl;
		} else {
			if (FPU) {
				std::cout << "\tfild\t (%rsp)"  << std::endl;
				std::cout << "\taddq\t$8, %rsp" << std::endl;
			} else {
				std::cout << "\tpopq\t%rax" << std::endl;
			}
		}

		unsigned long long tag = ++tag_number;
		switch(adop) {
			case Opadd::OR:
				std::cout << "\tcmpq\t$0, %rax" 			   << std::endl;
				std::cout << "\tje\tFaux" << tag			   << std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rax" << std::endl;
				std::cout << "\tjmp\tSuite" << tag			   << std::endl;
				std::cout << "Faux" << tag << ":" 			   << std::endl;
				std::cout << "\tmovq\t$0, %rax" 			   << std::endl;
				std::cout << "Suite" << tag << ":" 			   << std::endl;

				tag = ++tag_number;
				std::cout << "\tcmpq\t$0, %rbx" 			   << std::endl;
				std::cout << "\tje\tFaux" << tag			   << std::endl;
				std::cout << "\tmovq\t$FFFFFFFFFFFFFFFF, %rbx" << std::endl;
				std::cout << "\tjmp\tSuite" << tag			   << std::endl;
				std::cout << "Faux" << tag << ":" 			   << std::endl;
				std::cout << "\tmovq\t$0, %rbx" 			   << std::endl;
				std::cout << "Suite" << tag << ":" 			   << std::endl;

				std::cout << "\torq\t%rbx, %rax" << std::endl;  // %rax or %rbx => %rax
				std::cout << "\tpushq\t%rax"     << std::endl;
				break;
			case Opadd::ADD:
				if (concat) {

				}
				if (FPU) {
					std::cout << "\tfaddp\t%st(0), %st(1)" << std::endl;  // %st(0) + %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp"        << std::endl;
					std::cout << "\tfstpl\t(%rsp)"         << std::endl;
				} else {
					std::cout << "\taddq\t%rbx, %rax" << std::endl;   // %rbx + %rax => %rax
					std::cout << "\tpushq\t%rax"      << std::endl;
				}
				break;
			case Opadd::SUB:
				if (FPU) {
					std::cout << "\tfsubp\t%st(0), %st(1)" << std::endl;  // %st(0) - %st(1) => %st(1) puis pop
					std::cout << "\tsubq\t$8, %rsp"        << std::endl;
					std::cout << "\tfstpl\t(%rsp)"         << std::endl;
				} else {
					std::cout << "\tsubq\t%rbx, %rax" << std::endl;    // %rax - %rbx => %rax
					std::cout << "\tpushq\t%rax"      << std::endl;
				}
				break;
			default:
				error("(SimpleExpression) Erreur: Opérateur additif inconnu!");
		}

		if (FPU) {
			return_type = Type::DOUBLE;
		} else {
			return_type = Type::UINTEGER;
		}
	}

	return return_type;
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
Oprel Compiler::RelationalOperator() {
	Oprel oprel;
	if(strcmp("==", lexer->YYText()) == 0) {
		oprel = Oprel::EQU;
	} else if(strcmp("!=", lexer->YYText()) == 0) {
		oprel = Oprel::DIFF;
	} else if(strcmp("<", lexer->YYText()) == 0) {
		oprel = Oprel::INF;
	} else if(strcmp(">", lexer->YYText()) == 0) {
		oprel = Oprel::SUP;
	} else if(strcmp("<=", lexer->YYText()) == 0) {
		oprel = Oprel::INFE;
	} else if(strcmp(">=", lexer->YYText()) == 0) {
		oprel = Oprel::SUPE;
	} else {
		oprel = Oprel::WTFR;
	}
	read();
	return oprel;
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
Type Compiler::Expression() {
	Type return_type = SimpleExpression();

	if (current_token == Token::RELOP) {
		unsigned long long tag = ++tag_number;

		Oprel oprel = RelationalOperator();

		Type operand_type = SimpleExpression();

		if (operand_type == Type::STRING) {
			error("(Expression) Erreur: Opération sur string impossible!");
		}

		// Si au moins une opérande est Type::DOUBLE, alors le type de l'expression est Type::DOUBLE.
		// Sinon, le type de l'expression est Type::UINTEGER.

		bool FPU = (return_type == Type::DOUBLE || operand_type == Type::DOUBLE);

		// Opérande 2.
		if (operand_type == Type::DOUBLE) {
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
		if (return_type == Type::DOUBLE) {
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
			case Oprel::EQU:
				std::cout << "\tje\tVrai" << tag << "\t# If equal" << std::endl;
				break;
			case Oprel::DIFF:
				std::cout << "\tjne\tVrai" << tag << "\t# If not equal" << std::endl;
				break;
			case Oprel::SUPE:
				std::cout << "\tjae\tVrai" << tag << "\t# If above or equal" << std::endl;
				break;
			case Oprel::INFE:
				std::cout << "\tjbe\tVrai" << tag << "\t# If below or equal" << std::endl;
				break;
			case Oprel::INF:
				std::cout << "\tjb\tVrai" << tag << "\t# If below" << std::endl;
				break;
			case Oprel::SUP:
				std::cout << "\tja\tVrai" << tag << "\t# If above" << std::endl;
				break;
			default:
				error("(Expression) Erreur: Opérateur de comparaison inconnu!");
		}

		std::cout << "\tpushq\t$0\t# False" << std::endl;
		std::cout << "\tjmp\tSuite" << tag << std::endl;
		std::cout << "Vrai" << tag << ":" << std::endl;
		std::cout << "\tpushq\t$0xFFFFFFFFFFFFFFFF\t# True" << std::endl;	
		std::cout << "Suite" << tag << ":" << std::endl;

		return Type::BOOLEAN;
	}

	return return_type;
}

// BlockStatement := "BEGIN" Statement {";" Statement} "END"
void Compiler::BlockStatement() {
	read_keyword("BEGIN");
	Statement();
	while (current_token == Token::SEMICOLON) {
		read();
		Statement();
	}
	read_keyword("END");
}

// AssignmentStatement := Identifier ":=" Expression
void Compiler::AssignmentStatement() {
	if (current_token != Token::ID) {
		error("(AssignmentStatement) Erreur: Identifiant attendu!");
	}
	std::string id = lexer->YYText();
	read();

	// Appel de routine comme instruction après avoir lu un identifiant.
	if (current_token == Token::LPARENT) {
		if (!is_subroutine_declared(id)) {
			error("(AssignmentStatement) Erreur: Routine non déclarée!");
		}
		if (subroutine(id).get_return_type() == Type::WTFT) {
			ProcedureCall(id);
		} else {
			FunctionCall(id);
		}
		return;
	}

	Type type;
	bool found = false;
	std::string output;
	if (in_subroutine) {
		const Subroutine& this_subroutine = subroutine(id);
		if (current_subroutine == id && this_subroutine.get_return_type() != Type::WTFT) {
			// Cas spécial: assignement de la valeur de retour d'une fonction (pas de procédures donc).
			found = true;
			output = "\tpopq\t%rax";
			type = this_subroutine.get_return_type();
		} else {
			// Paramètres.
			const std::vector<Variable>& args = this_subroutine.get_args();
			for (int i = 0; i < args.size(); i++) {
				if (args[i].get_name() == id) {
					found = true;
					output = "\tpopq\t" + std::to_string(16 + 8 *i) + "(%rbp)";
					type = this_subroutine.get_return_type();
				}
			}
			// Variables locales.
			const std::vector<Variable>& locals = this_subroutine.get_local();
			for (int i = 0; i < locals.size(); i++) {
				if (locals[i].get_name() == id) {
					found = true;
					output = "\tpopq\t" + std::to_string(-8 - 8 * i) + "(%rbp)";
					type = locals[i].get_type();
				}
			}
		}
	}
	if (!found) {
		// Variables globales.
		if (!is_var_declared(id)) {
			error("(AssignmentStatement) Erreur: Variable `" + id + "` non déclarée!");
		}
		type = variable(id).get_type();
		output = "\tpopq\t" + id;
	}

	if (current_token != Token::ASSIGN) {
		error("(AssignmentStatement) Erreur: Symbole `:=` attendu!");
	}
	read();

	Type expr_type = Expression();
	switch (type) {
		case Type::UINTEGER:
		case Type::BOOLEAN:
		case Type::CHAR:
			if (expr_type == Type::DOUBLE) {
				// Arrondir à l'entier le plus proche.
				std::cout << "\tfldl\t(%rsp)" << std::endl;
				std::cout << "\tfistpq\t(%rsp)" << std::endl;
			}
			break;
		case Type::DOUBLE:
			if (is_integral(expr_type)) {
				// Convertir d'entier à flottant 64 bits.
				std::cout << "\tfild\t(%rsp)" << std::endl;
				std::cout << "\tfstpl\t(%rsp)" << std::endl;
			}
			break;
		case Type::STRING:
			if (expr_type != Type::STRING) {
				error("(AssignmentStatement) Erreur: Impossible d'assigner le type `" + TYPE_STRING[(int)expr_type] + "` au type `STRING`!");
			}
			break;
	}

	std::cout << output << std::endl;
}

// IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
void Compiler::IfStatement() {
	unsigned long long tag = ++tag_number;

	read_keyword("IF");
	std::cout << "If" << tag << ":" << std::endl;
	Type expr_type = Expression();
	if (expr_type != Type::BOOLEAN) {
		error("(IfStatement) Erreur: Expression booléenne attendue! (" + TYPE_STRING[(int)expr_type] + " lu)");
	}
	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t$0, %rax" << std::endl;
	std::cout << "\tje\tElse" << tag << std::endl;

	read_keyword("THEN");
	Statement();
	std::cout << "\tjmp\tEndIf" << tag << std::endl;

	// ELSE optionnel (=> pas d'erreur)
	std::cout << "Else" << tag << ":" << std::endl;
	if (current_token == Token::KEYWORD && strcmp("ELSE", lexer->YYText()) == 0) {
		read();
		Statement();
	}

	std::cout << "EndIf" << tag << ":" << std::endl;
}

// WhileStatement := "WHILE" Expression "DO" Statement
void Compiler::WhileStatement() {
	unsigned long long tag = ++tag_number;

	read_keyword("WHILE");
	std::cout << "TestWhile" << tag << ":" << std::endl;
	Type expr_type = Expression();
	if (expr_type != Type::BOOLEAN) {
		error("(WhileStatement) Erreur: Expression booléenne attendue! (" + TYPE_STRING[(int)expr_type] + " lue)");
	}
	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t$0, %rax" << std::endl;
	std::cout << "\tje\tEndWhile" << tag << std::endl;

	read_keyword("DO");
	Statement();
	std::cout << "\tjmp\tTestWhile" << tag << std::endl;
	std::cout << "EndWhile" << tag << ":" << std::endl;
}

// RepeatStatement := "REPEAT" Statement "UNTIL" Expression
void Compiler::RepeatStatement() {
	unsigned long long tag = ++tag_number;

	read_keyword("REPEAT");
	std::cout << "Repeat" << tag << ":" << std::endl;
	Statement();

	read_keyword("UNTIL");
	Type expr_type = Expression();
	if (expr_type != Type::BOOLEAN) {
		error("(RepeatStatement) Erreur: Expression booléenne attendue ! (" + TYPE_STRING[(int)expr_type] + " lue)");
	}
	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t$0, %rax" << std::endl;
	std::cout << "\tjne\tRepeat" << tag << std::endl;
}

// ForStatement := "FOR" AssignmentStatement ("TO" | "DOWNTO") Expression "DO" Statement
void Compiler::ForStatement() {
	unsigned long long tag = ++tag_number;

	read_keyword("FOR");
	bool found = false;
	std::string position;  // où incrémenter dans l'assembleur ?
	std::string id = lexer->YYText();  // nom de la variable utilisée lors de l'incrémentation (prochain token)
	if (in_subroutine) {
		const Subroutine& this_subroutine = subroutine(current_subroutine);
		// arguments
		const std::vector<Variable>& args = this_subroutine.get_args();
		for (int i = 0; i < args.size(); i++) {
			if (args[i].get_name() == id) {
				position = std::to_string(16 + 8 * i) + "(%rbp)";
				found = true;
			}
		}
		// varibales locales
		const std::vector<Variable>& locals = this_subroutine.get_local();
		for (int i = 0; i < locals.size(); i++) {
			if (locals[i].get_name() == id) {
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

	AssignmentStatement();

	if (current_token != Token::KEYWORD) {
		error("(ForStatement) Erreur: Mot clé attendu!");
	}
	if (strcmp("TO", lexer->YYText()) != 0 && strcmp("DOWNTO", lexer->YYText()) != 0) {
		error("(ForStatement) Erreur: Mots clés `TO` ou `DOWNTO` attendus!");
	}
	std::string jump;  // ja OR jb
	std::string increment;  // ++ OR --
	if (strcmp("TO", lexer->YYText()) == 0) {
		jump = "\tja\tEndFor";
		increment = "\taddq\t$1, ";
	} else {
		jump = "\tjb\tEndFor";
		increment = "\tsubq\t$1, ";
	}
	read();

	std::cout << "TestFor" << tag << ":" << std::endl;
	Type expr_type = Expression();
	if (expr_type != Type::UINTEGER) {
		error("(ForStatement) Erreur: L'incrément doit être entier!");
	}
	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t%rax, " << position << std::endl;
	std::cout << jump << tag << std::endl;
	
	read_keyword("DO");
	Statement();
	std::cout << increment << position << std::endl;  // incrémenter l'entier
	std::cout << "\tjmp\tTestFor" << tag << std::endl;
	std::cout << "EndFor" << tag << ":" << std::endl;
}

// DisplayStatement := "DISPLAY" Expression
void Compiler::DisplayStatement() {
	read_keyword("DISPLAY");

	Type expr_type = Expression();
	switch (expr_type) {
		case Type::UINTEGER:
			if (!FS["LLU"]) {
				FS["LLU"] = true;
				internal_data << "FSLLU:\n\t.string \"%llu\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSLLU, %rsi"     << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case Type::BOOLEAN: {
			if (!FS["B"]) {
				FS["B"] = true;
				internal_data << "FSTRUE:\n\t.string \"TRUE\"" << std::endl;
				internal_data << "FSFALSE:\n\t.string \"FALSE\"" << std::endl;
			}
			unsigned long long tag = ++tag_number;
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
		case Type::DOUBLE:
			if (!FS["F"]) {
				FS["F"] = true;
				internal_data << "FSF:\n\t.string \"%lf\"" << std::endl;
			}
			std::cout << "\tmovsd\t(%rsp), %xmm0"   << std::endl;
			std::cout << "\tmovq\t$FSF, %rsi"       << std::endl;
			std::cout << "\tmovl\t$1, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			std::cout << "\taddq\t$8, %rsp"         << std::endl;
			break;
		case Type::CHAR:
			if (!FS["C"]) {
				FS["C"] = true;
				internal_data << "FSC:\n\t.string \"%c\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSC, %rsi"       << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case Type::STRING:
			if (!FS["S"]) {
				FS["S"] = true;
				internal_data << "FSS:\n\t.string \"%s\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSS, %rsi"       << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		default:
			error("(DisplayStatement) Erreur: Type inconnu!");
	}
}

// DisplaylnStatement := "DISPLAYLN" Expression
void Compiler::DisplaylnStatement() {
	read_keyword("DISPLAYLN");

	Type expr_type = Expression();
	switch (expr_type) {
		case Type::UINTEGER:
			if (!FS["LLUln"]) {
				FS["LLUln"] = true;
				internal_data << "FSLLUln:\n\t.string \"%llu\\n\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSLLUln, %rsi"   << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case Type::BOOLEAN: {
			if (!FS["Bln"]) {
				FS["Bln"] = true;
				internal_data << "FSTRUEln:\n\t.string \"TRUE\\n\"" << std::endl;
				internal_data << "FSFALSEln:\n\t.string \"FALSE\\n\"" << std::endl;
			}
			unsigned long long tag = ++tag_number;
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
		case Type::DOUBLE:
			if (!FS["Fln"]) {
				FS["Fln"] = true;
				internal_data << "FSFln:\n\t.string \"%lf\\n\"" << std::endl;
			}
			std::cout << "\tmovsd\t(%rsp), %xmm0"   << std::endl;
			std::cout << "\tmovq\t$FSFln, %rsi"     << std::endl;
			std::cout << "\tmovl\t$1, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			std::cout << "\taddq\t$8, %rsp"         << std::endl;
			break;
		case Type::CHAR:
			if (!FS["Cln"]) {
				FS["Cln"] = true;
				internal_data << "FSCln:\n\t.string \"%c\\n\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSCln, %rsi"     << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		case Type::STRING:
			if (!FS["Sln"]) {
				FS["Sln"] = true;
				internal_data << "FSSln:\n\t.string \"%s\\n\"" << std::endl;
			}
			std::cout << "\tpopq\t%rdx"             << std::endl;
			std::cout << "\tmovq\t$FSSln, %rsi"     << std::endl;
			std::cout << "\tmovl\t$0, %eax"         << std::endl;
			std::cout << "\tmovl\t$1, %edi"         << std::endl;
			std::cout << "\tcall\t__printf_chk@PLT" << std::endl;
			break;
		default:
			error("(DisplayStatement) Erreur: Type inconnu!");
	}
}

// CaseLabelList := Constant {"," Constant}
Type Compiler::CaseLabelList() {
	unsigned long long tag = tag_number;  // numéro d'étiquette du cas actuel

	Type return_type = Constant();
	if (return_type == Type::STRING) {
		error("(CaseLabelList) Erreur: Constante ne peut pas être string!");
	}
	std::cout << "\tpopq\t%rax" << std::endl;
	std::cout << "\tcmpq\t(%rsp), %rax" << std::endl;
	std::cout << "\tje\tStatement" << tag << std::endl;

	while (current_token == Token::COMMA) {
		read();
		Type constType = Constant();
		if (return_type != constType) {
			error("(CaseLabelList) Erreur: Les constantes doivent avoir le même type! (" + TYPE_STRING[(int)constType] + " lu)");
		}
		std::cout << "\tpopq\t%rax" << std::endl;
		std::cout << "\tcmpq\t(%rsp), %rax" << std::endl;
		std::cout << "\tje\tStatement" << tag << std::endl;
	}
	return return_type;
}

// CaseElement := CaseLabelList ":" Statement
Type Compiler::CaseElement(unsigned long long endTagNumber) {
	unsigned long long tag = tag_number;  // numéro d'étiquette du cas actuel

	Type label_type = CaseLabelList();
	if (current_token != Token::COLON) {
		error("(CaseElement) Erreur: Symbole `:` attendu!");
	}
	read();

	std::cout << "\tjmp\tCase" << tag + 1 << std::endl;
	std::cout << "Statement" << tag << ":" << std::endl;
	Statement();
	std::cout << "\tjmp\tEndCase" << endTagNumber << std::endl;

	// Cette étiquette doit correspondre avec le jump juste au dessus.
	std::cout << "Case" << tag + 1 << ":" << std::endl;
	tag_number++;

	return label_type;
}

// CaseStatement := "CASE" Expression "OF" CaseElement {";" CaseElement} [";" "ELSE" Statement] "END"
void Compiler::CaseStatement() {
	unsigned long long tag = ++tag_number;

	read_keyword("CASE");
	std::cout << "Case" << tag << ":" << std::endl;

	Type expr_type = Expression();
	if (!is_integral(expr_type)) {
		error("(CaseStatement) Erreur: Expression de type entier attendue! (" + TYPE_STRING[(int)expr_type] + " lu)");
	}
	read_keyword("OF");

	Type label_type = CaseElement(tag);
	if (expr_type != label_type) {
		error("(CaseStatement) Erreur: Types incompatibles: " + TYPE_STRING[(int)expr_type] + " et " + TYPE_STRING[(int)label_type]);
	}

	while (current_token == Token::SEMICOLON) {
		read();
		if (current_token == Token::KEYWORD && strcmp("ELSE", lexer->YYText()) == 0) {
			read();
			Statement();
			break;
		}
		Type label_type = CaseElement(tag);
		if (expr_type != label_type) {
			error("(CaseStatement) Erreur: Types incompatibles: " + TYPE_STRING[(int)expr_type] + " et " + TYPE_STRING[(int)label_type]);
		}
	}

	read_keyword("END");
	std::cout << "EndCase" << tag << ":" << std::endl;
	std::cout << "\taddq\t$8, %rsp" << std::endl;
}

// Statement := AssignmentStatement | IfStatement | WhileStatement | RepeatStatement | ForStatement | BlockStatement | DisplayStatement | DisplaylnStatement | CaseStatement
void Compiler::Statement() {
	if (current_token == Token::ID) {
		AssignmentStatement();
	} else if (current_token == Token::KEYWORD) {
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
			error("(Statement) Erreur: Instruction inconnue!");
		}
	} else {
		error("(Statement) Erreur: Identifiant ou mot clé attendu!");
	}
}

// VarDeclaration := Identifier {"," Identifier} ":" Type
void Compiler::VarDeclaration() {
	if (current_token != Token::ID) {
		error("(VarDeclaration) Erreur: Identifiant attendu!");
	}

	// Ensemble de noms des variables déclarées ici.
	std::vector<std::string> var_list;
	// Vérification
	if (is_var_declared(lexer->YYText())) {
		error("(VarDeclaration) Erreur: Variable `" + std::string(lexer->YYText()) + "` déjà déclarée!");
	}
	var_list.push_back(lexer->YYText());
	read();

	while(current_token == Token::COMMA) {
		read();
		// Vérification
		if (is_var_declared(lexer->YYText())) {
			error("(VarDeclaration) Erreur: Variable `" + std::string(lexer->YYText()) + "` déjà déclarée!");
		}
		var_list.push_back(lexer->YYText());
		read();
	}

	if (current_token != Token::COLON) {
		error("(VarDeclaration) Erreur: Symbole `:` attendu!");
	}
	read();

	Type varType = Typename();
	// 1. Déclarer et 2. Créer les var_list lues.
	// Toutes les var_list sont initialisées avec la valeur 0 (0.0, false).
	switch (varType) {
		case Type::UINTEGER:
			// .quad.
			for (std::string variableName : var_list) {
				variables.push_back({variableName, Type::UINTEGER});  // déclarer
				std::cout << variableName << ":\n\t.quad 0" << std::endl;  // créer
			}
			break;
		case Type::BOOLEAN:
			// .byte OU .quad également, on n'est pas à ça près.
			for (std::string variableName : var_list) {
				variables.push_back({variableName, Type::BOOLEAN});
				std::cout << variableName << ":\n\t.quad 0" << std::endl;
			}
			break;
		case Type::DOUBLE:
			// .double
			for (std::string variableName : var_list) {
				variables.push_back({variableName, Type::DOUBLE});
				std::cout << variableName << ":\n\t.double 0.0" << std::endl;
			}
			break;
		case Type::CHAR:
			// .byte
			for (std::string variableName : var_list) {
				variables.push_back({variableName, Type::CHAR});
				std::cout << variableName << ":\n\t.byte 0" << std::endl;
			}
			break;
		case Type::STRING:
			// .quad adresse
			for (std::string variableName : var_list) {
				variables.push_back({variableName, Type::STRING});
				std::cout << variableName << ":\n\t.quad 0" << std::endl;
			}
			break;
		default:
			error("(VarDeclaration) Erreur: Type inconnu!");
	}
}

// VarSection := "VAR" VarDeclaration {";" VarDeclaration}
void Compiler::VarSection() {
	read_keyword("VAR");
	VarDeclaration();
	while (current_token == Token::SEMICOLON) {
		read();
		VarDeclaration();
	}
}

// ArgumentList := Identifier {"," Identifier} ":" Type
void Compiler::ArgumentList(const std::string& subroutine_name, std::vector<Variable>& args) {
	if (current_token != Token::ID) {
		error("(ArgumentList) Erreur: Identifiant attendu!");
	}

	std::vector<std::string> var_list;

	std::string id = lexer->YYText();
	if (std::find(var_list.begin(), var_list.end(), id) != var_list.end()) {
		error("(ArgumentList) Erreur: Argument `" + std::string(id) + "` déjà déclaré!");
	} else if (id == subroutine_name) {
		error("(ArgumentList) Erreur: Un argument ne peut pas avoir le même nom que sa fonction!");
	}
	var_list.push_back(id);
	read();

	while(current_token == Token::COMMA) {
		read();
		id = lexer->YYText();
		if (std::find(var_list.begin(), var_list.end(), id) != var_list.end()) {
			error("(ArgumentList) Erreur: Argument `" + std::string(id) + "` déjà déclaré!");
		} else if (id == subroutine_name) {
			error("(ArgumentList) Erreur: Un argument ne peut pas avoir le même nom que sa fonction!");
		}
		var_list.push_back(id);
		read();
	}

	if (current_token != Token::COLON) {
		error("(ArgumentList) Erreur: Symbole `:` attendu!");
	}
	read();

	Type var_type = Typename();
	if (var_type == Type::WTFT) {
		error("(ArgumentList) Erreur: Type inconnu!");
	}
	for (std::string var_name : var_list) {
		args.push_back({var_name, var_type});
	}
}

// LocalDeclaration := Identifier {"," Identifier} ":" TYPE
void Compiler::LocalDeclaration(const std::string& subroutine_name) {
	if (current_token != Token::ID) {
		error("(LocalVarDeclaration) Erreur: Identifiant attendu!");
	}

	std::vector<std::string> var_list;

	std::string id = lexer->YYText();
	if (std::find(var_list.begin(), var_list.end(), id) != var_list.end()) {
		error("(LocalVarDeclaration) Erreur: Argument `" + id + "` déjà déclaré!");
	} else if (id == subroutine_name) {
		error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom que sa fonction!");
	} else if (is_argument(id, subroutine_name)) {
		error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom qu'un argument!");
	}
	var_list.push_back(id);
	read();

	while(current_token == Token::COMMA) {
		read();
		id = lexer->YYText();
		if (std::find(var_list.begin(), var_list.end(), id) != var_list.end()) {
			error("(LocalVarDeclaration) Erreur: Argument `" + id + "` déjà déclaré!");
		} else if (id == subroutine_name) {
			error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom que sa fonction!");
		} else if (is_argument(id, subroutine_name)) {
			error("(LocalVarDeclaration) Erreur: Variable locale `" + id + "` ne peut pas avoir le même nom qu'un argument!");
		}
		var_list.push_back(id);
		read();
	}

	if (current_token != Token::COLON) {
		error("(ArgumentList) Erreur: Symbole `:` attendu!");
	}
	read();

	Type varType = Typename();
	if (varType == Type::WTFT) {
		error("(ArgumentList) Erreur: Type inconnu!");
	}
	for (std::string varName : var_list) {
		subroutine(subroutine_name).add_local({varName, varType});
	}
}

// LocalSection := "VAR" LocalVarDeclaration {";" LocalVarDeclaration}
void Compiler::LocalSection(const std::string& subroutine_name) {
	read_keyword("VAR");
	LocalDeclaration(subroutine_name);
	while (current_token == Token::SEMICOLON) {
		read();
		LocalDeclaration(subroutine_name);
	}
}

// Function := "FUNCTION" Identifier "(" [ArgumentList {";" ArgumentList}] ")" ":" Type [[LocalVarSection] BlockStatement]
void Compiler::Function() {
	read_keyword("FUNCTION");

	if (current_token != Token::ID) {
		error("(Function) Erreur: Identifiant attendu!");
	}
	std::string function_name = lexer->YYText();
	read();

	if (current_token != Token::LPARENT) {
		error("(Function) Erreur: Symbole `(` attendu!");
	}
	read();

	std::vector<Variable> arguments;  // on ne touche pas à la struct car il pourrait déjà y avoir une déclaration
	if (current_token == Token::ID) {
		ArgumentList(function_name, arguments);
		while (current_token == Token::SEMICOLON) {
			read();
			ArgumentList(function_name, arguments);
		}
	}
	std::reverse(arguments.begin(), arguments.end());

	if (current_token != Token::RPARENT) {
		error("(Function) Erreur: Symbole `)` attendu!");
	}
	read();

	if (current_token != Token::COLON) {
		error("(Function) Erreur: Symbole `:` attendu!");
	}
	read();

	Type returnType = Typename();
	if (returnType == Type::WTFT) {
		error("(Function) Erreur: Type de retour inconnu!");
	}

	if (is_subroutine_declared(function_name)) {
		if (arguments != subroutine(function_name).get_args() || returnType != subroutine(function_name).get_return_type()) {
			error("(Function) Erreur: Déclaration de la fonction `" + function_name + "` incompatible avec une précédente déclaration!");
		}
	} else {
		Subroutine S(function_name, arguments, returnType, false);
		subroutines.push_back(S);
	}

	if (current_token == Token::KEYWORD) {
		if(is_subroutine_defined(function_name)) {
			error("(Function) Erreur: Fonction `" + function_name + "` déjà définie!");
		}

		if (strcmp("VAR", lexer->YYText()) == 0) {
			LocalSection(function_name);
		}

		std::cout << function_name << ":" << std::endl;
		std::cout << "\tpushq\t%rbp" << std::endl;
		std::cout << "\tmovq\t%rsp, %rbp" << std::endl;

		unsigned long long local_size = subroutine(function_name).get_local().size();
		if (local_size > 0) {
			local_size *= 8;
			std::cout << "\tsubq\t$" << local_size << ", %rsp" << std::endl;
		}

		in_subroutine = true;
		current_subroutine = function_name;
		BlockStatement();
		current_subroutine = "";
		in_subroutine = false;

		subroutine(function_name).set_define(true);

		if (local_size != 0) {
			std::cout << "\taddq\t$" << local_size * 8 << ", %rsp" << std::endl;
		}
		std::cout << "\tpopq\t%rbp" << std::endl;
		std::cout << "\tret" << std::endl;
	}
}

// FunctionSection := Function {";" Function}
void Compiler::FunctionSection() {
	Function();
	while (current_token == Token::SEMICOLON) {
		read();
		Function();
	}
}

// Procedure := "PROCEDURE" Identifier "(" [ArgumentList {";" ArgumentList}] ")" [[LocalVarSection] BlockStatement]
void Compiler::Procedure() {
	read_keyword("PROCEDURE");

	if (current_token != Token::ID) {
		error("(Procedure) Erreur: Identifiant attendu!");
	}
	std::string procedure_name = lexer->YYText();
	read();

	if (current_token != Token::LPARENT) {
		error("(Procedure) Erreur: Symbole `(` attendu!");
	}
	read();

	std::vector<Variable> arguments;  // on ne touche pas à la struct car il pourrait déjà y avoir une déclaration
	if (current_token == Token::ID) {
		ArgumentList(procedure_name, arguments);
		while (current_token == Token::SEMICOLON) {
			read();
			ArgumentList(procedure_name, arguments);
		}
	}
	std::reverse(arguments.begin(), arguments.end());

	if (current_token != Token::RPARENT) {
		error("(Procedure) Erreur: Symbole `)` attendu!");
	}
	read();

	if (is_subroutine_declared(procedure_name)) {
		if (arguments != subroutine(procedure_name).get_args()) {
			error("(Procedure) Erreur: Déclaration de la fonction `" + procedure_name + "` incompatible avec une précédente déclaration!");
		}
	} else {
		Subroutine S(procedure_name, arguments, Type::WTFT, false);
		subroutines.push_back(S);
	}

	if (current_token == Token::KEYWORD) {
		if(is_subroutine_defined(procedure_name)) {
			error("(Procedure) Erreur: Fonction `" + procedure_name + "` déjà définie!");
		}

		if (strcmp("VAR", lexer->YYText()) == 0) {
			LocalSection(procedure_name);
		}

		std::cout << procedure_name << ":" << std::endl;
		std::cout << "\tpushq\t%rbp" << std::endl;
		std::cout << "\tmovq\t%rsp, %rbp" << std::endl;

		unsigned long long local_size = subroutine(procedure_name).get_local().size();
		if (local_size > 0) {
			local_size *= 8;
			std::cout << "\tsubq\t$" << local_size << ", %rsp" << std::endl;
		}

		in_subroutine = true;
		current_subroutine = procedure_name;
		BlockStatement();
		current_subroutine = "";
		in_subroutine = false;

		subroutine(procedure_name).set_define(true);

		if (local_size != 0) {
			std::cout << "\taddq\t$" << local_size * 8 << ", %rsp" << std::endl;
		}
		std::cout << "\tpopq\t%rbp" << std::endl;
		std::cout << "\tret" << std::endl;
	}
}

// ProcedureSection := Procedure {";" Procedure}
void Compiler::ProcedureSection() {
	Procedure();
	while (current_token == Token::SEMICOLON) {
		read();
		Procedure();
	}
}

// Program := "PROGRAM" Identifier ";" [VarSection "."] [FunctionSection "."] [ProcedureSection "."] BlockStatement "."
void Compiler::Program() {
	std::cout << "# This code was produced by VICTOR's Vcompiler for Vascal. <3" << std::endl;

	read_keyword("PROGRAM");

	if (current_token != Token::ID) {
		error("(Program) Erreur: Identifiant attendu!");
	}
	read();

	if (current_token != Token::SEMICOLON) {
		error("(Program) Erreur: Symbole `;` attendu!");
	}
	read();

	if (current_token == Token::KEYWORD && strcmp("VAR", lexer->YYText()) == 0) {
		std::cout << "\t.data" << std::endl;
		VarSection();
		if (current_token != Token::DOT) {
			error("(Program) Erreur: Symbole `.` attendu!");
		}
		read();
	}

	std::cout << "\t.text"    << std::endl;
	std::cout << "\t.align 8" << std::endl;

	if (current_token == Token::KEYWORD && strcmp("FUNCTION", lexer->YYText()) == 0) {
		FunctionSection();
		if (current_token != Token::DOT) {
			error("(Program) Erreur: Symbole `.` attendu!");
		}
		read();
	}

	if (current_token == Token::KEYWORD && strcmp("PROCEDURE", lexer->YYText()) == 0) {
		ProcedureSection();
		if (current_token != Token::DOT) {
			error("(Program) Erreur: Symbole `.` attendu!");
		}
		read();
	}
	
	std::cout << "\t.globl main"     << std::endl;
	// std::cout << "\t.align 8"     << std::endl;
	std::cout << "main:"             << std::endl;
	std::cout << "\tmovq\t%rsp, %rbp" << std::endl;

	BlockStatement();

	if (current_token != Token::DOT) {
		error("(Program) Erreur: Symbole `.` attendu!");
	}
	read();

	// Trailer for the gcc assembler / linker.
	std::cout << "\tmovq\t$0, %rax" << std::endl;  // valeur de retour de main égale à 0.
	std::cout << "\tmovq\t%rbp, %rsp" << std::endl;
	std::cout << "\tret" << std::endl;

	if (!internal_data.str().empty()) {
		std::cout << "\t.data" << std::endl;
		std::cout << internal_data.str();
	}
}
