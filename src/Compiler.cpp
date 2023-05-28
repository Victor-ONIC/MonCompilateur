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

Variable& Compiler::get_variable(const std::string& name) {  // Compiler::?
    return *std::find(variables.begin(), variables.end(), name);
}
const Variable& Compiler::get_variable(const std::string& name) const {  // ^
    return *std::find(variables.begin(), variables.end(), name);
}

Subroutine& Compiler::get_subroutine(const std::string& name) { // ^
    return *std::find(subroutines.begin(), subroutines.end(), name);
}
const Subroutine& Compiler::get_subroutine(const std::string& name) const { // ^
    return *std::find(subroutines.begin(), subroutines.end(), name);
}

void Compiler::error(const std::string& message) const {
	std::cerr << "Ligne n°" << lexer->lineno() << ", lu: `" << lexer->YYText() << "` (" << TOKEN_STRING[(int)current_token] << ")." << std::endl;
	std::cerr << message << std::endl;
	exit(-1);
}

// void Compiler::write(const std::string& line) const {
//     std::cout << line << std::endl;
// }

// const std::string Compiler::current_text() const {
//     return lexer->YYText();
// }

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

bool Compiler::is_var_declared(const std::string& name) const {
    return std::find(variables.begin(), variables.end(), name) != variables.end();
}

bool Compiler::is_subroutine_declared(const std::string& name) const {
    return std::find(subroutines.begin(), subroutines.end(), name) != subroutines.end();
}

bool Compiler::is_subroutine_defined(const std::string& name) const {
    return is_subroutine_declared(name) && get_subroutine(name).get_defined();
}

bool Compiler::is_argument(const std::string& name, const std::string& subroutine) const {
    if (!is_subroutine_declared(subroutine)) return false;
    const std::vector<Variable>& args = get_subroutine(subroutine).get_args();
    return std::find(args.begin(), args.end(), name) != args.end();
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
        if (get_subroutine(id).get_return_type() == Type::WTFT) {
            error("(Identifier) Erreur: Procédure n'a pas de valeur de retour!");
        }
        Type return_type = FunctionCall(id);
		std::cout << "\tpushq\t%rax" << std::endl;
        return return_type;
    }

    if (in_subroutine) {
        const std::vector<Variable>& args = get_subroutine(id).get_args();
        for (int i = 0; i < args.size(); i++) {
            if (args[i].get_name() == id) {
				std::cout << "\tpushq\t" << 16 + 8 * i << "(%rbp)" << std::endl;
                return args[i].get_type();
            }
        }

        const std::vector<Variable>& local = get_subroutine(id).get_local();
        for (int i = 0; i < local.size(); i++) {
            if (local[i].get_name() == id) {
				std::cout << "\tpushq\t" << -8 - 8 * i << "(%rbp)" << std::endl;
                return local[i].get_type();
            }
        }
    }

    if (!is_var_declared(id)) {
        error("(IdentifierVariable non déclarée!");
    }
	std::cout << "\tpushq\t" << id << std::endl;
    return get_variable(id).get_type();
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

Type Compiler::FunctionCall(const std::string& functionName) {
}
