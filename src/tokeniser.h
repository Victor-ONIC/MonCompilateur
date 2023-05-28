#ifndef TOKENISER_H_
#define TOKENISER_H_

#include <string>
#include <map>

// Traduction de ENUM à STRING pour afficher des erreurs plus lisibles.
const std::string TOKEN_STRING[] = {
	"FEOF", "UNKNOWN", "NUMBER", "ID", "STRINGCONST", "LBRACKET", "RBRACKET",
	"LPARENT", "RPARENT", "COMMA", "SEMICOLON", "DOT", "ADDOP", "MULOP",
	"RELOP", "NOT", "ASSIGN", "KEYWORD", "COLON", "FLOATCONST", "CHARCONST"
};
const std::string TYPE_STRING[] = {
	"UINTEGER", "BOOLEAN", "DOUBLE", "CHAR", "STRING"
};
// Pour chaque FormatString, doit-on l'afficher ?
const std::map<std::string, bool> FS = {
	{"LLU", false}, {"F", false}, {"C", false}, {"B", false}, {"S", false},
	{"LLUln", false}, {"Fln", false}, {"Cln", false}, {"Bln", false}, {"Sln", false}
};

enum class Token {
    FEOF, UNKNOWN, NUMBER, ID, STRINGCONST, LBRACKET, RBRACKET, LPARENT,
    RPARENT, COMMA, SEMICOLON, DOT, ADDOP, MULOP, RELOP, NOT, ASSIGN,
    KEYWORD, COLON, FLOATCONST, CHARCONST
};

enum class Oprel {
    EQU, DIFF, INF, SUP, INFE, SUPE, WTFR
};

enum class Opadd {
    ADD, SUB, OR, WTFA
};

enum class Opmul {
    MUL, DIV, MOD, AND, WTFM
};

enum class Type {
    UINTEGER, BOOLEAN, DOUBLE, CHAR, STRING, WTFT
};

#endif
