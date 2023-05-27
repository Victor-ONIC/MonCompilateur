// tokeniser.h : shared definition for tokeniser.l and compilateur.cpp

enum TOKEN {
    FEOF,
    UNKNOWN,
    NUMBER,
    ID,
    STRINGCONST,
    LBRACKET,
    RBRACKET,
    LPARENT,
    RPARENT,
    COMMA, 
    SEMICOLON,
    DOT,
    ADDOP,
    MULOP,
    RELOP,
    NOT,
    ASSIGN,
    KEYWORD,
    COLON,
    FLOATCONST,
    CHARCONST
};
