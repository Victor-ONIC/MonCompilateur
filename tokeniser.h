// tokeniser.h : shared definition for tokeniser.l and compilateur.cpp

// Les valeurs de FEOF à KEYWORD sont accessibles globalement.
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
    KEYWORD,     // ajouté TP3
    COLON
};
