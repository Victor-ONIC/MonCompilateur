	//////
	//
	// Documentation: https://www.cs.virginia.edu/~cr4bd/flex-manual/
	//
	// ( séparateur de sections: %% )
	// ( copie exacte de ce qui est entre %{ et }% )
	// ( copie exacte de ce qui est tabulé )
	// ( coloration syntaxique avec l'extension VSCode "Lex" par "luniclynx" )
	// 
	//////



	///////////////////////////////////////////////////////
	//
	// SECTION 1
	// DEFINITIONS SECTION
	//
	// Contient des déclarations simples du type "name definition"
	// afin de faciliter la suite.
	//
	// Doc: https://www.cs.virginia.edu/~cr4bd/flex-manual/Definitions-Section.html
	//
	///////////////////////////////////////////////////////



%{
// This is our Lexical tokeniser.
// It should be compiled into cpp with:
//     flex++ -d -otokeniser.cpp tokeniser.l 
// And then compiled into object with:
//     g++ -c tokeniser.cpp
// Tokens can be read using lexer->yylex().
//     lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h).
//     lexer->YYText() returns the lexicon entry as a c-string.

#include "tokeniser.h"
#include <iostream>
%}

	// option noyywrap: supprimer certains warnings/erreurs.
%option noyywrap
	// option c++: demande à flex de générer une classe c++.
%option c++
	// option yylineno: demande à flex de suivre les numéros de ligne.
%option yylineno



	/*
		Rappel: {name} correspond à réécrire l'expression définie par name.
		si:
		    expr    [abc]
		alors:
		    {expr}+ équivaut à ([abc])+
	*/

	// stringconst: toute suite de catactères différents de \n ou ", entourés de deux ".
stringconst  \"[^\n"]+\"

	// character: tout caractère entre deux '.
character	\'([^\'\\]|\\.)\'

	// ws: toute suite de ' ', \t, \n, ou \r.
ws      [ \t\n\r]+

	// alpha: un caractère de l'alphabet latin, majuscule ou minuscule.
alpha   [A-Za-z]

	// digit: un chiffre de 0 à 9.
digit   [0-9]

	// number: équivaut à [0-9]+, soit une suite de un ou plus <digit>.
number  {digit}+

	// float: un ou plusieurs chiffres, suivis d'un point, suivi d'un ou plusieurs chiffres.
float	-?{digit}+\.{digit}+

	// id: un caractère suivi d'une suite de 0 ou plus caractères ou chiffres.
id		{alpha}({alpha}|{digit}|_)*

	// addop: +, -, ou ||
addop	(\+|\-|\|\|)

	// mulop: *, /, %, ou &&
mulop	(\*|\/|%|\&\&)

	// relop: <, >, ==, <=, >=, ou !=
relop	(\<|\>|"=="|\<=|\>=|!=)

	// keyword: IF, THEN, ELSE, WHILE, DO, FOR, TO, BEGIN, ou END
keyword (PROGRAM|FUNCTION|PROCEDURE|IF|THEN|ELSE|WHILE|DO|REPEAT|UNTIL|FOR|TO|DOWNTO|BEGIN|END|DISPLAY|DISPLAYLN|CASE|OF|TRUE|FALSE|VAR|UINTEGER|BOOLEAN|DOUBLE|CHAR|STRING)

	// unknown: toute suite de un ou plus caractères DIFFÉRENTS de
	// ", A-Z, a-z, 0-9, ' ', \n, \r, \t, (, ), <, >, =, !, %, &, |, }, -, ;, ou .
unknown [^\"A-Za-z0-9 \n\r\t\(\)\<\>\=\!\%\&\|\}\-\;\.]+



%%
	///////////////////////////////////////////////////////
	//
	// SECTION 2
	// RULES SECTION
	//
	// Contient des règles de la forme "pattern action".
	// Que faire quand un token est lu ?
	// Lorsqu'un token est lu, chaque pattern est vérifié, DE HAUT EN BAS.
	//
	// Doc: https://www.cs.virginia.edu/~cr4bd/flex-manual/Rules-Section.html
	// Patterns: https://www.cs.virginia.edu/~cr4bd/flex-manual/Patterns.html
	// Actions: https://www.cs.virginia.edu/~cr4bd/flex-manual/Actions.html
	// EOF: https://www.cs.virginia.edu/~cr4bd/flex-manual/EOF.html 
	//
	///////////////////////////////////////////////////////



{stringconst}	return (int)Token::STRINGCONST;
{addop}		return (int)Token::ADDOP;
{mulop}		return (int)Token::MULOP;
{relop}		return (int)Token::RELOP;
{float} 	return (int)Token::FLOATCONST;
{number}	return (int)Token::NUMBER;
{keyword}	return (int)Token::KEYWORD;
{id}		return (int)Token::ID;
{character} 	return (int)Token::CHARCONST;
"["			return (int)Token::LBRACKET;
"]"			return (int)Token::RBRACKET;
","			return (int)Token::COMMA;
";"			return (int)Token::SEMICOLON;
":"         return (int)Token::COLON;
"."			return (int)Token::DOT;
":="		return (int)Token::ASSIGN;
"("			return (int)Token::LPARENT;
")"			return (int)Token::RPARENT;
"!"			return (int)Token::NOT;
{ws}    	// Skip whitespace characters: do nothing.
{unknown}	return (int)Token::UNKNOWN;
<<EOF>>		return (int)Token::FEOF;

"(*"    {

		// Skip comments between "(*" and "*)".
		int c;
		while ((c = yyinput()) != 0) {
    		if (c == '*') {
    	    	if ((c = yyinput()) == ')') {
    	    		break;
				} else {
  		    		unput(c);
				}
  			}
		}

		}



%%
	/// SECTION 3
	/// USER CODE SECTION
	/// Copie exacte de toute cette partie, utilisée pour créer des fonctions compagnon au scanneur.
	///
	/// Vide
	/// (le %% aurait donc pu être omis)
	///
	/// Doc: https://www.cs.virginia.edu/~cr4bd/flex-manual/User-Code-Section.html
