# Vcompiler

A simple compiler.  
From : Pascal-like imperative LL(k) langage  
To : 64 bit 80x86 assembly langage (AT&T)

**Download the repository :**
> git clone https://github.com/Victor-ONIC/MonCompilateur.git

**Build the compiler**
> make compilateur

**Test the compiler on file test.p**
> make  
> make test

**Both**
> make go

**This version Can handle:**   
Letter := "a"|...|"z"|"A"|...|"Z"   
Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"    
MultiplicativeOperator := "*" | "/" | "%" | "&&"    
AdditiveOperator := "+" | "-" | "||"    
RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="     
Number := Digit {Digit}     
Identifier := Letter {Letter | Digit}   
Factor := Number | Identifier | "(" Expression ")" | "!" Factor     
Term := Factor {MultiplicativeOperator Factor}  
SimpleExpression := Term {AdditiveOperator Term}    
Expression := SimpleExpression [RelationalOperator SimpleExpression]    
AssignmentStatement := Identifier ":=" Expression   
IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]  
WhileStatement := "WHILE" Expression "DO" Statement     
ForStatement := "FOR" AssignmentStatement "TO" Expression "DO" Statement    
BlockStatement := "BEGIN" Statement {";" Statement} "END"   
DisplayStatement := "DISPLAY" Expression    
Statement := AssignmentStatement | IfStatement | WhileStatement | ForStatement | BlockStatement | DisplayStatement  
StatementPart := Statement {";" Statement} "."  
DeclarationPart := "[" Identifier {"," Identifier} "]"  
Program := \[DeclarationPart\] StatementPart

