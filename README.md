# Vcompiler for Vascal

A simple compiler.  
From : Pascal-like imperative LL(k) langage  
To : 64 bit 80x86 assembly langage (AT&T)

<br>

**Download the repository :**
> git clone https://github.com/Victor-ONIC/MonCompilateur.git

<br>

**Build the Pascal program's executable with `make` or `make test`, then run it with `./test`**

**Build the compiler executable with `make compilateur`.**

**Reassemble the assembly file if needed with `make asm`.**

**Use `make clean` to remove unnecessary files**

**Use `make cleangit` to keep only source files.**

<br>

# La grammaire du langage
    cf tokeniser.l
       Identifier
       Number
       Boolean
       Float
       Character
       String
    Typename := "UINTEGER" | "BOOLEAN" | "DOUBLE" | "CHAR" | "STRING"
    Constant := Number | Boolean | Float | Character | String
    FunctionCall := Identifier "(" [Expression {"," Expression}] ")"
    ProcedureCall := Identifier "(" [Expression {"," Expression}] ")"
    Factor := "!" Factor | "(" Expression ")" | Identifier | Constant
    MultiplicativeOperator := "*" | "/" | "%" | "&&"
    Term := Factor {MultiplicativeOperator Factor}
    AdditiveOperator := "+" | "-" | "||".
    SimpleExpression := Term {AdditiveOperator Term}
    RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
    Expression := SimpleExpression [RelationalOperator SimpleExpression]
    
    BlockStatement := "BEGIN" Statement {";" Statement} "END"
    AssignmentStatement := Identifier ":=" Expression
    IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
    WhileStatement := "WHILE" Expression "DO" Statement
    RepeatStatement := "REPEAT" Statement "UNTIL" Expression
    ForStatement := "FOR" AssignmentStatement ("TO" | "DOWNTO") Expression "DO" Statement
    DisplayStatement := "DISPLAY" Expression
    DisplaylnStatement := "DISPLAYLN" Expression
    CaseLabelList := Constant {"," Constant}
    CaseElement := CaseLabelList ":" Statement
    CaseStatement := "CASE" Expression "OF" CaseElement {";" CaseElement} [";" "ELSE" Statement] "END"
    Statement := AssignmentStatement | IfStatement | WhileStatement | RepeatStatement | ForStatement | BlockStatement | DisplayStatement | DisplaylnStatement | CaseStatement
    
    VarDeclaration := Identifier {"," Identifier} ":" Type
    VarSection := "VAR" VarDeclaration {";" VarDeclaration}
    ArgumentList := Identifier {"," Identifier} ":" Type
    LocalDeclaration := Identifier {"," Identifier} ":" TYPE
    LocalSection := "VAR" LocalVarDeclaration {";" LocalVarDeclaration}
    Function := "FUNCTION" Identifier "(" [ArgumentList {";" ArgumentList}] ")" ":" Type [[LocalVarSection] BlockStatement]
    FunctionSection := Function {";" Function}
    Procedure := "PROCEDURE" Identifier "(" [ArgumentList {";" ArgumentList}] ")" [[LocalVarSection] BlockStatement]
    ProcedureSection := Procedure {";" Procedure}
    Program := "PROGRAM" Identifier ";" [VarSection "."] [FunctionSection "."] [ProcedureSection "."] BlockStatement "."

<br>

# Avancement

J'ai récupéré votre TP3, au moment où le lexer a été introduit.

Depuis, j'ai implémenté ces fonctions:
- Instructions
    - **IfStatement**
    - **WhileStatement**
    - **RepeatStatement**
    - **ForStatement**
    - **DisplayStatement**
    - **DisplaylnStatement (display line)**
    - **CaseStatement**
    - **CaseElement**
    - **CaseLabelList**
    - **Statement**
- Sous programmes (fonctions et procédures)
    - **Function**
    - **FunctionCall**
    - **Procedure**
    - **ProcedureCall**
    - **ArgumentList**
    - **LocalSection**
    - **LocalDeclaration**
- Sections du programme
    - **Program**
    - **VarSection**
    - **FunctionSection**
    - **ProcedureSection**
- Types UINTEGER, BOOLEAN, DOUBLE, CHAR, et STRING
- Conversions implicites entre les types compatibles

<br>

J'ai pris la décision de créer des classes pour mieux organiser mon code. Les variables
globales, non merci !!!

<br>

# Explications

1. Instructions

Il y a deux instructions d'affichage, pour ou sans retour à la ligne.\
Il est possible que le **display** ne fonctionne pas sur votre machine à cause d'un alignement 
incorrect de la pile.\
Cependant je vous assure que ça fonctionne sur ma machine :)

<br>

2. Types

Le type **STRING** a été implémenté de façon simple. Il est possible de créer et d'assigner 
des variables de type STRING, mais aucune opération ne peut être faite sur des STRINGs.\
L'opération de concaténation requiert l'implémentation concrète de la classe `Stackframe`.\
Cette classe a été créée mais n'a pas encore été implémentée...\
Sachant que cette implémentation servirait également à fix le **display**.\
NB: On distingue les CHARs et les STRINGs par les guillements simples et doubles.

<br>

3. Conversions

La conversion se fait lors d'une expression, d'une assignation, lors d'un appel de fonction, 
et lors d'un retour de fonction.\
Dans le code, ces conversions se présentent sous une condition sur le type de l'expression,
qui mène à la transformation de la valeur.\
Par exemple, toute valeur de chaque type qui ne s'évalue pas à 0 donne TRUE en booléen.
Cette fonctionnalité a été ajoutée afin de pouvoir faire

    FOR a := 1 TO 10 DO
        global_float := global_float + a

avec `a` de type UINTEGER.

