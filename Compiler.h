#ifndef COMPILER_H_
#define COMPILER_H_

#include <iostream>
#include <string>
#include <cstring>
#include <vector>
#include <sstream>
#include <algorithm>
#include <FlexLexer.h>
#include "Subroutine.h"
#include "Variable.h"
#include "tokeniser.h"
#include "main.h"

class Compiler {
private:
    // typedefs ?
    // unsigned long long ---> llu
    // const std::vector<Compiler::Variable>& ---> arg_list_t
    // const std::vector<Compiler::Variable>& ---> local_list_t

    yyFlexLexer* lexer;
    Token current_token;
    unsigned long long tag_number;
    std::stringstream internal_data;

    std::vector<Variable> variables;
    std::vector<Subroutine> subroutines;

    bool in_subroutine;
    std::string current_subroutine;

    Variable& variable(const std::string& var_name);
    const Variable& variable(const std::string& var_name) const; // used?
    Subroutine& subroutine(const std::string& subroutine_name);
    const Subroutine& subroutine(const std::string& subroutine_name) const;

    void error(const std::string& message) const;
    // void write(const std::string& line) const;
    // const std::string current_text() const;
    void read();
    void read_keyword(const std::string& keyword);

    bool is_integral(const Type& type) const;
    bool is_var_declared(const std::string& var_name) const;
    bool is_subroutine_declared(const std::string& subroutine_name) const;
    bool is_subroutine_defined(const std::string& subroutine_name) const;
    bool is_argument(const std::string& name, const std::string& subroutine_name) const;  

    Type Identifier();
    Type Number();
    Type Boolean();
    Type Float();
    Type Character();
    Type String();
    Type Typename();
    Type Constant();

    Type FunctionCall(const std::string& function_name);
    void ProcedureCall(const std::string& procedure_name);
    Type Factor();
    Opmul MultiplicativeOperator();
    Type Term();
    Opadd AdditiveOperator();
    Type SimpleExpression();
    Oprel RelationalOperator();
    Type Expression();

    void BlockStatement();
    void AssignmentStatement();
    void IfStatement();
    void WhileStatement();
    void RepeatStatement();
    void ForStatement();
    void DisplayStatement();    //
    void DisplaylnStatement();  //
    Type CaseLabelList();
    Type CaseElement(unsigned long long end_tag_number);
    void CaseStatement();
    void Statement();

    void VarDeclaration();  //
    void VarSection();  //
    void ArgumentList(const std::string& subroutine_name, std::vector<Variable>& args);
    void LocalDeclaration(const std::string& subroutine_name);
    void LocalSection(const std::string& subroutine_name);
    void Function();
    void FunctionSection();
    void Procedure();
    void ProcedureSection();
    void Program();

public:
    Compiler();
    ~Compiler();
    void run();
};

#endif
