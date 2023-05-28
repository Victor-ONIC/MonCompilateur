#ifndef SUBROUTINE_H_
#define SUBROUTINE_H_

#include <string>
#include <vector>
#include "Variable.h"
#include "tokeniser.h"

class Subroutine {
private:
    std::string name;
    std::vector<Variable> args;
    std::vector<Variable> local;
    Type return_type;
    bool defined;

public:
    Subroutine(const std::string& name);

    const std::string& get_name() const;
    const std::vector<Variable>& get_args() const;
    const std::vector<Variable>& get_local() const;
    const Type& get_return_type() const;
    const bool get_defined() const;
};

#endif
