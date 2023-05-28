#ifndef SUBROUTINE_H_
#define SUBROUTINE_H_

#include <string>
#include <vector>
#include "Variable.h"
#include "tokeniser.h"
#include "main.h"

class Subroutine {
private:
    std::string name;
    std::vector<Variable> args;
    std::vector<Variable> local;
    Type return_type;
    bool defined;

public:
    Subroutine(const std::string& name);
    Subroutine(const std::string& name, const std::vector<Variable>& args, Type return_type, bool defined);

    const std::string& get_name() const;
    const std::vector<Variable>& get_args() const;
    const std::vector<Variable>& get_local() const;
    void add_local(const Variable& v);
    const Type& get_return_type() const;
    const bool get_defined() const;
    void set_define(bool status);

    bool operator==(const Subroutine& other) const {
        return name == other.name;
    }
};

#endif
