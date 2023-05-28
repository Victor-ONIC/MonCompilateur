#include "Subroutine.h"

Subroutine::Subroutine(const std::string& name)
    : name(name), return_type((Type)0), defined(false)
{}

Subroutine::Subroutine(const std::string& name, const std::vector<Variable>& args, Type return_type, bool defined)
    : name(name), args(args), return_type(return_type), defined(defined)
{}

const std::string& Subroutine::get_name() const {
    return name;
}

const std::vector<Variable>& Subroutine::get_args() const {
    return args;
}

const std::vector<Variable>& Subroutine::get_local() const {
    return local;
}

void Subroutine::add_local(const Variable& v) {
    local.push_back(v);
}

const Type& Subroutine::get_return_type() const {
    return return_type;
}

const bool Subroutine::get_defined() const {
    return defined;
}

void Subroutine::set_define(bool status) {
    defined = status;
}
