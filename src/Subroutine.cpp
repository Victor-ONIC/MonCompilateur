#include "Subroutine.h"

Subroutine::Subroutine(const std::string& name)
    : name(name), return_type((Type)0), defined(false)
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

const Type& Subroutine::get_return_type() const {
    return return_type;
}

const bool Subroutine::get_defined() const {
    return defined;
}

