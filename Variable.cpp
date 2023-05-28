#include "Variable.h"

Variable::Variable(const std::string& name)
    : name(name), type((Type)0)
{}

Variable::Variable(const std::string& name, const Type& type)
    : name(name), type(type)
{}

const std::string& Variable::get_name() const {
    return name;
}

void Variable::set_name(const std::string& s) {
    name = s;
}

const Type& Variable::get_type() const {
    return type;
}

void Variable::set_type(const Type& t) {
    type = t;
}
