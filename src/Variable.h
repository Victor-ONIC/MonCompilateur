#ifndef VARIABLE_H_
#define VARIABLLE_H_

#include <string>
#include "tokeniser.h"

class Variable {
private:
    std::string name;
    Type type;

public:
    Variable(const std::string& name);

    const std::string& get_name() const;
    void set_name(const std::string& s);

    const Type& get_type() const;
    void set_type(const Type& t);
};

#endif
