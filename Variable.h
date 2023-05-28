#ifndef VARIABLE_H_
#define VARIABLE_H_

#include <string>
#include "tokeniser.h"
#include "main.h"

class Variable {
private:
    std::string name;
    Type type;

public:
    Variable(const std::string& name);
    Variable(const std::string& name, const Type& type);

    const std::string& get_name() const;
    void set_name(const std::string& s);

    const Type& get_type() const;
    void set_type(const Type& t);

    bool operator==(const Variable& other) const {
        return name == other.name;
    }
};

#endif
