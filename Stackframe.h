#ifndef STACKFRAME_H_
#define STACKFRAME_H_

#include <string>
#include <stdexcept>
#include <iostream>
#include <regex>

class Stackframe {
private:
    unsigned long long pos;
    unsigned long long size;

    bool is_memop(const std::string& operand);
    bool is_full();
    void extend(unsigned long long amount);

public:
    Stackframe();
    void push_stackframe(unsigned long long size);
    void pop_stackframe();
    void push(const std::string& operand);
    void pop(const std::string& operand);
};

#endif
