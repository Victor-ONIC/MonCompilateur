#include "Stackframe.h"

Stackframe::Stackframe()
    : pos(0), size(0)
{}

bool Stackframe::is_memop(const std::string& operand) {
    const std::regex re("^[^$%].*");
    if (std::regex_match(operand, re)) {
        return true;
    }
    return false;
}

bool Stackframe::is_full() {
    return pos >= size;
}

void Stackframe::extend(unsigned long long amount) {
    size = amount;
    if (amount % 16 != 0) {
        throw std::invalid_argument("Invalid stackframe extension size!");
    }
    std::cout << "\tsubq\t$" << amount << ", %rsp" << std::endl;
}

void Stackframe::push_stackframe(unsigned long long amount) {
    size = amount;
    if (amount % 16 != 0) {
        throw std::invalid_argument("Invalid stackframe size!");
    }
    std::cout << "\tpushq\t%rbp"                 << std::endl;
    std::cout << "\tmovq\t%rsp, %rbp"            << std::endl;
    std::cout << "\tsubq\t$" << amount << ", %rsp" << std::endl;
}

void Stackframe::pop_stackframe() {
    std::cout << "\tmovq\t%rbp, %rsp" << std::endl;
    std::cout << "\tpopq\t%rbp"       << std::endl;
}

void Stackframe::push(const std::string& operand) {
    if (is_full()) {
        extend(64);  // 
    }
    pos -= 8;
    if (is_memop(operand)) {
        std::cout << "\tmovq\t" << operand << ", %rax"   << std::endl;
        std::cout << "\tmovq\t%rax, " << pos << "(%rbp)" << std::endl;
    } else {
        std::cout << "\tmovq\t" << operand << ", " << pos << "(%rbp)" << std::endl;
    }
}

void Stackframe::pop(const std::string& operand) {
    if (is_memop(operand)) {
        std::cout << "\tmovq\t" << pos << "(%rbp), %rax" << std::endl;
        std::cout << "\tmovq\t%rax, " << operand         << std::endl;
    } else {
        std::cout << "\tmovq\t" << pos << "(%rbp), " << operand << std::endl;
    }
    pos += 8;
}
