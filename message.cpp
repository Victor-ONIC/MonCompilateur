#include <string>
#include <regex>

using namespace std;

/*
    This class is used to generate the instructions related to the stack frame of a function.
    @note uses %rbp as a base pointer
    @note the stack assume that the stack is aligned on 16 bytes when instanciated
    @note %rax can be overwritten by the mehods
*/
class StackFrame
{
  private:
    int currentPosition = 0;
    int size = NULL;
    /*
        Checks if the operand is a memory operand
        @param operand: the operand to check
        @return true if the operand is a memory operand, false otherwise
    */
    bool is_memory_operand(const char *operand)
    {
        regex re("^[^$%].*"); // Regex to check if the operand is a memory operand
        string operand_str = string(operand);
        if (regex_match(operand_str, re))
        {
            return true;
        }
        return false;
    }

  public:
    /*
        Default constructor
    */
    StackFrame()
    {
        this->currentPosition = 0;
        this->size = 0;
    }

    /*
        Constructor by copy
        @param other: an instance of StackFrame to copy from
    */
    StackFrame(const StackFrame &other)
    {
        this->currentPosition = other.currentPosition;
        this->size = other.size;
    }

    /*
        Generates the instructions to initialize (allocate) the stack frame of a function.
        (pushes %rbp, moves %rsp to %rbp, and substracts the size (passed in argument) from %rsp to allocate the stack
       frame)
        @param size: the desired size of the stack frame (in number of bytes). It needs to be a multiple of 16 to keep
       the stack aligned.
        @return the assembly instructions as a string
        @throw invalid_argument if the size is not a multiple of 16
    */
    string allocate(int size)
    {
        this->size = size;
        if (size % 16 != 0) // We want to keep the stack aligned on 16 bytes
        {
            throw invalid_argument("The size needs to be a multiple of 16");
        }
        string instruction = string("\tpushq %rbp\n");
        instruction += string("\tmovq %rsp, %rbp\n");
        instruction += string("\tsubq $") + to_string(size) + ", %rsp";
        return instruction;
    }

    /*
        Generates the instructions to free the stack frame
        @return the assembly instructions as a string
    */
    string free()
    {
        string instruction = string("\tmovq %rbp, %rsp\n");
        instruction += string("\tpopq %rbp");
        return instruction;
    }

    /*
        Generates the instructions to push based on the stack frame
        @warning %rax can be used as a temporary register if the from parameter is a memory operand
        @param from: source of the push (register, memory, immediate value)
        @return the assembly instructions as a string
    */
    string push(const char *from)
    {
        string instruction;
        this->currentPosition += 8;
        if (this->is_memory_operand(from))
        {
            instruction = string("\tmovq ") + string(from) + ", %rax\n ";
            instruction += string("\tmovq %rax, ") + to_string(-currentPosition) + "(%rbp)";
        }
        else
        {
            instruction = string("\tmovq ") + string(from) + ", " + to_string(-currentPosition) + "(%rbp)";
        }
        return instruction;
    }

    /*
        Generates the instructions to pop based on the stack frame
        @warning %rax can be used as a temporary register if the from parameter is a memory operand
        @param to: destination of the pop
        @return the assembly instructions as a string
    */
    string pop(const char *to)
    {
        string instruction;
        if (this->is_memory_operand(to))
        {
            instruction = string("\tmovq ") + to_string(-currentPosition) + string("(%rbp)") + ", %rax\n";
            instruction += string("\tmovq ") + "%rax, " + string(to);
        }
        else
        {
            instruction = string("\tmovq ") + to_string(-currentPosition) + "(%rbp), " + string(to);
        }
        this->currentPosition -= 8;
        return instruction;
    }

    string push(string from)
    {
        return this->push(from.c_str());
    }

    string pop(string to)
    {
        return this->pop(to.c_str());
    }

    /*
        Get the operand to use in order to access the value at the given position in the stack frame
        @param position: the position (positive) of the value (needs to be a multiple of 8)
        @return the value at the given position
        @throws invalid_argument if the position is not a multiple of 8
    */
    string get_value_at_position(int position)
    {
        if (position % 8 != 0)
        {
            throw invalid_argument("The position needs to be a multiple of 8");
        }
        return to_string(-position) + "(%rbp)";
    }

    /*
        Get the current position of the stack frame (in bytes)
        @return the current position of the stack frame
    */
    int get_current_position()
    {
        return this->currentPosition;
    }

    /*
        Set the current position of the stack frame (in bytes)
        @param position: the new position of the stack frame (in bytes) (can be relative to the current position with
       getCurrentPosition)
        @return the new position of the stack frame
    */
    int setCurrentPosition(int position)
    {
        if (position % 8 != 0)
        {
            throw invalid_argument("The position needs to be a multiple of 8");
        }
        this->currentPosition = position;
    }
};
