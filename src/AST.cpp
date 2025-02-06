#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include "headers/lexer.h"

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

using namespace std;
namespace { // these unnamed namespaces are accessible within the file they're created in, as if you had an implicit using-clause to them.

/// ExprAST - Base class for all expression nodes.
/*
    Since all values are double 
    precision floating point, the type of each argument 
    doesn’t need to be stored anywhere. 
    In a more realistic language, 
    the “ExprAST” class would probably have a type field.
*/
class ExprAST
{
public:
    virtual ~ExprAST() = default;  
    /* 
        - The virtual keyword here ensures that when a derived class 
        object is deleted through a pointer to the base class, 
        the correct destructor (the destructor of the most derived class) 
        will be called. 
        - 'default' means the compiler will 
        generate a default implementation of the destructor
    */
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST
{
private:
    double Val; // instance variable. Every object of this class has its own copy
public:
    NumberExprAST(double Val) : Val(Val) {} // initialize member variables directly during object construction.
};

/// VariableExprAST - Expression class for referencing a variable, like "x".
class VariableExprAST : public ExprAST
{
private:
    string Name;
public:
    VariableExprAST(const  string &Name) : Name(Name) {}
};

/// BinaryExprAST - Expression class for a binary operator.(+,-,*,/,^)
class BinaryExprAST : public ExprAST
{
private:
    char Op;
    unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op, unique_ptr<ExprAST> LHS, unique_ptr<ExprAST> RHS) 
    : Op(Op), LHS(move(LHS)), RHS(move(RHS)) {} 
    /* 
        move is essentially a cast that transforms an 
        object into an "rvalue reference", which allows for more 
        efficient resource transfer between objects. 
        Think of it as telling the C++ compiler, 
        "I don't need this object anymore, 
        so feel free to steal its resources."
    */
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST
{
private:
     string Callee;
     vector<unique_ptr<ExprAST>> Args;
public:
    CallExprAST(string Callee, vector<unique_ptr<ExprAST>> Args) 
    : Callee(Callee), Args( move(Args)) {}
};


/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
/// Think of extern
class PrototypeAST
{
private:
    string Name;
    vector<string> Args;
public:
    PrototypeAST(string Name, vector<string>(Args)) 
    : Name(Name), Args(std::move(Args)) {}
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST 
{
private:
    unique_ptr<PrototypeAST> Proto;
    unique_ptr<ExprAST> Body;
public:
    FunctionAST(unique_ptr<PrototypeAST> Proto, unique_ptr<ExprAST> Body)
    : Proto(move(Proto)), Body(move(Body)) {}
};
} // end of anonymous namespace

