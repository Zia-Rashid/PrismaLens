#include "../include/Kaleidoscope.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token
{
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr;   // Filled in if tok_identifier
static double NumVal;               // Filled in if tok_number

///gettok - Return the next token from standard input.
static int gettok() 
{
    static int LastChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar))
        LastChar = getchar();

    if (isalpha(LastChar))      // identifier: [a-zA-Z][a-zA-Z0-9]*
    {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

        // keywords \/
        if (IdentifierStr == "def")             // TODO: extend this to contain more keywords in the future.
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        return tok_identifier;   
    }

    if (isdigit(LastChar) || LastChar == '.') // Number: [0-9.]+
    {   
        std::string NumStr;
        do
        {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;  
    }

    if (LastChar == '#')
    {   // Comment until end of line.
        do
        {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        
        if (LastChar != EOF)
            return gettok();
    }
    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF)
        return tok_eof;

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;    
}



//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

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
    virtual Value *codegen() = 0; 
    /* 
        - The virtual keyword here ensures that when a derived class 
        object is deleted through a pointer to the base class, 
        the correct destructor (the destructor of the most derived class) 
        will be called. 
        - 'default' means the compiler will 
        generate a default implementation of the destructor
    */
};
/// the codegen() method says to emit IR for that AST node along with all the things it depends on, 

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST
{
private:
    double Val; // instance variable. Every object of this class has its own copy
public:
    NumberExprAST(double Val) : Val(Val) {} // initialize member variables directly during object construction.
    Value *codegen() override;              // “Value” is the class used to represent a “Static Single Assignment (SSA) register” 
};

/// VariableExprAST - Expression class for referencing a variable, like "x".
class VariableExprAST : public ExprAST
{
private:
    std::string Name;
public:
    VariableExprAST(const  std::string &Name) : Name(Name) {}
    Value *codegen() override; 
};

/// BinaryExprAST - Expression class for a binary operator.(+,-,*,/,^)
class BinaryExprAST : public ExprAST
{
private:
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) 
    : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {} 
    Value *codegen() override; 
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
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    CallExprAST(std::string Callee, std::vector<std::unique_ptr<ExprAST>> Args) 
    : Callee(Callee), Args(std::move(Args)) {}
    Value *codegen() override; 
};


/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
/// Think of extern
class PrototypeAST
{
private:
    std::string Name;
    std::vector<std::string> Args;
public:
    PrototypeAST(std::string Name, std::vector<std::string>(Args)) 
    : Name(Name), Args(std::move(Args)) {}
    Function *codegen();
    const std::string &getName() const // string won't change and result won't change. i.e permenant
    {
        return Name;
    }
};

/// FunctionAST - This class represents a function definition itself.
/*
    This code does have a bug, though: If the FunctionAST::codegen() method finds an existing IR Function, it does not validate its signature against the definition’s own prototype. This means that an earlier ‘extern’ declaration will take precedence over the function definition’s signature, which can cause codegen to fail, for instance if the function arguments are named differently. There are a number of ways to fix this bug, see what you can come up with! Here is a testcase:
    Idea: make the attribute weak here, so that they can be overwritten. unsure if this would work though given that when they are parsed into symbols and added to the 'TheModule' there is no difference between weak and strong, it is just there.
    To fix this, you could:
        Make the compiler check that the parameter names in the extern and def match
        Have the def override the extern's parameter names
        Report an error if a def tries to use different parameter names than an existing extern
*/
class FunctionAST 
{
private:
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;
public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body)
    : Proto(std::move(Proto)), Body(std::move(Body)) {}
    Function *codegen();
};
} // end of anonymous namespace


/**
 * Remember when reading this code that parsing is like a binary tree. 
 * You follow the branches as low as you can go and evaluate the way back up
 */

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
// we evaluate each token 1 at a time
static int getNextToken() 
{
    return CurTok = gettok();
}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence()
{
    if (!(isascii(CurTok)))
        return -1;
    
    // Make sure it's a declared binary
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) 
        return -1; // not a BinOp
    return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression(); // forward declaration

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr()
{
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consumes(passes) the number
    return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr()
{
    // We have to treat it like a subprocess of normal evaluation. 
    getNextToken(); // eat(.
    auto V = ParseExpression();
    if (!V)
        return nullptr;
    
    if (CurTok != ')')
        return LogError("expected ')'");
    getNextToken(); // eat ).
    return V;  
    // (a+b) -> a+b     the only difference is precedence.
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr()
{
    // checks if stand alone variable reference or if it is a function call expression. 
    std::string IdName = IdentifierStr;

    getNextToken(); // eat identifier.

    /* 
        for a second i thought this would cause errors but remember
        we are dealing with not a strings of text but a list of tokens. 
        a function name wouldn't be like: func(a+b); 
        it would be ['func', '(','a', '+', 'b', ')', ';']
    */
    if (CurTok != '(') // Simple variable ref.
        return std::make_unique<VariableExprAST>(IdName); 
    // Call
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')')
    {
        while ( true)
        {
            if (auto Arg = ParseExpression()) // concat arguments 
                Args.push_back(std::move(Arg));
            else    
                return nullptr;
            
            if (CurTok == ')')  // should have broken by now 
                break;
            if (CurTok != ',')  // unless it is this
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    // Eat the ')'.
    getNextToken();

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
/*
    Now that you see the definition of this function, 
    it is more obvious why we can assume the state of 
    CurTok in the various functions. 
    This uses look-ahead to determine which sort of 
    expression is being inspected, 
    and then parses it with a function call.
*/
static std::unique_ptr<ExprAST> ParsePrimary() 
{
    switch (CurTok)
    {
        default:
            return LogError("Unknown token when expecting an expression");
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
    }
}

/// binoprhs
///   ::= ('+' primary)*
/*
    /// @brief For example, if the current pair stream is 
    ///   [+, x] and ParseBinOpRHS is passed in a precedence of 40, 
    ///   it will not consume any tokens (because the precedence of ‘+’ is only 20).
*/
/*  @brief Because we defined invalid tokens to have a precedence of -1, 
///  this check implicitly knows that the pair-stream ends when the 
///  token stream runs out of binary operators. If this check succeeds, 
///  we know that the token is a binary operator and that it will be included in this expression 
/// @param ExprPrec is the minimal operator precedence the func is allowed to eat */
static std::unique_ptr<ExprAST> ParseBinOpsRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) 
{   // we parse the RHS because we work from left to right. we go down the left side completely before moving right

    // If this is a binop, find its precedence
    while (true)
    {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS; 
            // this means we are at the end of parsing this token. 
        
        // this code eats (and remembers) the binary operator 
        // and then parses the primary expression that follows
        int BinOp = CurTok;
        getNextToken(); // eat binop

        // Parse the primary expression after the binary operator.
        auto RHS = ParsePrimary();
        if(!(RHS))
            return nullptr;

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        // a + b * c    --->    b * c + a   
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec)
        {
            // In our example, the current operator is “+” and the next operator is “+”, 
            // we know that they have the same precedence. 
            // In this case we’ll create the AST node for “a+b”, and then continue parsing
            RHS = ParseBinOpsRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }

        // Merge LHS/RHS
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    } // loop to the top of the while loop

    /**
     * In our code above, this will turn “a+b+” into “(a+b)” 
     * and execute the next iteration of the loop, with “+” as the current token.
     */
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() 
{
    auto LHS = ParsePrimary();
    if (!(LHS))
    return nullptr;

    return ParseBinOpsRHS(0, std::move(LHS));  // TODO - fix error in declaration order.
    }

/// prototype
///   ::= id '(' id* ')' 
static std::unique_ptr<PrototypeAST> ParsePrototype()
{
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype.");

    std::string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(')
        LogErrorP("Expected '(' in prototype.");

    // Read the list of argument names.
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr); // what is FnName for then?
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype.");
    
    // success
    getNextToken(); // eat ')'.

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition()
{
    getNextToken(); // eat def
    auto Proto = ParsePrototype();
    if (!Proto)
        return nullptr;

    if (auto E = ParseExpression())
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

/// external ::= 'extern' prototype
/// To support forward declaration of user functions. 
/// These ‘extern’s are just prototypes with no body
static std::unique_ptr<PrototypeAST> ParseExtern()
{
    getNextToken(); // eat extern
    return ParsePrototype(); // essentially treating it as any other function
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto E = ParseExpression())
    {
        // Make an anonymous Proto
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}


//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value *LogErrorV(const char* str)
{
    LogError(str);
    return nullptr;
}

Function *getFunction(std::string Name)
{
    // First check if func has already been added tot he module
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // if not, check for existing prototype to codegen from
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();
    
        // if no existing prototypes, return null
        return nullptr;
}

Value *NumberExprAST::codegen()
{
    /*
        LLVM IR, numeric constants are represented 
        with the ConstantFP class, which holds the 
        numeric value in an APFloat internally
    */
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen()
{
    // Look this variable up in the definition
    Value *V = NamedValues[Name];
    if (!(V))
        return LogErrorV("Unknown variable name");
    return V;
}

Value *BinaryExprAST::codegen()
{
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();  
    if (!L || !R)
        return nullptr;
    
    /*
        IRBuilder knows where to insert the newly 
        created instruction, all you have to do is 
        specify what instruction to create (e.g. with CreateFAdd)
    */
    switch (Op)
    {
        case '+':
            return Builder->CreateFAdd(L, R, "addtmp");
        case '-':
            return Builder->CreateFSub(L, R, "subtmp");
        case '*':
            return Builder->CreateFMul(L, R, "multmp");
        case '<':
            L = Builder->CreateFCmpULT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.1
            return Builder-> CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
        default:
            return LogErrorV("invalid bianry operator");
    }
}

Value *CallExprAST::codegen()
{
    // Look up function name in global module table
    // By giving each function the same name as what the user specifies, 
    // we can use the LLVM symbol table to resolve function names for us.
    Function *CalleeF = TheModule->getFunction(Callee);
    if (!(CalleeF))
        return LogErrorV("Unknown function referenced");
    
    // If argument mismatch error
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");
    
    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; i++)
    {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back()) // if it is still empty
            return nullptr;
    }
    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

/*
    Note first that this function returns a “Function*” 
    instead of a “Value*”. Because a “prototype” really talks 
    about the external interface for a function 
    (not the value computed by an expression)
*/
Function *PrototypeAST::codegen()
{
    // Make the function type: double(double, double) etc...
    std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext)); // if i am correct retrieving the type double from LLVM. Like a wrapper for the original data type
    FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);
    /*
        FunctionType::get creates the FunctionType that should 
        be used for a given Prototype. Since all function 
        arguments in Kaleidoscope are of type double, 
        the first line creates a vector of “N” LLVM double types. 
    */

    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set names for all arguments.
    /*
        Finally, we set the name of each of the function’s 
        arguments according to the names given in the Prototype. 
        This step isn’t strictly necessary but it keeps the names 
        consistent and makes the IR more readable.
        It allows subsequent code to refer directly to the arguments for their names, 
        rather than having to look up them up in the Prototype AST
    */
    unsigned Idx = 0;           
    for (auto &Arg : F->args())   // This l
        Arg.setName(Args[Idx++]);
    
    return F;
}

Function *FunctionAST::codegen()
{

    // transfer ownership of the prototypes to the FunctionProtos map, but keep 
    // a reference for use below
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *TheFunction = getFunction(P.getName());

    if (!TheFunction)                  
        return nullptr;

    // Create a new basic block to start insertion into.
    // when you call the proto above the below codegen'd body will execute
    /*
        The first line creates a new basic block (named “entry”), 
        which is inserted into TheFunction. 
        The second line then tells the builder that new instructions 
        should be inserted into the end of the new basic block.
    */
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the NamedValues Map
    NamedValues.clear();           // not sure why we clear
    for (auto &Arg : TheFunction->args()) 
        NamedValues[std::string(Arg.getName())] = &Arg;
    
    if (Value *RetVal = Body->codegen()) // fill body
    {
        // Finish off the function
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency; can catch a lot of bugs
        verifyFunction(*TheFunction);

        TheFPM->run(*TheFunction, *TheFAM); // pass the function IR for analysis, which will lead to more calls later.

        return TheFunction;
    }

    // Error reading body, remove function
    TheFunction->eraseFromParent();     
    /*
        This allows the user to redefine a function that they 
        incorrectly typed in before: if we didn’t delete it, 
        it would live in the symbol table, with a body, 
        preventing future redefinition.
    */
    return nullptr;
}
/*
    Function *FunctionAST::codegen() {
  // First, check for an existing function from an 'extern' declaration.
  Function *TheFunction = TheModule->getFunction(Proto->getName());
  
  if (TheFunction) {
    // Function already exists - need to validate parameter names match
    if (TheFunction->arg_size() != Proto->getArgs().size())
      return LogErrorV("Function redefinition with different # args");
    
    // Check each parameter name matches
    unsigned Idx = 0;
    for (auto &Arg : TheFunction->args()) {
      if (Arg.getName() != Proto->getArgs()[Idx])
        return LogErrorV("Function redefinition with different argument names");
      Idx++;
    }
  } else {
    // No existing function, create a new one
    TheFunction = Proto->codegen();
  }
  
  if (!TheFunction)
    return nullptr;
  // ... rest of the codegen implementation ...
}
*/


//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndManagers()
{   // Open a new context and module.
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    // Create new pass and analysis managers
    TheFPM = std::make_unique<FunctionPassManager>();
    TheLAM = std::make_unique<LoopAnalysisManager>();
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    TheMAM = std::make_unique<ModuleAnalysisManager>();
    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(*TheContext, 
                                                                /* Debug logging */ true);

    TheSI->registerCallbacks(*ThePIC, TheMAM.get());

    // Add transform passes
    // Do simple 'peephole' optimizations and bit-twiddling optzns
    TheFPM->addPass(InstCombinePass());
    // reassociate expressions
    TheFPM->addPass(ReassociatePass());
    // Eliminate common subexpresions
    TheFPM->addPass(GVNPass());
    // Simplify control flow graph(remove unreachable code)
    TheFPM->addPass(SimplifyCFGPass());
    // Register the analysis passes used in the transform passes
    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static void HandleDefinition()
{
    if (auto FnAST = ParseDefinition())   // parsing
    {                               
        if(auto *FnIR = FnAST->codegen()) // IR gen
        { 
            fprintf(stderr, "Read function definition:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            ExitOnErr(TheJIT->addModule(
                                ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            InitializeModuleAndManagers();
        }
    }
    else
        getNextToken();
        // Skip token for error recovery
}

static void HandleExtern()
{
    if (auto ProtoAST = ParseExtern())
    {
        if(auto *FnIR = ProtoAST->codegen())
        {
            fprintf(stderr, "Read extern:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    }
    else
        getNextToken();
        // Skip token for error recovery
}

static void HandleTopLevelExpression() 
{
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr())
    {
        if (auto *FnIR = FnAST->codegen())
        {
            // Create a ResourceTracker to track JIT'd memory allocated
            // to our anonymous expression --- that way we 
            // can free it after executing
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();

            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            InitializeModuleAndManagers();

            // Search the JIT for __anon_expr symbol
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

            // Get the symbol's address and cast it to the right type (takes no 
            // arguments, returns a double) so we can call it as a native function
            double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
            fprintf(stderr, "Evaluated to %f\n", FP());

            // delete the anonymous expression module from the JIT
            ExitOnErr(RT->remove());
        }
    }
    else    
        getNextToken();
        // Skip token for error recovery
}

/// top ::= definition | external | expression | ';'
static void MainLoop()
{
    while (true)
    {
        fprintf(stderr, "ready> ");
        switch (CurTok)
        {
            case tok_eof:
                return;
            case ';':           // Having top-level semicolons allows you to type “4+5;”, and the parser will know you are done.
                getNextToken(); // ignore the top-level ';'
                break;
            case tok_def:
                HandleDefinition(); 
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                HandleTopLevelExpression();
                break;
        }
    }
}


//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0
extern "C" DLLEXPORT double putchard(double X)
{
    fputc((char)X, stderr);
    return 0;
}

/// printd - printf that takes a double as "%f\n", and returns 0
extern "C" DLLEXPORT double printd(double X)
{
    fprintf(stderr, "%f\n", X);
    return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

/// @brief TODO: Extend supported operations 
int main()
{   
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();


    // Install standard binary operators.
    // 1 is the lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;  // highest.

    // Prime the first token
    fprintf(stderr, "ready> ");
    getNextToken();

    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    // Make the module which holds all of the code
    InitializeModuleAndManagers();

    // Run the main interpreter loop now
    MainLoop();

    // // Print out all of the generated code
    // TheModule->print(errs(), nullptr);

    return 0;
}


