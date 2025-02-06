#include <iostream>
#include <memory>
#include <vector>
#include <map>
#include "headers/lexer.h"
#include "headers/parser.h"
#include "AST.cpp"

/**
 * Remember when reading this code that parsing is like a binary tree. 
 * You follow the branches as low as you can go and evaluate the way back up
 */

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

using namespace std;

static std::unique_ptr<ExprAST> ParseExpression(); // forward declaration

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
// we evaluate each token 1 at a time
static int getNextToken() 
{
    return CurTok = gettok();
}

/// LogError* - These are little helper functions for error handling.
unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}

unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
}

/// numberexpr ::= number
static unique_ptr<ExprAST> ParseNumberExpr()
{
    auto Result = make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consumes(passes) the number
    return move(Result);
}

/// parenexpr ::= '(' expression ')'
static unique_ptr<ExprAST> ParseParenExpr()
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
static unique_ptr<ExprAST> ParseIdentifierExpr()
{
    // checks if stand alone variable reference or if it is a function call expression. 
    string IdName = IdentifierStr;

    getNextToken(); // eat identifier.

    /* 
        for a second i thought this would cause errors but remember
        we are dealing with not a strings of text but a list of tokens. 
        a function name wouldn't be like: func(a+b); 
        it would be ['func', '(','a', '+', 'b', ')', ';']
    */
    if (CurTok != '(') // Simple variable ref.
        return make_unique<VariableExprAST>(IdName); 
    // Call
    getNextToken(); // eat (
    vector<unique_ptr<ExprAST>> Args;
    if (CurTok != ')')
    {
        while ( true)
        {
            if (auto Arg = ParseExpression()) // concat arguments 
                Args.push_back(move(Arg));
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

    return make_unique<CallExprAST>(IdName, move(Args));
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
static unique_ptr<ExprAST> ParsePrimary() 
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

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static map<char, int> BinopPrecedence;

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

/// binoprhs
///   ::= ('+' primary)*
/*
    /// @brief For example, if the current pair stream is 
    ///   [+, x] and ParseBinOpRHS is passed in a precedence of 40, 
    ///   it will not consume any tokens (because the precedence of ‘+’ is only 20).
*/
/// @brief Because we defined invalid tokens to have a precedence of -1, 
///  this check implicitly knows that the pair-stream ends when the 
///  token stream runs out of binary operators. If this check succeeds, 
///  we know that the token is a binary operator and that it will be included in this expression
/// @param ExprPrec is the minimal operator precedence the func is allowed to eat 
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
        LHS = std::make_unique<BinaryExprAST>(BinOp, (std::move(LHS), std::move(RHS)));
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
    if (!LHS)
    return nullptr;

    return ParseBinOpRHS(0, std::move(LHS));  // TODO - fix error in declaration order.
    }

/// @brief TODO: Extend supported operations 
int main()
{   
    // Install standard binary operators.
    // 1 is the lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;  // highest.
}

/// prototype
///   ::= id '(' id* ')' 
static unique_ptr<PrototypeAST> ParsePrototype()
{
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype.");

    string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(')
        LogErrorP("Expected '(' in prototype.");

    // Read the list of argument names.
    vector<string> ArgNames;
    while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr); // what is FnName for then?
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype.");
    
    // success
    getNextToken(); // eat ')'.

    return make_unique<PrototypeAST>(FnName, move(ArgNames));
}

/// definition ::= 'def' prototype expression
static unique_ptr<FunctionAST> ParseDefinition()
{
    getNextToken(); // eat def
    auto Proto = ParsePrototype();
    if (!Proto)
        return nullptr;

    if (auto E = ParseExpression())
        return make_unique<FunctionAST>(move(Proto), move(E));
    return nullptr;
}

/// external ::= 'extern' prototype
/// To support forward declaration of user functions. 
/// These ‘extern’s are just prototypes with no body
static unique_ptr<PrototypeAST> ParseExtern()
{
    getNextToken(); // eat extern
    return ParsePrototype(); // essentially treating it as any other function
}

/// toplevelexpr ::= expression
static unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto E = ParseExpression())
    {
        // Make an anonymous Proto
        auto Proto = make_unique<PrototypeAST>("", vector<string>());
        return make_unique<FunctionAST>(move(Proto), move(E));
    }
    return nullptr;
}

////////////////////////////////////////////////////////////////////////////////////////////////
///
///                                 Top Level Parsing
///
////////////////////////////////////////////////////////////////////////////////////////////////


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
                getNextToken(); // ignore the ';'
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