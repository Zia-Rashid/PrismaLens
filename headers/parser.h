/**
 * Parsing Process:
 *      Lexical analysis (tokenization)
 *      Syntax parsing
 *      AST generation
 *      Code transformation and optimization
 * 
 */


#ifndef PARSER_H
#define PARSER_H

#include <memory>
#include <map>
#include "lexer.h"
#include "../src/AST.cpp"  // is this unnecessary?


std::unique_ptr<ExprAST> ParseExpression;
std::unique_ptr<ExprAST> LogError;
std::unique_ptr<PrototypeAST> LogErrorP;
std::unique_ptr<ExprAST> ParseNumberExpr;
std::unique_ptr<ExprAST> ParseParenExpr;
std::unique_ptr<ExprAST> ParseIdentifierExpr;
std::unique_ptr<ExprAST> ParsePrimary; 
std::map<char, int> BinopPrecedence;
int GetTokPrecedence();
std::unique_ptr<ExprAST> ParseBinOpsRHS;
std::unique_ptr<ExprAST> ParseExpression;
std::unique_ptr<PrototypeAST> ParsePrototype;
std::unique_ptr<FunctionAST> ParseDefinition;
std::unique_ptr<PrototypeAST> ParseExtern;
std::unique_ptr<FunctionAST> ParseTopLevelExpr;

#endif  