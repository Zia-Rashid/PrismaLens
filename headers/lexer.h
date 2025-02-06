#ifndef LEXER_H
#define LEXER_H
#include <string>

// ask sharad if this being declared twice is a waste of space
// also ask about imports in header vs source

// Token enumeration
enum Token { 
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5
};

// Global variables for token processing
extern std::string IdentifierStr;
extern double NumVal;

// Function to get the next token
int gettok();

#endif // LEXER_H
