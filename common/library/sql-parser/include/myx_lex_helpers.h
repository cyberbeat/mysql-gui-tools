#ifndef myx_lex_helpers_h
#define myx_lex_helpers_h

#include <iostream>
#include <fstream>

extern int yyparse();
extern void yyerror(const char *msg);

extern std::istream* lex_input_stream;
//extern HANDLE h_file;


extern "C" {

extern void *tree;
extern FILE *yyin;

extern int yylex(char **);
extern int yywrap();
extern void yy_custom_input(char *buf, int* result, int max_size);
extern int yy_token_match(int token, const char *value);
int yy_unknown_token(const char *value);

} // extern C


#endif // myx_lex_helpers_h

