#include <stdio.h>
#include <sstream>
#include "myx_sql_parser_public_interface.h"
#include "myx_lex_helpers.h"

void yyerror(const char *msg) { printf("error %s met\n", msg); }

std::istream* lex_input_stream= 0;

MYX_PUBLIC_FUNC void *myx_get_parser_tree()
{
  return tree;
}

MYX_PUBLIC_FUNC void myx_set_parser_input(std::istream *sqlstream)
{
  lex_input_stream = sqlstream;
}

MYX_PUBLIC_FUNC void myx_set_parser_source(const char *sql)
{
  lex_input_stream = new std::istringstream(sql);
}

MYX_PUBLIC_FUNC void myx_set_parser_source(std::istream *sqlstream)
{
  lex_input_stream = sqlstream;
}

MYX_PUBLIC_FUNC void myx_free_parser_source(void)
{
  delete lex_input_stream;
}

MYX_PUBLIC_FUNC void myx_parse(void)
{
  yyparse();
}

extern "C" {

int yywrap() { return 1; }  // strop after EOF

void yy_custom_input(char *buf, int* result, int max_size) 
{
  lex_input_stream->read(buf, max_size);
  *result= lex_input_stream->gcount();
}

int yy_token_match(int token, const char *value)
{
  //printf("lexer token %s\n", value);
  return token;
}

int yy_unknown_token(const char *value)
{
  //printf("error %s", value);
  return 0;
}

} // extern C
