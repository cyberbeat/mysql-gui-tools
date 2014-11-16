#ifndef statement_parser_h
#define statement_parser_h

#include <string>
#include "myx_sql_parser_public_interface.h"

class MYX_PUBLIC_FUNC MyxStatementParser
{
  enum ParserState { start, stmt, str, comment1, comment2, mlcomment, delimtok, delimkwd, eos };
  
  std::string delim;

public:
  MyxStatementParser();
  virtual ~MyxStatementParser();

  void process(std::istream& is, process_sql_statement_callback, void *arg, int mode);
};

#endif // _STATEMENT_PARSER_H_

