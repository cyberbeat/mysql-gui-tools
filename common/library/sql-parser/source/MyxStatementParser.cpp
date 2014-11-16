#include <ctype.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include "MyxStatementParser.h"


MyxStatementParser::MyxStatementParser()
{
  delim= ";";
}

MyxStatementParser::~MyxStatementParser()
{
}

static bool is_empty_statement(const std::string& str)
{
  int i= 0;
  while(str[i] != 0)
    if(str[i++] > ' ')
      return false;

  return true;
}

void MyxStatementParser::process(std::istream& is, process_sql_statement_callback cb, void *arg, int mode)
{
  static const char *kwd = "DELIMITER";
  
  int c;
  ParserState state= start, prevState;
  std::string stmt_buffer;
  std::string delim_buffer;
  char strchar;

  int i;
  bool m;
  //char p;

  while(!is.eof()) {
    switch(state) {
    case eos:
      break;
    
    case start:
      stmt_buffer.clear();
      c= is.get();
      while(isspace(c) || (c == '\n') || (c == '\r')) {
        stmt_buffer += c;
        c= is.get();
      }
      stmt_buffer += c;
      if(kwd[0] == toupper(c)) {
        state= delimkwd;
      } else if(c == '`') {
        strchar= '`';
        state= str;
      } else if(c == '\'') {
        strchar= '\'';
        state= str;
      } else if(c == '"') {
        strchar= '"';
        state= str;
//      } else if(c == '/') {
//        prevState= start;
//        state= mlcomment;
      } else if(c == '-') {
        prevState= start;
        state= comment1;
      } else if(c == '#') {
        prevState= start;
        state= comment2;
      } else if(c == delim[0]) {
        state= delimtok;
      } else {
        state= stmt;
      }
      continue;
    
    case delimkwd:
      m= true;
      for(i= 1; kwd[i] != '\0'; i++) {
        c= is.get();
        if(toupper(c) != kwd[i]) {
          m= false;
          is.putback(c);
          break;
        }
        else
        {
          stmt_buffer += c;        
        }
      }
      if(!m) {
        //state= stmt;
        //continue;
        goto stmtlabel;
      }
      c= is.get();
      stmt_buffer += c;
      if(!isspace(c)) {
        state= stmt;
        continue;
      }
      c= is.get();
      while(isspace(c)) {
        stmt_buffer += c;
        c= is.get();
      }
      if((c == '\r') || (c == '\n')) {
        stmt_buffer += c;
        state= stmt;
        continue;
      }
      delim_buffer.clear();
      while((c != '\r') && (c != '\n') && !isspace(c) && (c != -1)) 
      {
        delim_buffer += c;
        c= is.get();
      }
      if(delim_buffer.length() > delim.length()) 
      {
        if(delim_buffer.compare(delim_buffer.length() - delim.length(), delim.length(), delim) == 0)
        {
          delim_buffer.erase(delim_buffer.length() - delim.length());
        }
      }
      // new delimiter
      stmt_buffer.clear();
      delim= delim_buffer;
      state= start;
      
      while((c == '\r') || (c == '\n') || isspace(c))
      {
        c= is.get();
      }
      is.putback(c);

      continue;

    case str:
      c= is.get();
      while((c != strchar) && (c != -1)) 
      {
        stmt_buffer += c;
        if(c == '\\') 
        {
          c= is.get();
          stmt_buffer += c;
        }
        c= is.get();
      }
      stmt_buffer += c;
      if(c != -1) 
      {
        state= stmt;
      }
      continue;
/*
    case mlcomment:
      c= is.get();
      stmt_buffer += c;
      if(c != '*') {
        state= stmt;
        continue;
      }

      p = ' ';
      while(c != -1) {
        c= is.get();
        stmt_buffer += c;
        if((c == '/') && (p == '*')) {
          state= prevState;
          break;
        }
        if(c == -1)
        {
          break;
        }
        p= c;
      }
      continue;
*/
    case comment2:
      c= is.get();
      while((c != '\r') && (c != '\n') && (c != -1)) {
        stmt_buffer += c;
        c= is.get();
      }
      stmt_buffer += c;
      state= prevState;
      
      c= is.get();
      while((c == '\r') || (c == '\n'))
      {
        stmt_buffer += c;
        c= is.get();
      }
      is.putback(c);

      continue;

    case comment1:
      c= is.get();
      if(c != '-') {
        stmt_buffer += c;
        state= prevState;
        continue;
      }
      while((c != '\r') && (c != '\n') && (c != -1)) {
        stmt_buffer += c;
        c= is.get();
      }
      stmt_buffer += c;
      state= stmt;

      c= is.get();
      while((c == '\r') || (c == '\n'))
      {
        stmt_buffer += c;
        c= is.get();
      }
      is.putback(c);
      continue;

    case delimtok:
      m= true;
      for(i= 1; delim[i] != '\0'; i++) {
        c= is.get();
        stmt_buffer += c;
        if(toupper(c) != delim[i]) {
          m= false;
          break;
        }
      }
      if(!m) {
        state= stmt;
        continue;
      }
      // new statement is read
      stmt_buffer.erase(stmt_buffer.length() - delim.length());
      if(!is_empty_statement(stmt_buffer))
      {
        cb(stmt_buffer.c_str(), arg);
      }
      stmt_buffer.clear();
      state= start;
      continue;

    case stmt:
stmtlabel:
      while(c != -1) {
        c= is.get();
        stmt_buffer += c;
        if(kwd[0] == toupper(c)) {
          prevState= stmt;
          state= delimkwd;
          break;
        } else if(c == '`') {
          prevState= stmt;
          strchar= '`';
          state= str;
          break;
        } else if(c == '\'') {
          prevState= stmt;
          strchar= '\'';
          state= str;
          break;
        } else if(c == '"') {
          prevState= stmt;
          strchar= '"';
          state= str;
          break;
//        } else if(c == '/') {
//          prevState= stmt;
//          state= mlcomment;
//          break;
        } else if(c == '-') {
          prevState= stmt;
          state= comment1;
          break;
        } else if(c == '#') {
          prevState= stmt;
          state= comment2;
          break;
        } else if(c == delim[0]) {
          prevState= stmt;
          state= delimtok;
          break;
        }
      }
      continue;
    } // switch
  }

  if(!(mode & MYX_SPM_DELIMS_REQUIRED) && (stmt_buffer.length() > 0))
  {
    if(stmt_buffer[stmt_buffer.length() - 1] == (char)-1)
    {
      stmt_buffer.erase(stmt_buffer.length() - 1);
    }
    if(stmt_buffer.length() > 0) // might be changed after trimming -1
    {
      if(!is_empty_statement(stmt_buffer))
      {
        cb(stmt_buffer.c_str(), arg);
      }
    }
  }
}

MYX_PUBLIC_FUNC int myx_process_sql_statements(const char *sql, process_sql_statement_callback cb, void *user_data, int mode)
{
  MyxStatementParser p;
  std::istringstream tmp(sql);
  p.process(tmp, cb, user_data, mode);
  return 0;
}

MYX_PUBLIC_FUNC int myx_process_sql_statements_from_file(const char *filename, process_sql_statement_callback cb, void *user_data, int mode)
{
  MyxStatementParser p;
  std::ifstream is(filename);
  p.process(is, cb, user_data, mode);
  return 0;
}
