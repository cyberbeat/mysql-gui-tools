
#include "myx_public_interface.h"
#include "myx_qb_public_interface.h"
#include "myx_const_string.h"

#include <ctype.h>
#include <string>
#include <vector>
#include <list>

///////////////////////////////////////////////////////////////////////////////
struct Text_vector
{
  size_t char_vector[256];
  Text_vector(){}
  Text_vector(const const_string & text){init(text);}
  void init(const const_string & text)
  {
    for (size_t *spos= char_vector; spos!=char_vector+256; spos++)
      *spos= 0;
    for (const char * pos= text.begin(); pos!=text.end(); pos++)
      char_vector[*pos]++;
  }
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
long double get_distance(const Text_vector & tv1, const Text_vector & tv2)
{
  double sum= 0;
  const size_t * pos1, * pos2;
  for (pos1= tv1.char_vector, pos2= tv2.char_vector; 
       pos1!=tv1.char_vector+256; 
       pos1++, pos2++)
  {
    double diff= ((double)*pos1)-((double)*pos2);
    sum+= diff*diff;
  }
  return sum;
}

///////////////////////////////////////////////////////////////////////////////
struct lines_iterator
{
  const char * begin;
  const char * end;
  const char * pos;

  lines_iterator(const const_string & text)
  {
    begin= text.data();
    pos= begin;
    end= begin + text.length();
    while (pos!=end && *pos!='\r' && *pos!='\n')
      pos++;
  }

  const_string operator * ()
  {
    return const_string(begin,pos-begin);
  }

  bool is_valid()
  {
    return pos!=0;
  }

  void operator ++ ()
  {
    if (pos==end)
    {
      pos= 0;
      begin= 0;
    }
    else
    {
      if (*pos=='\r')
      {
        pos++;
        if (pos!=end && *pos=='\n')
          pos++;
        begin= pos;
      }
      else if (*pos=='\n')
      {
        pos++;
        begin= pos;
      }
      while (pos!=end && *pos!='\r' && *pos!='\n')
        pos++;
    }
  }
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
struct Line_part
{
  enum Kind
  {
    part_sql,
    part_3_language,
  };
  Kind kind;
  const_string text;
  Line_part(Kind kind, const const_string & text)
  {
    this->kind= kind;
    this->text= text;
  }
  bool is_3_language_part() { return kind==part_3_language; }
};
///////////////////////////////////////////////////////////////////////////////

struct Param;

///////////////////////////////////////////////////////////////////////////////
struct Line_part_3_lang : public Line_part
{
  bool contains_comments;
  bool contains_operators;
  bool written;

  std::string   name;
  Param       * param;

  Line_part_3_lang(const const_string & text)
   : Line_part(part_3_language,text)
  {
    param= 0;
    contains_comments= false;
    contains_operators= false;
    written= false;
  }
};
///////////////////////////////////////////////////////////////////////////////

struct Original_line_info;

///////////////////////////////////////////////////////////////////////////////
struct Result_line_info
{
  const_string          edited_line;
  Text_vector           text_vector;
  Original_line_info  * original_line;

  Result_line_info() { original_line= 0; }
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
struct Param
{
  std::string   name;
  std::string   value;
  const_string  prototype;
  bool use_as_part_of_sql_string;

  Param()
  {
    use_as_part_of_sql_string= false;
  }
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
struct Original_line_info
{
  const_string original_line;
  const_string stripped_line;

  std::vector<Line_part*> line_parts;

  Text_vector        stripped_line_vector;

  bool               is_unchanged;
  Result_line_info * result_line;

  void try_register_unchanged_result_line(Result_line_info * rline)
  {
    if (!result_line && stripped_line==rline->edited_line)
    {
      result_line= rline;
      is_unchanged= true;
      rline->original_line= this;
    }
  }

  Original_line_info()
  {
    is_unchanged= false;
    result_line= 0;
  }
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
struct Correspondence_variant
{
  Result_line_info    * result_line;
  Original_line_info  * original_line;
  double                distance;

  Correspondence_variant(Result_line_info   * result_line,
                         Original_line_info * original_line)
  {
    this->result_line= result_line;
    this->original_line= original_line;
    this->distance= get_distance(result_line->text_vector,
                                 original_line->stripped_line_vector);
  }
};
///////////////////////////////////////////////////////////////////////////////

const_string prefix_tab=
  CONST_STR("                                                               ");
const_string standard_middle_java_postfix=       CONST_STR("\" +");
const_string standard_middle_java_space_postfix= CONST_STR(" \" +");

///////////////////////////////////////////////////////////////////////////////
class SQL_query_for_3_language : public MYX_Q_SQL_STRIPPED
{
 public:
  SQL_query_for_3_language(const char * original_code_arg,
                              MYX_Q_SQL_STRIPPED_CODE_LANGUAGE  code_lang_arg,
                              MYX_Q_SQL_STRIPPED_COPY_MODE      copy_mode_arg);
  ~SQL_query_for_3_language();

  char * reconstruct(const char *sql);
  MYX_STRINGLIST * get_parameters();

 protected:
  std::string original_code, stripped_code;  

  std::vector<Original_line_info> original_lines;
  std::vector<Result_line_info>   result_lines;

  std::list<Param> params;

  Param *      add_parameter        (const const_string & name);
  Param * safe_add_parameter        (const const_string & name);
  Param * safe_add_parameter_prefix (const const_string & prefix);
  Param *     find_parameter        (const const_string & name);

  Line_part        * create_sql_line_part        (const const_string & text);
  Line_part_3_lang * create_3_language_line_part (const const_string & text);

  void parse_original_lines();
  void extract_sql_parts(const const_string & line, bool * is_in_sql_part);
  void extract_part(const const_string & text, bool is_in_sql_part);
  void extract_sql_part(const const_string & text);
  void extract_3_lang_part(const const_string & text);
  void push_back_original_line(const const_string & original_line,
                               bool * is_in_sql_part);
  void prepare_for_delphi();

  const_string  tab;
  void calculate_tab();
  const_string  calculate_original_3_language_tab       ();
  size_t        calculate_original_3_language_tab_size  ();

  void split_result_lines     (const const_string & edited_code);
  void push_back_edited_line  (const const_string & edited_line);

  void look_for_unchanged_lines ();
  void look_for_changed_lines   ();
    void calc_variants    (std::list<Correspondence_variant> * variants);
    void choose_variants  (const std::list<Correspondence_variant> & variants);

  void reconstruct_lines  (std::string * result_code);

  void construct_new_line     (Result_line_info * rline, std::string * res);
  void construct_new_prefix   (Result_line_info * rline, std::string * res);
  void construct_new_postfix  (Result_line_info * rline, std::string * res);

  void reconstruct_original_line(Result_line_info * rline, std::string * res);

  
  void reconstruct_changed_line  (Result_line_info * rline, std::string * res);
  void reconstruct_changed_postfix(Result_line_info * rline,std::string * res);
  void reconstruct_changed_prefix(Result_line_info * rline, std::string * res);

  bool use_original_changed_prefix(Result_line_info * rline);

  void reconstruct_line       (Result_line_info * rline, std::string * res);

  void encode_sql_part(const const_string & text, std::string * res);
  void decode_sql_part(const const_string & text, std::string * res);
  void encode_param(const const_string & param_name, std::string * res);
  void decode_param(const const_string & text, std::string * res);

  void analyze_3_lang_part(Line_part_3_lang * part);

  void prepare_parameter_value(const const_string & text, std::string * res);
  char get_concat_operator();

  bool check_if_starts_with_sql_keyword(const const_string & text);

  bool is_stripped_param_name_symbol(char c)const;
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
MYX_Q_SQL_STRIPPED * 
            myx_strip_embedded_sql(const char *sql,
                                   MYX_Q_SQL_STRIPPED_CODE_LANGUAGE  code_lang,
                                   MYX_Q_SQL_STRIPPED_COPY_MODE      copy_mode)
{
  return new SQL_query_for_3_language(sql,code_lang,copy_mode);
}

///////////////////////////////////////////////////////////////////////////////
MYX_STRINGLIST * myx_get_params_from_stripped_query(MYX_Q_SQL_STRIPPED *stripped)
{
  return ((SQL_query_for_3_language*)stripped)->get_parameters();
}

///////////////////////////////////////////////////////////////////////////////
char * myx_reconstruct_embedded_sql(MYX_Q_SQL_STRIPPED * strp, const char *sql)
{
  return ((SQL_query_for_3_language*)strp)->reconstruct(sql);
}

///////////////////////////////////////////////////////////////////////////////
void myx_free_stripped_sql(MYX_Q_SQL_STRIPPED *strp)
{
  delete (SQL_query_for_3_language*)strp;
}

///////////////////////////////////////////////////////////////////////////////
// implementation for SQL_query_for_3_language:
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::prepare_for_delphi()
{
  query_stripped= (char*)stripped_code.c_str();
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::push_back_original_line
                    (const const_string & original_line, bool * is_in_sql_part)
{
  original_lines.push_back(Original_line_info());
  Original_line_info & orig_line= original_lines.back();
  orig_line.original_line= original_line;
  extract_sql_parts(original_line,is_in_sql_part);
  orig_line.stripped_line_vector.init(orig_line.stripped_line);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::analyze_3_lang_part(Line_part_3_lang * part)
{
  bool is_inside_comment= false;
  const char * end= part->text.end();
  for (const char * pos= part->text.begin(); pos!=end; pos++)
  {
    if (*pos=='/')
    {
      pos++;
      if (pos==end || *pos=='/')
      {
        part->contains_comments= true;
        return;
      }
      if (*pos=='*')
      {
        part->contains_comments= true;
        is_inside_comment= true;
      }
    }
    else if (is_inside_comment && *pos=='*')
    {
      pos++;
      for (;;)
      {
        if (pos==end)
          return;
        if (*pos=='/')
        {
          is_inside_comment= false;
          break;
        }
        if (*pos!='*')
          break;
      }
    }
    else if (is_stripped_param_name_symbol(*pos))
    {
      part->name.append(1,*pos);
    }
    else if (!isspace(*pos) && *pos!='\t')
    {
      part->contains_operators= true;
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
bool is_completed_sql_line(const const_string & line)
{
  char opened_quote= 0;
  const char * end= line.end();
  for (const char * pos= line.begin(); pos!=end; pos++)
  {
    if (*pos=='\'' || *pos=='\"' || *pos=='`')
    {
      opened_quote= opened_quote==*pos ? 0 : *pos;
    }
    else  if (*pos=='\\')
    {
      if (opened_quote!=0)
      {
        pos++;
        if (pos==end)
          break;
      }
    }
  }

  return (0 == opened_quote);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::parse_original_lines()
{
  const_string const_original_code(original_code.c_str(),
                                   original_code.length());
  lines_iterator iter(const_original_code);
  const char * line_begin= stripped_code.data();
  bool is_in_sql_part= check_if_starts_with_sql_keyword(const_original_code);
  if (iter.is_valid())
  {
    push_back_original_line(*iter,&is_in_sql_part);
    for (++iter; iter.is_valid(); ++iter)
    {
      const char * line_end= &*stripped_code.end();
      if (is_completed_sql_line(const_string(line_begin,line_end-line_begin)))
      {
        stripped_code.append(1,'\n');
        line_begin= line_end+1;
      }
      push_back_original_line(*iter,&is_in_sql_part);
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
Line_part * 
   SQL_query_for_3_language::create_sql_line_part(const const_string & text)
{
  return new Line_part(Line_part::part_sql,text);
}

///////////////////////////////////////////////////////////////////////////////
Line_part_3_lang * SQL_query_for_3_language::create_3_language_line_part
                                                    (const const_string & text)
{
  return new Line_part_3_lang(text);
}

///////////////////////////////////////////////////////////////////////////////
Param * SQL_query_for_3_language::add_parameter(const const_string & name)
{
  params.push_back(Param());
  Param * result= &params.back();
  result->name.assign(name.data(),name.length());
  return result;
}

///////////////////////////////////////////////////////////////////////////////
Param * SQL_query_for_3_language::safe_add_parameter_prefix
                                                  (const const_string & prefix)
{
  std::string name;
  name.assign(prefix.data(),prefix.length());
  name.reserve(name.length()+5);
  char * pos= (char*)(name.data() + name.length());
  for (size_t i= 1; i<100; i++)
  {
    *pos= 0;
    size_t diff_length= sprintf(pos,"%u",i);
    const_string name_variant(name.data(),name.length()+diff_length);
    Param * param= find_parameter(name_variant);
    if (!param)
      return add_parameter(name_variant);
  }
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
Param * SQL_query_for_3_language::safe_add_parameter(const const_string & name)
{
  return  name.empty()        ? safe_add_parameter_prefix(CONST_STR("param")) :
          find_parameter(name)? safe_add_parameter_prefix(name)
                              : add_parameter(name);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::extract_sql_part(const const_string & text)
{
  Original_line_info & oline= original_lines.back();
  oline.line_parts.push_back(create_sql_line_part(text));
  size_t old_length= stripped_code.length();
  decode_sql_part(text,&stripped_code);
  size_t length_diff= stripped_code.length() - old_length;
  const_string & stripped_line= oline.stripped_line;
  stripped_line= const_string(stripped_line.data(),
                              stripped_line.length() + length_diff);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::prepare_parameter_value
                                 (const const_string & text, std::string * res)
{
  const char * begin= text.begin();
  const char * end= text.end()-1;

  char concat_operator= get_concat_operator();

  while (isspace(*begin) || *begin=='\t' || *begin==concat_operator)
    begin++;

  while (isspace(*end) || *end=='\t' || *end==concat_operator)
    end--;

  res->assign(begin,end-begin+1);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::extract_3_lang_part(const const_string & text)
{
  Original_line_info & oline= original_lines.back();
  Line_part_3_lang * part= create_3_language_line_part(text);
  oline.line_parts.push_back(part);
  if (original_lines.size()!=1 || oline.line_parts.size()!=1)
  {
    analyze_3_lang_part(part);
    if (!part->name.empty())
    {
      Param * param= safe_add_parameter(const_string(part->name.data(),
                                                     part->name.length()));
      prepare_parameter_value(text,&param->value);
      part->param= param;
      size_t old_length= stripped_code.length();
      stripped_code.append(1,':');
      stripped_code.append(param->name);
      size_t length_diff= stripped_code.length() - old_length;
      const_string & stripped_line= oline.stripped_line;
      stripped_line= const_string(stripped_line.data(),
                                  stripped_line.length() + length_diff);
    }
  }
}

const_string sql_keywords[]=
{
  CONST_STR("select"),
  CONST_STR("update"),
  CONST_STR("delete"),
  CONST_STR("create"),
  CONST_STR("alter"),
  CONST_STR("drop"),
};
const_string * sql_keywords_end=
                      sql_keywords + sizeof(sql_keywords)/sizeof(const_string);

#ifndef _WINDOWS_
int strnicmp(const char *string1, const char *string2, size_t count)
{
  const char * c1= string1;
  const char * c1_end= c1 + count;
  const char * c2= string2;
  for (; *c1 && *c2 && c1!=c1_end; c1++, c2++)
  {
    char cc1= toupper(*c1);
    char cc2= toupper(*c2);
    if (cc1>cc2)
      return 1;
    if (cc1<cc2)
      return -1;
  }
  return 0;
}
#endif

///////////////////////////////////////////////////////////////////////////////
bool SQL_query_for_3_language::check_if_starts_with_sql_keyword
                                                    (const const_string & text)
{
  const char * begin= text.begin();
  const char * pos= begin;
  const char * end= text.end();

  for (; pos!=end && (isspace(*pos) || *pos=='\t'); pos++);

  if (pos==end)
    return false;

  size_t length= text.length() - (pos - begin);

  for (const_string * keyword= sql_keywords;
       keyword!=sql_keywords_end; keyword++)
  {
    if (keyword->length()<=length && 
        !strnicmp(pos,keyword->begin(),keyword->length()))
    {    
      return true;
    }
  }
  return false;
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::extract_part(const const_string & text,
                                            bool is_in_sql_part)
{
  if (is_in_sql_part)
  {
    extract_sql_part(text);
  }
  else
  {
    extract_3_lang_part(text);
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::extract_sql_parts(const const_string & line,
                                                 bool * is_in_sql_part)
{
  const char * begin= line.data();
  const char * end= begin + line.length();
  const char * pos= begin;

  original_lines.back().stripped_line=
                   const_string(stripped_code.data()+stripped_code.length(),0);

  while (pos!=end)
  {
    if (*pos=='\"')
    {
      extract_part(const_string(begin,pos-begin),*is_in_sql_part);
      *is_in_sql_part= !(*is_in_sql_part);
      pos++;
      begin= pos;
    }
    else if (*(is_in_sql_part) && *pos=='\\')
    {
      pos++;
      if (pos==end)
        break;
      pos++;
    }
    else
    {
      pos++;
    }
  }
  extract_part(const_string(begin,pos-begin),*is_in_sql_part);
}

///////////////////////////////////////////////////////////////////////////////
SQL_query_for_3_language::SQL_query_for_3_language
                              (const char * original_code_arg,
                               MYX_Q_SQL_STRIPPED_CODE_LANGUAGE  code_lang_arg,
                               MYX_Q_SQL_STRIPPED_COPY_MODE      copy_mode_arg)
{
  query_stripped= 0;

  original_code= original_code_arg;
  stripped_code.reserve(original_code.length());
  code_lang= code_lang_arg;
  copy_mode= copy_mode_arg;

  parse_original_lines();
  prepare_for_delphi();
}

///////////////////////////////////////////////////////////////////////////////
SQL_query_for_3_language::~SQL_query_for_3_language()
{
  std::vector<Original_line_info>::iterator iter;
  for (iter= original_lines.begin();
       iter != original_lines.end(); iter++)
  {
    std::vector<Line_part*>::iterator parts_iter;
    for (parts_iter= iter->line_parts.begin();
         parts_iter!=iter->line_parts.end(); parts_iter++)
    {
      delete (*parts_iter);
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
const_string SQL_query_for_3_language::calculate_original_3_language_tab()
{
  std::vector<Original_line_info>::iterator iter;
  for (iter= original_lines.begin();
       iter != original_lines.end(); iter++)
  {
    if (!iter->line_parts.empty())
    {
      Line_part * first_line_part= iter->line_parts.front();
      const_string text= first_line_part->text;
      if (first_line_part->is_3_language_part() && *text.end()=='\"')
      {
        for (const char * pos= text.begin(); pos!=text.end(); pos++)
        {
          if (!isspace(*pos))
            goto next_original_line;
        }
        return text;
      }
    }
next_original_line:;
  }
  return const_string();
}

///////////////////////////////////////////////////////////////////////////////
size_t SQL_query_for_3_language::calculate_original_3_language_tab_size()
{
  std::vector<Original_line_info>::iterator iter;
  for (iter= original_lines.begin();
       iter != original_lines.end(); iter++)
  {
    if (!iter->line_parts.empty())
    {
      Line_part * first_line_part= iter->line_parts.front();
      const_string text= first_line_part->text;
      if (first_line_part->is_3_language_part() && *text.end()=='\"')
        return text.length();
    }
  }
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::construct_new_prefix(Result_line_info * rline,
                                                    std::string * res)
{
  const_string prefix= (rline!=&result_lines.front() || original_lines.empty())
                        ? tab
                        : original_lines.front().line_parts[0]->text;
  res->append(prefix.data(),prefix.length());
  res->append(1,'\"');
}

///////////////////////////////////////////////////////////////////////////////
bool next_line_requires_space(const const_string & line)
{
  if (line.empty())
    return false;
  const char last_char= *(line.end()-1);
  if (!isalpha(last_char) && !isdigit(last_char))
    return false;
  for (const char * pos= line.end(); *pos; pos++)
  {
    if (*pos!='\r' && *pos!='\n')
      return isalpha(*pos) || isdigit(*pos);
  }
  return false;
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::construct_new_postfix(Result_line_info * rline,
                                                     std::string * res)
{
  const_string postfix;
  if (rline != &result_lines.back())
  {
    postfix= next_line_requires_space(rline->edited_line)
               ? standard_middle_java_space_postfix
               : standard_middle_java_postfix;
    res->append(postfix.data(),postfix.length());
  }
  else
  {
    res->append(1,'\"');
    if (!original_lines.empty())
    {
      postfix= original_lines.back().line_parts.back()->text;
      res->append(postfix.data(),postfix.length());
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::construct_new_line(Result_line_info * rline,
                                                  std::string * res)
{
  construct_new_prefix(rline,res);
  encode_sql_part(rline->edited_line,res);
  construct_new_postfix(rline,res);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::reconstruct_original_line
                                                     (Result_line_info * rline,
                                                      std::string * res)
{
  Original_line_info * oline= rline->original_line;
  const_string orig_text= oline->original_line;
  if (oline==&original_lines.front() && rline!=&result_lines.front())
  {
    size_t tab= oline->line_parts.front()->text.length();
    res->append(tab,' ');
    res->append(orig_text.data()+tab,orig_text.length()-tab);
  }
  else if (oline==&original_lines.back() && rline!=&result_lines.back())
  {
    size_t tab= oline->line_parts.back()->text.length()+1;
    res->append(orig_text.data(),orig_text.length()-tab);
    res->append(next_line_requires_space(rline->edited_line) 
                ? " \" +" : "\" +");
  }
  else
  {
    res->append(orig_text.data(),orig_text.length());
  }
}

///////////////////////////////////////////////////////////////////////////////
bool SQL_query_for_3_language::use_original_changed_prefix
                                                     (Result_line_info * rline)
{
  Original_line_info * oline= rline->original_line;
  if (oline==&original_lines.front() && rline!=&result_lines.front())
    return false;

  Line_part * first_part= oline->line_parts.front();
  if (!first_part->is_3_language_part())
    return false;

  Line_part_3_lang * part= (Line_part_3_lang*)first_part;

  if (part->param || part->contains_operators)
    return false;
  
  return true;
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::reconstruct_changed_prefix
                                                     (Result_line_info * rline,
                                                      std::string * res)
{
  Original_line_info * first_oline= &*original_lines.begin();
  if (first_oline && rline == &*result_lines.begin() && 
      rline->original_line != first_oline &&
      first_oline->line_parts.front()->is_3_language_part())
  {
    Line_part * first_part= first_oline->line_parts.front();
    const_string orig_prefix= first_part->text;
    res->append(orig_prefix.data(),orig_prefix.length());
    ((Line_part_3_lang*)first_part)->written= true;
  }
  else
  {
    Original_line_info * oline= rline->original_line;
    Line_part * first_part= oline->line_parts.front();
    if (!first_part->is_3_language_part())
      return;
    if (!use_original_changed_prefix(rline))
    {
      res->append(first_part->text.length(),' ');
    }
    else
    {
      const_string orig_prefix= first_part->text;
      res->append(orig_prefix.data(),orig_prefix.length());
      ((Line_part_3_lang*)first_part)->written= true;
    }
  }
  res->append("\"");
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::reconstruct_changed_postfix
                                  (Result_line_info * rline, std::string * res)
{
  Original_line_info * oline= rline->original_line;
  bool reauires_space= next_line_requires_space(rline->edited_line);
  Line_part * last_part= oline->line_parts.back();
  if (!last_part->is_3_language_part())
    return;
  if (oline==&original_lines.back() && rline!=&result_lines.back() ||
      !last_part->is_3_language_part() || 
      ((Line_part_3_lang*)last_part)->param)
  {
    res->append(reauires_space ? " \" +" : "\" +");
  }
  else
  {
    res->append(reauires_space ? " \"" : "\"");
    const_string orig_postfix= last_part->text;
    res->append(orig_postfix.data(),orig_postfix.length());
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::reconstruct_changed_line
                                                     (Result_line_info * rline,
                                                      std::string * res)
{
  reconstruct_changed_prefix(rline,res);
  encode_sql_part(rline->edited_line,res);
  reconstruct_changed_postfix(rline,res);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::push_back_edited_line
                                             (const const_string & edited_line)
{
  result_lines.push_back(Result_line_info());
  Result_line_info & result_line= result_lines.back();
  result_line.edited_line= edited_line;
  result_line.text_vector.init(edited_line);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::split_result_lines
                                             (const const_string & edited_code)
{
  lines_iterator iter(edited_code);
  for (; iter.is_valid(); ++iter)
    push_back_edited_line(*iter);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::look_for_unchanged_lines()
{
  std::vector<Result_line_info>::iterator riter;
  for (riter= result_lines.begin(); riter!=result_lines.end(); riter++)
  {
    std::vector<Original_line_info>::iterator oiter;
    for (oiter= original_lines.begin(); oiter!=original_lines.end(); oiter++)
      oiter->try_register_unchanged_result_line(&*riter);
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::calc_variants
                                 (std::list<Correspondence_variant> * variants)
{
  std::vector<Result_line_info>::iterator riter;
  std::vector<Original_line_info>::iterator oiter;
  std::list<Correspondence_variant>::iterator viter;

  for (riter= result_lines.begin(); riter!=result_lines.end(); riter++)
  {
    for (oiter= original_lines.begin(); oiter!=original_lines.end(); oiter++)
    {
      if (!oiter->result_line)
      {
        Correspondence_variant variant(&*riter,&*oiter);
        for (viter= variants->begin(); viter!=variants->end(); viter++)
        {
          if (viter->distance > variant.distance)
            break;
        }
        variants->insert(viter,variant);
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::choose_variants
                           (const std::list<Correspondence_variant> & variants)
{
  std::vector<Original_line_info>::iterator oiter;
  std::list<Correspondence_variant>::const_iterator viter;

  for (oiter= original_lines.begin(); oiter!=original_lines.end(); oiter++)
  {
    if (!oiter->result_line)
    {
      for (viter= variants.begin(); viter!=variants.end(); viter++)
      {
        if (viter->original_line==&*oiter)
        {
          oiter->result_line= viter->result_line;
          oiter->result_line->original_line= &*oiter;
          break;
        }
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::look_for_changed_lines()
{
  std::list<Correspondence_variant> variants;
  calc_variants(&variants);
  choose_variants(variants);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::reconstruct_line(Result_line_info * rline,
                                                std::string * res)
{
  if (!rline->original_line)
  {
    construct_new_line(rline,res);
  }
  else
  {
    Original_line_info * oline= rline->original_line;
    if (oline->is_unchanged)
    {
      reconstruct_original_line(rline,res);
    }
    else
    {
      reconstruct_changed_line(rline,res);
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::reconstruct_lines(std::string * result_code)
{
  std::vector<Result_line_info>::iterator riter= result_lines.begin();
  if (riter!=result_lines.end())
  {
    reconstruct_line(&*riter,result_code);
    for (riter++; riter!=result_lines.end(); riter++)
    {
      result_code->append("\n");
      reconstruct_line(&*riter,result_code);
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::calculate_tab()
{
  tab= calculate_original_3_language_tab();
  if (tab.empty())
  {
    size_t size= calculate_original_3_language_tab_size();
    if (size>prefix_tab.length())
      size= prefix_tab.length();
    tab= const_string(prefix_tab.data(),size);
  }
}

///////////////////////////////////////////////////////////////////////////////
char * SQL_query_for_3_language::reconstruct(const char * str_edited_code)
{
  const_string edited_code  (str_edited_code,strlen(str_edited_code));

  split_result_lines        (edited_code);

  look_for_unchanged_lines  ();
  look_for_changed_lines    ();
  calculate_tab             ();

  std::string result_code;

  reconstruct_lines         (&result_code);

  if (result_code.empty())
    result_code= "\"\"";

  return g_strdup(result_code.c_str());
}

///////////////////////////////////////////////////////////////////////////////
Param * SQL_query_for_3_language::find_parameter(const const_string & name)
{
  for (std::list<Param>::iterator iter= params.begin(); 
       iter!=params.end(); iter++)
  {
    if (const_string(iter->name.data(),iter->name.length())==name)
      return &*iter;
  }
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
MYX_STRINGLIST * SQL_query_for_3_language::get_parameters()
{
  MYX_STRINGLIST *param_list=
                            (MYX_STRINGLIST*)g_malloc0(sizeof(MYX_STRINGLIST));

  param_list->strings= (char **)g_malloc0(sizeof(char*)*(gulong)params.size());
  param_list->strings_num= (unsigned int)params.size();

  char ** str= param_list->strings;
  for (std::list<Param>::iterator iter= params.begin();
       iter!=params.end(); iter++, str++)
  {
    *str= g_strdup(iter->name.c_str());
  }

  return param_list;
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::encode_param(const const_string & param_name,
                                            std::string * res)
{
  Param * param= find_parameter(param_name);
  if (!param)
  {
    res->append(1,'?');
  }
  else if (param->use_as_part_of_sql_string)
  {
    if (code_lang==MYX_QSSCL_JAVA)
    {
      res->append(1,'?');
    }
    else
    {
      res->append(param->value);
    }
  }
  else
  {
    char concat_operator= get_concat_operator();
    res->append("\" ");
    res->append(1,concat_operator);
    res->append(" ");
    res->append(param->value);
    res->append(" ");
    res->append(1,concat_operator);
    res->append(" \"");
  }
}

///////////////////////////////////////////////////////////////////////////////
char SQL_query_for_3_language::get_concat_operator()
{
  return code_lang==MYX_QSSCL_PHP ? '.' : '+';
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::encode_sql_part(const const_string & text,
                                               std::string * res)
{
  const char * begin= text.begin();
  const char * end= text.end();
  const char * pos= begin;
  while ( pos != end )
  {
    if (*pos=='\"')
    {
      res->append(begin,pos-begin);
      res->append("\\\"",2);
      pos++;
      begin= pos;
      pos++;
    }
    else if (*pos==':')
    {
      res->append(begin,pos-begin);
      pos++;
      const char * name_begin= pos;
      for ( ; pos!=end && (is_stripped_param_name_symbol(*pos)); pos++)
      {}
      encode_param(const_string(name_begin,pos-name_begin),res);
      if (pos!=end && *pos==':')
        pos++;
      begin= pos;
    }
    else
    {
      pos++;
    }
  }
  res->append(begin,pos-begin);
}

///////////////////////////////////////////////////////////////////////////////
bool SQL_query_for_3_language::is_stripped_param_name_symbol(char c)const
{
  return (isalpha(c) || isdigit(c) || c=='_');
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::decode_param(const const_string & text,
                                            std::string * res)
{
  std::string name;
  for (const char * pos= text.begin(); pos!=text.end(); pos++)
  {
    if (is_stripped_param_name_symbol(*pos))
      name.append(1,*pos);
  }
  Param * param= safe_add_parameter(const_string(name.data(),name.length()));
  param->value.assign(text.data(),text.length());
  param->use_as_part_of_sql_string= true;
  res->append(1,':');
  res->append(param->name);
}

///////////////////////////////////////////////////////////////////////////////
void SQL_query_for_3_language::decode_sql_part(const const_string & text,
                                               std::string * res)
{
  const char * begin= text.begin();
  const char * end= text.end();
  const char * pos= begin;
  while ( pos!=end )
  {
    if (*pos=='\\')
    {
      res->append(begin,pos-begin);
      pos++;
      begin= pos;
      pos++;
    }
    else if (*pos==':')
    {
      res->append(begin,pos-begin);
      res->append(2,':');
      pos++;
      begin= pos;
    }
    else if (*pos=='$' && code_lang==MYX_QSSCL_PHP)
    {
      res->append(begin,pos-begin);
      const char * name_begin= pos;
      pos++;
      for ( ; pos!=end && (isalpha(*pos) || isdigit(*pos) || *pos=='_'); pos++)
      {}
      decode_param(const_string(name_begin,pos-name_begin),res);
      begin= pos;
      if (pos!=end)
        pos++;
    }
    else if (*pos=='{' && code_lang==MYX_QSSCL_PHP)
    {
      res->append(begin,pos-begin);
      const char * name_begin= pos;
      pos++;
      for ( ; pos!=end && *pos!='}'; pos++)
      {}
      if (pos!=end)
        pos++;
      decode_param(const_string(name_begin,pos-name_begin),res);
      begin= pos;
    }
    else if (*pos=='?' && code_lang==MYX_QSSCL_JAVA)
    {
      res->append(begin,pos-begin);
      pos++;
      decode_param(CONST_STR("?"),res);
      begin= pos;
    }
    else
    {
      pos++;
    }
  }
  res->append(begin,pos-begin);
}

///////////////////////////////////////////////////////////////////////////////
