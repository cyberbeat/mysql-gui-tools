/* Copyright (C) 2004 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

#include "keywords.h"
#include "myx_library.h"
#include <glib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define INITIAL_ARRAY_SIZE 500
#define ARRAY_INC_SIZE 100


struct myx_syn
{
  MYSQL *mysql;
  GSList *synlist, *syntypelist;
};

struct line_state
{
  int state;
  int token_begin;
};

static MYX_SYN_TYPE syn_types[]= {MYX_SYN_NORMAL, MYX_SYN_TABLE, 
				  MYX_SYN_COLUMN, MYX_SYN_COMMENT, 
				  MYX_SYN_STRING, MYX_SYN_SYMBOL, 
				  MYX_SYN_FUNCTION};

//N = newline, E= any other character
static int state_machine[15][12] =
{
  //      ; ' " # - / * N \ E ! `
/*0*/    {1,3,2,6,4,7,0,0,0,0,0,14},
/*1*/    {13,13,13,13,13,13,13,13,13,13,13,13},
/*2*/    {2,2,0,2,2,2,2,2,11,2,2,2},
/*3*/    {3,0,3,3,3,3,3,3,10,3,3,3},
/*4*/    {1,3,2,6,5,7,0,0,0,0,0,14},
/*5*/    {5,5,5,5,5,5,5,0,5,5,5,5},
/*6*/    {6,6,6,6,6,6,6,0,6,6,6,6},
/*7*/    {1,3,2,6,4,7,8,0,0,0,0,14},
/*8*/    {8,8,8,8,8,8,12,8,8,8,8,8},
/*9*/    {9,9,9,9,9,9,12,9,9,9,9,9}, /* not used */
/*10*/   {3,3,3,3,3,3,3,3,3,3,3,3},
/*11*/   {2,2,2,2,2,2,2,2,2,2,2,2},
/*12*/   {8,8,8,8,8,0,12,8,8,8,8,8},
/*13*/   {13,13,13,13,13,13,13,13,13,13,13,13}, /* error state */
/*14*/   {14,14,14,14,14,14,14,14,14,14,0}
};


#if 0
/* This is stuff from glib. This is included
 * here because we need a little extra
 * functionality.
 */
typedef struct _GTreeNode  GTreeNode;

struct _GTreeNode
{
  gint balance;      /* height (left) - height (right) */
  GTreeNode *left;   /* left subtree */
  GTreeNode *right;  /* right subtree */
  gpointer key;      /* key for this node */
  gpointer value;    /* value stored at this node */
};


struct _GTree
{
  GTreeNode *root;
  GCompareDataFunc key_compare;
  GDestroyNotify   key_destroy_func;
  GDestroyNotify   value_destroy_func;
  gpointer         key_compare_data;
};

/* end glib stuff */
#endif

/*
 * Forward declaratioins
 */

static void make_new_word(MYX_SQL_HIGHLIGHTING *sql_highlighting, int begin, int end, const char *text, int last_state);
//static GTreeNode *tree_node_lookup (GTreeNode        *node, gconstpointer     key);
//static gint g_tree_node_in_order (GTreeNode     *node, GTraverseFunc  traverse_func, gpointer       data);
static gboolean add_suggestions (gpointer key, gpointer value, gpointer data);
static gint comp_func (gconstpointer a,gconstpointer b, gpointer user_data);
static void free_syn_suggestion_content(MYX_SYN_SUGGESTION *s);
static int is_delimiter(gunichar c);
static char *get_word(const char* text, int beg, int end);
static int make_state_transition(int state, gunichar c);




/*
 * PUBLIC FUNCIONS
 */


/** 
 * @_syn: The previous syn or NULL if called for the
 *   first time
 * @mysql: The connection to the database. It is important that
 *   a database is already selected. Otherwise this function
 *   will fail. 
 *
 * Reads all table and column-names from the database
 * and stored them internally in the returned value. This is
 * typically is typically called in the beginning and then
 * every time the user changes the database.
 *
 * Return value: A pointer to a MYX_SYN structure that has
 *  to be passed to all future SYN(=autocompletion & syntax
 *  highlighting) function-calls.
 **/
MYX_SYN* myx_refresh_dbinfo(MYSQL *mysql, MYX_SYN *_syn)
{
  MYX_SYN *syn;
  char *stmt;
  MYSQL_RES *result;
  MYSQL_ROW row;
  unsigned int i;

  if (!_syn)
  {
    syn= g_malloc0(sizeof(MYX_SYN));
  }
  else 
  {
    syn= _syn;
    //g_tree_destroy(syn->tree);
    // TODO: free the lists
  }

  syn->mysql= mysql;
  //syn->tree=  g_tree_new_full(comp_func, NULL, g_free, NULL);
  syn->synlist= NULL;
  syn->syntypelist= NULL;

  /* insert all keywords */
  for (i=0; i < sizeof(symbols)/sizeof(SYMBOL); i++)
  {
    //g_tree_insert(syn->tree, g_strdup(symbols[i].name), &syn_types[MYX_SYN_SYMBOL]);
    //g_tree_insert(syn->tree, g_strdup(symbols[i].name), 
    //  new_lookup_tree_pair(syn_types[MYX_SYN_SYMBOL], symbols[i].name));
    syn->synlist= g_slist_append(syn->synlist, g_strdup(symbols[i].name));
    syn->syntypelist= g_slist_append(syn->syntypelist, &syn_types[MYX_SYN_SYMBOL]);
  } 
  for (i=0; i < sizeof(sql_functions)/sizeof(SYMBOL); i++)
  {
    //g_tree_insert(syn->tree, g_strdup(sql_functions[i].name), &syn_types[MYX_SYN_FUNCTION]);
    //g_tree_insert(syn->tree, g_strdup(sql_functions[i].name), 
    //  new_lookup_tree_pair(syn_types[MYX_SYN_FUNCTION], sql_functions[i].name));
    syn->synlist= g_slist_append(syn->synlist,  g_strdup(sql_functions[i].name));
    syn->syntypelist= g_slist_append(syn->syntypelist, &syn_types[MYX_SYN_FUNCTION]);    
  } 

  /* insert all database and fieldnames */
  result= NULL;
  stmt= "SHOW TABLES";
  if (mysql_query(mysql, stmt) == 0)
    result= mysql_store_result(mysql);
  if (result == NULL)
  {
    // probably means there's no DB selected, return with keywords only
    return syn;
  }

  do
  {
    MYSQL_RES *result2;
    MYSQL_ROW row2;
    char *table_name;
    char *stmt2;

    row= mysql_fetch_row(result);
    if (row == NULL)
      break;
    
    table_name= row[0];

    //g_tree_insert(syn->tree, g_strdup(table_name), &syn_types[MYX_SYN_TABLE]);
    //g_tree_insert(syn->tree, g_strdup(table_name), 
    //  new_lookup_tree_pair(syn_types[MYX_SYN_TABLE], table_name));
    syn->synlist= g_slist_append(syn->synlist, g_strdup(table_name));
    syn->syntypelist= g_slist_append(syn->syntypelist, &syn_types[MYX_SYN_TABLE]);

    /* search for all fields */
    result2= NULL;
    stmt2= g_strconcat("DESCRIBE `",table_name, "`", NULL);
    if (mysql_query(mysql, stmt2) == 0)
      result2= mysql_store_result(mysql);
    if (result2 == NULL)
    {
      g_free(stmt2);
      myx_free_syn(syn);
      return syn;
    } 
    g_free(stmt2);

    do
    {
      char *field_name;

      row2= mysql_fetch_row(result2);
      if (row2 == NULL)
        break;

      field_name= row2[0];
      
      //g_tree_insert(syn->tree, g_strdup(field_name), &syn_types[MYX_SYN_COLUMN]);
      //g_tree_insert(syn->tree, g_strdup(field_name), 
      //  new_lookup_tree_pair(syn_types[MYX_SYN_COLUMN]. field_name));
      syn->synlist= g_slist_append(syn->synlist, g_strdup(field_name));
      syn->syntypelist= g_slist_append(syn->syntypelist, &syn_types[MYX_SYN_COLUMN]);
      
      //g_tree_insert(syn->tree, g_strconcat(table_name, ".", field_name, NULL), &syn_types[MYX_SYN_COLUMN]);
      //field_name= g_strconcat(table_name, ".", field_name, NULL);
      //g_tree_insert(syn->tree, field_name, 
      //  new_lookup_tree_pair(syn_types[MYX_SYN_COLUMN], field_name));
      syn->synlist= g_slist_append(syn->synlist,g_strconcat(table_name, ".", field_name, NULL));
      syn->syntypelist= g_slist_append(syn->syntypelist, &syn_types[MYX_SYN_COLUMN]);
    }
    while (1);

    mysql_free_result(result2);
  }
  while (1);
    

  mysql_free_result(result);

  return syn;
}

/* Free functions */

int myx_free_syn(MYX_SYN *syn)
{
  if (syn)
  {
    //if (syn->tree) g_tree_destroy(syn->tree);
    // TODO: free keyword lists
  }

  return 0;
}


int myx_free_syn_suggestions(MYX_SYN_SUGGESTIONS *s)
{
  if (s)
  {
    unsigned int i;

    for (i=0; i< s->suggestions_num; i++)
    {
      free_syn_suggestion_content(s->suggestions+i);
    }
    g_free(s->suggestions);
    g_free(s);
  }

  return 0;
}


/* auto-completion */

/**
 * This is a convenience wrapper around myx_lookup_word.
 * Call it when you don't want to find out the last word
 * of a line yourself.
 **/
MYX_SYN_SUGGESTIONS* myx_lookup_line(MYX_SYN* syn, const char *line)
{
  const char *end= line+ strlen(line)-1;
  const char *p= end;
  char *word;
  MYX_SYN_SUGGESTIONS* result;

  while (p!=line && !isspace(*p) && *p!='"' && *p!='\'' && *p!='`')
  {
    p--;
  }
  
  word=g_malloc((gulong)(end-p+2));
  strncpy(word, p, end-p+1);
  word[end-p+1]= 0;

  result= myx_lookup_word(syn, word);
  g_free(word);

  return result;
}

MYX_SYN_SUGGESTIONS* myx_lookup_word(MYX_SYN* syn,const char *word_beg)
{
  //GTreeNode *node;
  MYX_SYN_SUGGESTIONS *s;
  size_t len;
  GSList *next_type, *next_name;
  
  s= g_malloc0(sizeof(MYX_SYN_SUGGESTIONS));

  /*
  node= tree_node_lookup(syn->tree->root, word_beg);
  if (node)
  {
    g_tree_node_in_order (node, add_suggestions, s);
  }
  */
  
  len= strlen(word_beg);
  next_type= syn->syntypelist;
  for(next_name= syn->synlist; next_name != NULL; 
    next_type= g_slist_next(next_type), next_name= g_slist_next(next_name))
  {
    if(g_strncasecmp(next_name->data, word_beg, len) == 0)
    {
      add_suggestions(next_name->data, &next_type->data, s);
    }
  }

  return s;
}


/* SYNTAX-HIGHLIGHTING */


MYX_SQL_HIGHLIGHTING* myx_init_sql_parsing(MYX_SYN* syn)
{
  MYX_SQL_HIGHLIGHTING* highlighting;

  highlighting= g_malloc0(sizeof(MYX_SQL_HIGHLIGHTING));

  highlighting->line_states_size= INITIAL_ARRAY_SIZE;
  highlighting->line_states= g_malloc0(sizeof(MYX_LINE_STATE) * INITIAL_ARRAY_SIZE);

  highlighting->words_size= INITIAL_ARRAY_SIZE;
  highlighting->words= g_malloc0(sizeof(MYX_SYN_WORD) * INITIAL_ARRAY_SIZE);

  highlighting->syn= syn;

  return highlighting;
}

int myx_free_sql_highlighting(MYX_SQL_HIGHLIGHTING *h)
{
  if (h)
  {
    g_free(h->line_states);
    g_free(h->words);
    g_free(h);
  }

  return 0;
}

/**
 * @text: The line that should be analyzed in UTF8-encoding
 * @line_no: The line-number of @text. The first line-number
 *     should be 0.
 *
 * Analyzes one text-line and returns information how
 * to highlight this line.
 *
 * Return value: A number greater than 0 if the line below
 *    this line needs to be updated. You should call 
 *    myx_highlight in a loop as long as this
 *    function returns a value > 0 or there are no more
 *    lines.
 *
 **/
int myx_highlight(MYX_SQL_HIGHLIGHTING *sql_highlighting, const char *text, unsigned int line_no)
{
  const char *p;
  int state;
  int last_state;
  int token_begin;
  int counter;
			
  g_return_val_if_fail(text, 0);
  g_return_val_if_fail(g_utf8_validate(text, -1, NULL), 0);
  g_return_val_if_fail(sql_highlighting, 0);

  sql_highlighting->words_num= 0;

  if (!*text) return 0;

  if (line_no == 0)
  {
    /* there is no previous state/line */
    state= 0;
    token_begin= -1;
  }
  else
  {
    if (line_no >= sql_highlighting->line_states_size)
    {
      sql_highlighting->line_states_size+= ARRAY_INC_SIZE;
      sql_highlighting->line_states= g_realloc(sql_highlighting->line_states, sizeof(MYX_LINE_STATE)*sql_highlighting->line_states_size);
    }
    state= sql_highlighting->line_states[line_no-1].state; 
    token_begin= (sql_highlighting->line_states[line_no-1].token_begin!=-1)? 0: -1;
  }

  p= text;
  counter= 0;

  while( *p)
  {
    gunichar c= g_utf8_get_char(p);

    last_state= state;
    state= make_state_transition(state, c);
    assert(state != 13);

    if (state == 1) 
    {
      assert(c == ';');

      if (token_begin != -1)
	make_new_word(sql_highlighting, token_begin, counter-1, text, last_state);

      /* make a token for the semicolon */
      make_new_word(sql_highlighting, counter, counter, text, 0);

      token_begin= -1;
      state= 0; 
    }
    else if (state == 0 || state == 4 || state == 7)
    {
      if (token_begin == -1)
      {
	if ( isspace(c))
	{
	  /* do nothing */
	}
	else if (is_delimiter(c) && (state !=4 || c != '-' ))
	{
	  const char *q= p;

	  /* let the delimiter be a token for itself */
	  while (is_delimiter(*q)) q= g_utf8_next_char(q);

	  make_new_word(sql_highlighting, counter, g_utf8_prev_char(q)-p+counter, text, 0);

	  counter=  g_utf8_prev_char(q)-p+counter;
	  p= g_utf8_prev_char(q);
	  token_begin= -1;
	}
	else 
	{
	  token_begin= counter;
	}
      }
      else /* a token has already begun */
      {
	/* check if the token is already ending */

	if (last_state == 12 || last_state == 6 || last_state == 5 || last_state == 3 || last_state == 2)
	{
	  make_new_word(sql_highlighting, token_begin, counter, text, last_state);
	  
	  token_begin= -1;
	}
	else if (isspace(c))
	{
	  make_new_word(sql_highlighting, token_begin, counter-1, text, last_state);
	  
	  token_begin= -1;
	}
	else if (is_delimiter(c))
	{
	  const char *q= p;

	  make_new_word(sql_highlighting, token_begin, counter-1, text, last_state);

	  /* let the delimiter be a token for itself */
	  while (is_delimiter(*q)) q= g_utf8_next_char(q);

	  make_new_word(sql_highlighting, counter, g_utf8_prev_char(q)-p+counter, text, 0);

	  counter= g_utf8_prev_char(q)-p+counter;
	  p= g_utf8_prev_char(q);
	  token_begin= -1;
	}
      }
    }
    else if ( (state == 2 && c=='"') || (state==3 && c=='\'') || (state==6 && c=='#') || (state == 14 && c == '`') )
    {
      /* a quote or comment starts */

      if (token_begin != -1)
      {
        make_new_word(sql_highlighting, token_begin, counter-1, text, last_state);
      }

      token_begin= counter;
    }
    else if (state == 8 && c=='*')
    {
// uncommenting the line below causes http://bugs.mysql.com/bug.php?id=26179
//      assert(token_begin == counter -1);
    }
    else if (state == 5 && c == '-')
    {
//      assert(token_begin == counter-1);
    }

    counter+= g_utf8_next_char(p)-p;
    p= g_utf8_next_char(p);
  }

  /* return the last token somehow although it didn't really end */
  if (state == 2 || state == 3 || state == 5 || state == 6 || state == 12)
  {
    make_new_word(sql_highlighting, token_begin, counter-1, text, state);
  }
  else if (state == 8)
  {
    make_new_word(sql_highlighting, token_begin, counter-1, text, 12);
  }
  else if (state == 11)
  {
    make_new_word(sql_highlighting, token_begin, counter-1, text, 2);
  }
  else if (state == 10)
  {
    make_new_word(sql_highlighting, token_begin, counter-1, text, 3);
  }
  else
  {
    if (token_begin != -1)
    {
      make_new_word(sql_highlighting, token_begin, counter-1, text, state);
      token_begin= -1;
    }
  }

  if (line_no > sql_highlighting->line_states_size)
  {
    sql_highlighting->line_states_size+= ARRAY_INC_SIZE;
    sql_highlighting->line_states= g_realloc(sql_highlighting->line_states, sizeof(MYX_LINE_STATE) * sql_highlighting->line_states_size);
  }
  sql_highlighting->line_states[line_no].state= state;
  sql_highlighting->line_states[line_no].token_begin= (int)token_begin;

  if (state == 8 || state == 12 || state == 3 || state == 10 || state == 2 || state == 11)
  {
    /* we have opened a comment or quote */
    return 1;
  }
  else if (sql_highlighting->words_num > 0)
  {
    MYX_SYN_TYPE  s= sql_highlighting->words[sql_highlighting->words_num-1].word_type;
    
    if (s == MYX_SYN_COMMENT || s == MYX_SYN_STRING) //maybe it would be enough to check if the last char was /," or ' ?
      return 1;
    else
      return 0;
  } 
  else
    return 0;
}



/* 
 * PRIVATE FUNCTIONS
 */


static void free_syn_suggestion_content(MYX_SYN_SUGGESTION *s)
{
  g_free(s->name);
}

static gint comp_func (gconstpointer a,gconstpointer b, gpointer user_data)
{
  return g_strcasecmp((char *) a, (char *) b);
}

static char *get_word(const char* text, int beg, int end)
{
  char *rval;
  
  if (beg >= end)
    return g_strdup("");

  rval= g_malloc((gulong)(end-beg+2));
  strncpy(rval, text + beg, (int)(end-beg+1));
  rval[end-beg+1]= 0;

  return rval;
}

static int is_delimiter(gunichar c)
{
  switch(c) {
  case '+': return 1;
  case '-': return 1;
  case '*': return 1;
  case '!': return 1;
  case '&': return 1;
  case '(': return 1;
  case ')': return 1;
  case '<': return 1;
  case '>': return 1;
  case '|': return 1;
  case '=': return 1;
  case ',': return 1;
  default: return 0;
  }
}

static void make_new_word(MYX_SQL_HIGHLIGHTING *sql_highlighting, int begin, int end, const char *text, int last_state)
{
  char *token_text;
  MYX_SYN_TYPE type;
  MYX_SYN_WORD *word;

  sql_highlighting->words_num++;
  if (sql_highlighting->words_num > sql_highlighting->words_size)
  {
    sql_highlighting->words_size+= ARRAY_INC_SIZE;
    sql_highlighting->words= g_realloc(sql_highlighting->words, sql_highlighting->words_size * sizeof(MYX_SYN_WORD));
  }
  word= sql_highlighting->words + sql_highlighting->words_num-1;

  switch (last_state) {
  case 2: 
    word->word_type= MYX_SYN_STRING;
    break;
  case 3:
    word->word_type= MYX_SYN_STRING ;
    break;
  case 5:
    word->word_type= MYX_SYN_COMMENT;
    break;
  case 6:
    word->word_type=MYX_SYN_COMMENT;
    break;
  case 12:
    word->word_type= MYX_SYN_COMMENT;
    break;
  case 14:
    begin++;
    end--;
    break;
  default:
    word->word_type= 0;
    break;
  }

  token_text= get_word(text, begin, end);
  if (begin)
  {
    char *beg_text;
    beg_text= get_word(text,0, g_utf8_prev_char(text+begin)-text);

    word->word_begin= g_utf8_strlen(beg_text, -1);
    g_free(beg_text);
  }
  else
    word->word_begin= 0;

  word->word_end= word->word_begin + g_utf8_strlen(token_text, -1) - 1;

  if (!word->word_type)
  {
    //if (sql_highlighting->syn)
    //  type= g_tree_lookup(sql_highlighting->syn->tree,token_text);
    //else
    //  type= NULL;
    //word->word_type= (type)?(*type):MYX_SYN_NORMAL;
    if (sql_highlighting->syn)
      word->word_type= myx_get_identifier_type(sql_highlighting->syn, token_text);
    else
      word->word_type= MYX_SYN_NORMAL;
  }

  g_free(token_text);
}


MYX_SYN_TYPE myx_get_identifier_type(MYX_SYN *syn, const char *word)
{
/*
   MYX_SYN_TYPE *type= g_tree_lookup(syn->tree, word);
   if (type)
     return *type;
   else
     return MYX_SYN_NORMAL;
*/
// TODO: implement type substitution from list

  GSList *next_name= syn->synlist, *next_type= syn->syntypelist;

  for(; (next_name != NULL) && (next_type != NULL); 
    next_type= g_slist_next(next_type), next_name= g_slist_next(next_name))
  {
    size_t len= strlen(next_name->data);
    if(g_strncasecmp(next_name->data, word, len) == 0)
    {
      return *(MYX_SYN_TYPE *)next_type->data;
    }
  }
  
  return MYX_SYN_NORMAL;
}


static gboolean add_suggestions (gpointer key, gpointer value, gpointer data)
{
  MYX_SYN_SUGGESTIONS *s;

  s= (MYX_SYN_SUGGESTIONS *) data;
  s->suggestions_num++;
  s->suggestions=  g_realloc(s->suggestions, sizeof(MYX_SYN_SUGGESTION) * s->suggestions_num);
  s->suggestions[s->suggestions_num-1].name= g_strdup(key);
  s->suggestions[s->suggestions_num-1].s_type= *((MYX_SYN_TYPE *)value);

  return 0;
}


static int make_state_transition(int state, gunichar c)
{
  
  switch(c){

    case ';':
      state=state_machine[state][0];
      break;
    case '\'':
      state=state_machine[state][1];
      break;
    case '"':
      state=state_machine[state][2];
      break;
    case '#':
      state=state_machine[state][3];
      break;
    case '-':
      state=state_machine[state][4];
      break;
    case '/':
      state=state_machine[state][5];
      break;
    case '*':
      state=state_machine[state][6];
      break;
    case '\n':
      state=state_machine[state][7];
      break;
    case '\\':
      state=state_machine[state][8];
      break;
    case '!':
      state=state_machine[state][10];
      break;
    case '`':
      state=state_machine[state][11];
      break;
    default:
      state=state_machine[state][9];
      break;
    } 
  
  return state;
}



/* from the glib-library */

#if 0

static gint g_tree_node_in_order (GTreeNode     *node,
		      GTraverseFunc  traverse_func,
		      gpointer       data)
{
  if (node->left)
    {
      if (g_tree_node_in_order (node->left, traverse_func, data))
	return TRUE;
    }
  if ((*traverse_func) (node->key, node->value, data))
    return TRUE;
  if (node->right)
    {
      if (g_tree_node_in_order (node->right, traverse_func, data))
	return TRUE;
    }

  return FALSE;
}

static GTreeNode *tree_node_lookup (GTreeNode        *node, gconstpointer     key)
{
  gint cmp;

  if (!node || !node->key)
    return NULL;

  cmp = g_strncasecmp(key, node->key, (guint)strlen(key));

  if (cmp == 0)
    return node;

  if (cmp < 0)
    {
      if (node->left)
	return tree_node_lookup (node->left, key);
    }
  else if (cmp > 0)
    {
      if (node->right)
	return tree_node_lookup (node->right, key);
    }

  return NULL;
}

#endif
