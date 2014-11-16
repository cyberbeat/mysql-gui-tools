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


#include <myx_qb_library.h>

#include <glib.h>

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>

#include <assert.h>
#include <myx_xml_util_functions.h>
#include <string.h>
#include <time.h>



#define DEFAULT_CATALOG_NAME "DEFAULT_CATALOG"
#define DEFAULT_SCHEMA_NAME "DEFAULT_SCHEMA"

/*
* Forward declarations
*/

static void read_in_history_entry(xmlNodePtr node, MYX_HISTORY_ENTRY *entry);
static void free_history_entry_content(MYX_HISTORY_ENTRY *he);
static void store_history_entry(MYX_HISTORY_ENTRY *he, xmlNodePtr parent);
static void split_iso8601_date(const char* iso_date, int *year, int *month, 
                               int *day, int *hour, int *min, int *sec);
static int history_entry_compare_asc(const void *a, const void *b);
static int history_entry_compare_desc(const void *a, const void *b);
static void free_history_interval_content(MYX_HISTORY_INTERVAL *hi);
static void free_history_catalog_content(MYX_HISTORY_CATALOG *hc);
static void free_history_schema_content(MYX_HISTORY_SCHEMA *hs);
static void insert_into_catalog(MYX_HISTORY_CATALOG *catalog, MYX_HISTORY_ENTRY *entry);
static void insert_into_interval(MYX_HISTORY_TREE *tree, MYX_HISTORY_ENTRY *entry,
                                 MYX_HISTORY_INTERVAL_TYPE interval_type);



/*
* Public functions 
*/



MYX_HISTORY * myx_history_new()
{
  MYX_HISTORY *history;

  history= g_malloc0(sizeof(MYX_HISTORY));

  return history;  
}

MYX_HISTORY * myx_history_load(const char *filename, MYX_LIB_ERROR *error_code)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  MYX_HISTORY *history;
  char* local_filename;
  *error_code = MYX_NO_ERROR;

  if ((local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL)) == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
  {
    *error_code = MYX_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  doc = myx_xmlParseFile(filename);

  if (doc == NULL ) {
    *error_code = MYX_XML_PARSE_ERROR;
    return NULL;
  }


  root = xmlDocGetRootElement(doc);

  if (root == NULL) {
    *error_code = MYX_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  history= g_malloc0(sizeof(MYX_HISTORY));

  history->entries_num= get_child_count(root, (xmlChar*)"history_entry");

  if (history->entries_num > 0)
  {
    int i;
    xmlNodePtr cur;

    history->entries= g_malloc0(history->entries_num * sizeof(MYX_HISTORY_ENTRY));

    i=0;
    cur = root->children;
    while (cur != NULL) 
    {
      if (!xmlStrcmp(cur->name, (xmlChar*)"history_entry"))
      {
        read_in_history_entry(cur, history->entries+i);
        i++;
      }

      cur = cur->next;
    }
  }


  xmlFreeDoc(doc);

  return history;
}


MYX_LIB_ERROR myx_history_store(const char *filename, MYX_HISTORY *history)
{
  xmlDocPtr doc;
  xmlNodePtr root_node;
  unsigned int i;

  g_return_val_if_fail(history, -1);
  g_return_val_if_fail(filename, -1);

  doc = xmlNewDoc((xmlChar*)"1.0");

  root_node = doc->children = xmlNewDocRawNode(doc, NULL, (xmlChar*)"history", NULL);  

  for (i=0; i < history->entries_num; i++)
  {
    MYX_HISTORY_ENTRY *entry= history->entries+i;
    if(entry->marked_deleted != 1)
      store_history_entry(history->entries+i, root_node);
  }

  if (myx_xmlSaveFile(filename, doc) == -1)
  {
    xmlFreeDoc(doc);
    return -1;
  }

  xmlFreeDoc(doc);
  return 0;
}




void myx_history_free(MYX_HISTORY *history)
{
  if (history)
  {
    unsigned int i;

    for(i=0; i < history->entries_num; i++)
    {
      free_history_entry_content(history->entries+i);
    }

    g_free(history->entries);
    g_free(history);
  }
}




MYX_HISTORY_TREE *myx_history_get_tree(MYX_HISTORY *history)
{
  GDate cur_date;
  MYX_HISTORY_TREE *history_tree;
  unsigned int i;
  time_t timestamp;
  struct tm *cur_tm_date;

  history_tree= g_malloc0(sizeof(MYX_HISTORY_TREE));

  /* sort all history entries according to their date */
  qsort(history->entries, history->entries_num, sizeof(MYX_HISTORY_ENTRY), history_entry_compare_asc);


  /* get current date */
  timestamp= time(NULL);
  cur_tm_date= localtime(&timestamp); // use local time for comparation, not UTC.
  g_date_set_dmy(&cur_date, (GDateDay) cur_tm_date->tm_mday, (GDateMonth) (cur_tm_date->tm_mon + 1),
    (GDateYear) (cur_tm_date->tm_year + 1900));

  for (i=0; i < history->entries_num; i++)
  {
    GDate tmp_date;
    int diff_days;
    struct tm entryDate;
    time_t dummy;
    struct tm* localEntryDate;

    /* put each history_entry into
    * the appropriate category/group
    */

    split_iso8601_date(history->entries[i].date_last_access, &entryDate.tm_year, &entryDate.tm_mon, &entryDate.tm_mday,
      &entryDate.tm_hour, &entryDate.tm_min, &entryDate.tm_sec);
    --entryDate.tm_mon;
    entryDate.tm_year -= 1900;
    entryDate.tm_isdst  = cur_tm_date->tm_isdst;
    dummy = mktime(&entryDate);
    localEntryDate = localtime(&dummy);
    g_date_set_dmy(&tmp_date, localEntryDate->tm_mday, (GDateMonth) (localEntryDate->tm_mon + 1), localEntryDate->tm_year + 1900);

    diff_days= g_date_days_between(&cur_date, &tmp_date);

    if ((diff_days == 0) || (diff_days > 0)) 
    {
      //we will put dates that lie in the future into the the today-category
      insert_into_interval(history_tree, history->entries+i, MYX_HIT_TODAY);
    }
    else if (diff_days == -1)
    {
      insert_into_interval(history_tree, history->entries+i, MYX_HIT_YESTERDAY);
    }
    else if (diff_days == -2)
    {
      int t= g_date_get_weekday(&cur_date)-2;
      if (t <= 0)
        t+= 7;
      insert_into_interval(history_tree, history->entries+i, t);
    }
    else if (diff_days == -3)
    {
      int t= g_date_get_weekday(&cur_date)-3;
      if (t <= 0)
        t+= 7;
      insert_into_interval(history_tree, history->entries+i, t);
    }
    else if (diff_days == -4)
    {
      int t= g_date_get_weekday(&cur_date)-4;
      if (t <= 0)
        t+= 7;
      insert_into_interval(history_tree, history->entries+i, t);
    }
    else if (diff_days == -5)
    {
      int t= g_date_get_weekday(&cur_date)-5;
      if (t <= 0)
        t+= 7;
      insert_into_interval(history_tree, history->entries+i, t);
    }
    else if (diff_days == -6)
    {
      int t= g_date_get_weekday(&cur_date)-6;
      if (t <= 0)
        t+= 7;
      insert_into_interval(history_tree, history->entries+i, t);
    }
    else if (diff_days < -6 && diff_days  >= -13)
    {
      insert_into_interval(history_tree, history->entries+i, MYX_HIT_LAST_WEEK);	
    }
    else
    {
      insert_into_interval(history_tree, history->entries+i, MYX_HIT_BEFORE_LAST_WEEK);	
    }

  }

  return history_tree;
}


void  myx_history_free_tree(MYX_HISTORY_TREE *history_tree)
{
  if (history_tree)
  {
    unsigned int i;

    for (i=0; i < history_tree->history_intervals_num; i++)
    {
      free_history_interval_content(history_tree->history_intervals+i);
    }
    g_free(history_tree->history_intervals);
    g_free(history_tree);
  }
}

MYX_HISTORY_ENTRY *
myx_history_add_entry(MYX_HISTORY *history, const char *catalog, 
                      const char *schema, const char *sql, unsigned int max_entries)
{
  assert(history);
  assert(sql);

  if (!schema)
    schema= DEFAULT_SCHEMA_NAME;

  if ((((history->entries_num>0) && 
      (strcmp2(history->entries[history->entries_num - 1].sql, sql)==0))) ||
    (strcmp2(sql, "")==0))
    return NULL;

  if (history->entries_num +1 > max_entries)
  {
    /* we have to delete some old entries */

    qsort(history->entries, history->entries_num, sizeof(MYX_HISTORY_ENTRY), history_entry_compare_desc);

    while (history->entries_num +1 > max_entries)
    {
      /* delete the last item of the array */
      history->entries_num--;
    }
  }

  history->entries_num++;
  history->entries= g_realloc(history->entries, sizeof(MYX_HISTORY_ENTRY) * history->entries_num);
  history->entries[history->entries_num - 1].marked_deleted= 0;
  history->entries[history->entries_num - 1].catalog= xmlMemStrdup(catalog);
  history->entries[history->entries_num - 1].schema= xmlMemStrdup(schema);
  history->entries[history->entries_num - 1].sql= xmlMemStrdup(sql);
  history->entries[history->entries_num - 1].query_type= 0; //find_out_query_type(sql);

  /* get and set current date */
  {
    time_t timestamp;
    struct tm *cur_tm_date;
    char *g_str;

    timestamp= time(NULL);
    cur_tm_date= gmtime(&timestamp);
    //YYYY-MM-DDThh:mm:ssTZD  TZD= timezone or Z for UTC
    g_str= g_strdup_printf("%d-%02d-%02dT%02d:%02d:%02dZ", cur_tm_date->tm_year+1900,
      cur_tm_date->tm_mon+1, cur_tm_date->tm_mday,
      cur_tm_date->tm_hour, cur_tm_date->tm_min,
      cur_tm_date->tm_sec);

    history->entries[history->entries_num - 1].date_last_access= xmlMemStrdup(g_str);
    g_free(g_str);
  }


  return history->entries+(history->entries_num-1);
}





/*
* Private functions 
*/




static void 
insert_into_catalog(MYX_HISTORY_CATALOG *catalog, MYX_HISTORY_ENTRY *entry)
{
  unsigned int j; 
  int found;

  //see if we already have a schema in this catalog
  found= -1;
  for (j=0; j < catalog->schemata_num; j++)
  {
    if (!strcmp2(catalog->schemata[j].caption, entry->schema) )
    {
      found= j;
      break;
    }
  }

  if (found == -1)
  {
    catalog->schemata_num++;
    catalog->schemata= g_realloc( catalog->schemata, 
      sizeof(MYX_HISTORY_SCHEMA) * catalog->schemata_num);

    j= catalog->schemata_num - 1;
    if(entry->schema)
      catalog->schemata[j].caption= g_strdup(entry->schema);
    else
      catalog->schemata[j].caption= g_strdup("");
    catalog->schemata[j].entries_num= 0;
    catalog->schemata[j].entries= NULL;
  }

  catalog->schemata[j].entries_num++;
  catalog->schemata[j].entries= g_realloc(catalog->schemata[j].entries, 
    sizeof(MYX_HISTORY_ENTRY_PTR) * catalog->schemata[j].entries_num);
  catalog->schemata[j].entries[catalog->schemata[j].entries_num-1]= entry;
}


static void insert_into_interval(MYX_HISTORY_TREE *tree, MYX_HISTORY_ENTRY *entry,
                                 MYX_HISTORY_INTERVAL_TYPE interval_type)
{
  unsigned int i;
  int found_interval= 0;

  if(entry->marked_deleted == 0)
  {
    for(i=0; i < tree->history_intervals_num; i++)
    {
      if (tree->history_intervals[i].interval_type == interval_type)
      {
        /* we have found our category */
        MYX_HISTORY_INTERVAL *interval;
        MYX_HISTORY_CATALOG *catalog;

        found_interval= 1;
        interval= tree->history_intervals +i;
        catalog= interval->catalogs; //we only have one catalog at the moment

        insert_into_catalog(catalog, entry);             
      }
    }

    if (!found_interval)
    {
      MYX_HISTORY_INTERVAL *interval;
      MYX_HISTORY_CATALOG *catalog;

      /* create a new interval */

      tree->history_intervals_num++;
      tree->history_intervals= g_realloc(tree->history_intervals, 
        sizeof(MYX_HISTORY_INTERVAL) * 
        tree->history_intervals_num);

      interval= tree->history_intervals + tree->history_intervals_num-1;
      interval->interval_type= interval_type;
      interval->catalogs_num= 1;
      interval->catalogs= g_malloc0(sizeof(MYX_HISTORY_SCHEMA));

      catalog= interval->catalogs; //we only have one catalog
      catalog->caption= g_strdup(DEFAULT_CATALOG_NAME);

      insert_into_catalog(catalog, entry);
    }
  }
}




static void free_history_interval_content(MYX_HISTORY_INTERVAL *hi)
{
  unsigned int i;

  for (i=0; i < hi->catalogs_num; i++)
  {
    free_history_catalog_content(hi->catalogs + i);
  }
  g_free(hi->catalogs);
}


static void free_history_catalog_content(MYX_HISTORY_CATALOG *hc)
{
  unsigned int i;

  g_free(hc->caption);
  for (i=0; i < hc->schemata_num; i++)
  {
    free_history_schema_content(hc->schemata + i);
  }
  g_free(hc->schemata);
}

static void free_history_schema_content(MYX_HISTORY_SCHEMA *hs)
{
  g_free(hs->caption);
  /*
  not necessary, since we do not copy the pointers

  for (i=0; i < hs->entries_num; i++)
  {
  free_history_entry_content(hs->entries + i);
  }
  */
  g_free(hs->entries);
}


/* we want the (biggest) latest date to be the first
* item in the array
*/
static int history_entry_compare_desc(const void *a_,const void *b_)
{
  MYX_HISTORY_ENTRY *a= (MYX_HISTORY_ENTRY *) a_;
  MYX_HISTORY_ENTRY *b= (MYX_HISTORY_ENTRY *) b_;
  int a_year, a_month, a_day, a_hour, a_min, a_sec;
  int b_year, b_month, b_day, b_hour, b_min, b_sec;

  split_iso8601_date(a->date_last_access, &a_year, &a_month, &a_day,
    &a_hour, &a_min, &a_sec);
  split_iso8601_date(b->date_last_access, &b_year, &b_month, &b_day,
    &b_hour, &b_min, &b_sec);


  if (a_year < b_year)
    return 1;
  else if (a_year > b_year)
    return -1;

  if (a_month < b_month)
    return 1;
  else if (a_month > b_month)
    return -1;

  if (a_day < b_day)
    return 1;
  else if (a_day > b_day)
    return -1;

  if (a_hour < b_hour)
    return 1;
  else if (a_hour > b_hour)
    return -1;

  if (a_min < b_min)
    return 1;
  else if (a_min > b_min)
    return -1;

  if (a_sec < b_sec)
    return 1;
  else if (a_sec > b_sec)
    return -1;

  return 0;
}

static int history_entry_compare_asc(const void *a_,const void *b_)
{
  MYX_HISTORY_ENTRY *a= (MYX_HISTORY_ENTRY *) a_;
  MYX_HISTORY_ENTRY *b= (MYX_HISTORY_ENTRY *) b_;
  int a_year, a_month, a_day, a_hour, a_min, a_sec;
  int b_year, b_month, b_day, b_hour, b_min, b_sec;

  split_iso8601_date(a->date_last_access, &a_year, &a_month, &a_day,
    &a_hour, &a_min, &a_sec);
  split_iso8601_date(b->date_last_access, &b_year, &b_month, &b_day,
    &b_hour, &b_min, &b_sec);


  if (a_year < b_year)
    return -1;
  else if (a_year > b_year)
    return 1;

  if (a_month < b_month)
    return -1;
  else if (a_month > b_month)
    return 1;

  if (a_day < b_day)
    return -1;
  else if (a_day > b_day)
    return 1;

  if (a_hour < b_hour)
    return -1;
  else if (a_hour > b_hour)
    return 1;

  if (a_min < b_min)
    return -1;
  else if (a_min > b_min)
    return 1;

  if (a_sec < b_sec)
    return -1;
  else if (a_sec > b_sec)
    return 1;

  return 0;
}

static void split_iso8601_date(const char* iso_date, int *year, int *month, 
                               int *day, int *hour, int *min, int *sec)
{
  //assert(strlen(iso_date) >= 10);
  if(strlen(iso_date) >= 10)
  {
    *year= (iso_date[0]-'0') * 1000 + (iso_date[1]-'0') * 100 + (iso_date[2]-'0') * 10 + 
      (iso_date[3]-'0'); 
    *month= (iso_date[5]-'0') * 10 + (iso_date[6]-'0');
    *day= (iso_date[8]-'0') * 10 + (iso_date[9]-'0');

    if (strlen(iso_date) < 19)
    {
      //old file
      *hour= *min= *sec= 0;
      return;
    }

    *hour= (iso_date[11]-'0') * 10 + (iso_date[12]-'0');
    *min= (iso_date[14]-'0') * 10 + (iso_date[15]-'0');
    *sec= (iso_date[17]-'0') * 10 + (iso_date[18]-'0');

    if((*hour < 0) || (*hour > 23) || (*min < 0) || (*min > 59) || (*sec < 0) || (*sec > 59) 
      || (*month < 1) || (*month > 12) || (*day < 1) || (*day > 31)) 
    {
      *year= 2004;
      *month= 2;
      *day= 1;
      *hour= *min= *sec= 0;
    }
  }
  else  // invalid date format
  {
    *year= 2004;
    *month= 2;
    *day= 1;
    *hour= *min= *sec= 0;
  }
}

static void store_history_entry(MYX_HISTORY_ENTRY *he, xmlNodePtr parent)
{
  xmlNodePtr he_node;

  he_node= xmlNewTextChild(parent, NULL, (xmlChar*)"history_entry", NULL);

  xmlNewTextChild(he_node, NULL, (xmlChar*)"catalog",(xmlChar*)he->catalog);
  xmlNewTextChild(he_node, NULL, (xmlChar*)"schema",(xmlChar*)he->schema);
  xmlNewTextChild(he_node, NULL, (xmlChar*)"sql",(xmlChar*)he->sql);
  NewTextChild_int_content(he_node, NULL, (xmlChar*)"query_type", he->query_type); 
  xmlNewTextChild(he_node, NULL, (xmlChar*)"date_last_access",(xmlChar*)he->date_last_access);
}

static void free_history_entry_content(MYX_HISTORY_ENTRY *he)
{
  xmlFree(he->catalog);
  xmlFree(he->schema);
  xmlFree(he->sql);
  xmlFree(he->date_last_access);
}


static void read_in_history_entry(xmlNodePtr node, MYX_HISTORY_ENTRY *entry)
{
  xmlNodePtr cur;
  xmlDocPtr doc= node->doc;

  cur = node->children;
  while (cur != NULL) 
  {
    if (!xmlStrcmp(cur->name, (xmlChar*)"catalog"))
    {
      entry->catalog= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"schema"))
    {
      entry->schema= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"sql"))
    {
      entry->sql= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"query_type"))
    {
      entry->query_type= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"date_last_access"))
    {
      entry->date_last_access= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }

    cur= cur->next;
  }
}
