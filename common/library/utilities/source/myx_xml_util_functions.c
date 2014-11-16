/* Copyright (C) 2005 MySQL AB

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA */


/*
*
* (XML Load/Save) Helper Functions
*
*
*/

#include <sys/types.h>
#include <sys/stat.h>
#ifdef __GNUC__
  #include <unistd.h>
#endif

#include "myx_xml_util_functions.h"
#include "myx_shared_util_functions.h"
#include <string.h>
#include <stdlib.h>
#include <glib.h>

///////////////////////////////////////////////////////////////////////////////
/** @brief print the number to g_malloc-ed string
    @param number number to print
    @return g_malloc-ed string with number
*//////////////////////////////////////////////////////////////////////////////
char* int_to_str(int number)
{
  char buf[12];
  size_t len= sprintf(buf, "%d", number)+1;
  char *result= (char*) g_malloc((gulong)len);
  if (result)
    memcpy(result,buf,len);
  return result;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief print the myx_bool value to g_malloc-ed string
    @param b value to print
    @return g_malloc-ed string ("0" or "1")
*//////////////////////////////////////////////////////////////////////////////
char* bool_to_str(myx_bool b)
{
  char *result= (char*) g_malloc(2);
  result[0]= b ? '1' : '0';
  result[1]= '\0';
  return result;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief atoi string and xmlFree it
    @param string string for atoi
    @return parsed number, 0 if the string was empty
*//////////////////////////////////////////////////////////////////////////////
int atoi_and_free(xmlChar *string)
{
  int a= 0;
  if (string != NULL)
  {
    a= atoi((char*)string);
    xmlFree(string);
  }
  return a;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief atof string and xmlFree it
    @param string string for atof
    @return parsed number, 0.0 if the string was empty
*//////////////////////////////////////////////////////////////////////////////
double atof_and_free(xmlChar *string)
{
  double a= 0.0;
  if (string != NULL) 
  {
    a= atof((char*)string);
    xmlFree(string);
  }
  return a;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief skip all symbols in string greater than 127
    @param latin_str string to process
    @return result string alloced by g_malloc

    we just skip all characters above 127 and return safe ascii-chars only
*//////////////////////////////////////////////////////////////////////////////
xmlChar* get_failsafe_utf8(const unsigned char* latin_str)
{
  int i,j;
  xmlChar *result;
  long len;

  len= (long)strlen((char*)latin_str);
  result= (xmlChar*) g_malloc(len + 1);

  j= 0;
  for (i=0; i < len; i++)
  {
    if (latin_str[i] <= 127)
      result[j++]= latin_str[i];
  }
  result[j]= '\0';

  if (j < i)
    result= (xmlChar*) realloc(result,j+1);

  return result;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get file size as 64 bit value
    @param filename file name
    @return result lon long file name, zero if an error occured
*//////////////////////////////////////////////////////////////////////////////
long long file_size(const char *filename)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  LARGE_INTEGER fsize;
  HANDLE f= CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
  ZeroMemory(&fsize, sizeof(fsize));
  GetFileSizeEx(f, &fsize);
  CloseHandle(f);
  return fsize.QuadPart;
#else
  struct stat buf;
  if(stat(filename, &buf))
  {
    return 0;
  }
  return buf.st_size;
#endif
}

///////////////////////////////////////////////////////////////////////////////
/** @brief checks if filename really exists
    @param filename path to file
    @return 0 if the file doesn't exist else 1
*//////////////////////////////////////////////////////////////////////////////
int file_exists(const char *filename)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  return _access(filename, 0) != -1 ? 1 : 0;
#else
  return access(filename, R_OK) == 0 ? 1 : 0;
#endif
}

///////////////////////////////////////////////////////////////////////////////
/** @brief create new xml node in xml-dom-document with int content
    @param parent parent for node
    @param ns node sequence?? \b vva_todo
    @param name name of new node
    @param content_int value of new node
    @return created node
*//////////////////////////////////////////////////////////////////////////////
xmlNodePtr NewTextChild_int_content(xmlNodePtr parent, xmlNsPtr ns,
                                    const xmlChar *name, int content_int)
{
  xmlNodePtr node;
  char content[12];
  sprintf(content,"%d",content_int);
  node= xmlNewTextChild(parent, ns, name, (xmlChar*)content);
  return node;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief create new xml node in xml-dom-document with double content
    @param parent parent for node
    @param ns node sequence?? \b vva_todo
    @param name name of new node
    @param content_double value of new node
    @return created node
*//////////////////////////////////////////////////////////////////////////////
xmlNodePtr NewTextChild_double_content(xmlNodePtr parent, xmlNsPtr ns,
                                       const xmlChar *name,
                                       double content_double)
{
  xmlNodePtr node;
  char* content =  g_strdup_printf("%f", content_double);
  node = xmlNewTextChild(parent, ns, name, (xmlChar*)content);
  g_free(content);
  return node;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief returns the number of children of the specified type 
            the parent-node has
    @param parent node to calculate children of
    @param element_type_name name of childrens to calculate
    @return the calculated number of children
*//////////////////////////////////////////////////////////////////////////////
int get_child_count(const xmlNodePtr parent, const xmlChar *element_type_name)
{
  xmlNodePtr cur;
  int node_count = 0;

  for (cur= parent->children; cur != NULL; cur= cur->next)
  {
    if ( !xmlStrcmp(cur->name, element_type_name) &&
        cur->type == XML_ELEMENT_NODE)
      node_count++;
  }

  return node_count;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Wrapper around xmlParseFile that expects a filename
                                in UTF-8 encoding
    @param filename name of file to parse
    @return NULL if execution of g_filename_from_utf8 wasn't success else
            parsed xml document
*//////////////////////////////////////////////////////////////////////////////
xmlDocPtr myx_xmlParseFile(const char *filename)
{
  char * local_filename;
  xmlDocPtr doc;

  if ((local_filename= g_filename_from_utf8(filename,-1,NULL,NULL,NULL)) == NULL)
    return NULL;

  doc= xmlParseFile(local_filename);
  g_free(local_filename);

  return doc;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Wrapper around xmlSaveFormatFile that expects a filename 
             in UTF-8 encoding
    @param filename name of file to save to
    @param doc xml document to save
    @return -1 if execution of g_filename_from_utf8 wasn't success else
                result of xmlSaveFormatFile (number of bytes written, or -1)
*//////////////////////////////////////////////////////////////////////////////
int myx_xmlSaveFile(const char *filename, xmlDocPtr doc)
{
  char * local_filename;
  int result;
  FILE* file;

  if ((local_filename= g_filename_from_utf8(filename,-1,NULL,NULL,NULL)) == NULL)
    return -1;

  // Check if the file already exists and if so store under a temporary name first.
  file = fopen(local_filename, "r");
  if (file != NULL)
  {
    char *tempName= g_strdup_printf("%s.tmp", local_filename);

    fclose(file);
    result = xmlSaveFormatFile(tempName, doc, 1);
    if (result > 0)
    {
      // If saving the content was successful then delete the old file and use the new one.
      remove(local_filename);
      rename(tempName, local_filename);
    };
    g_free(tempName);
  }
  else
    result= xmlSaveFormatFile(local_filename,doc,1);
  g_free(local_filename);

  return result;
}



///////////////////////////////////////////////////////////////////////////////
/** @brief if there's node's name asked, get its value
    @param doc xml-document
    @param cur xml-node
    @param name asked name
    @param field value to return

    if  field was not null then xmlFree it
*//////////////////////////////////////////////////////////////////////////////
void try_to_get_string_field(xmlDocPtr doc, xmlNodePtr cur,
                             const char * name, char ** field)
{
  if ( !xmlStrcmp(cur->name, (xmlChar*)name) )
  {
    if (*field)
      xmlFree(*field);
    *field= (char*)xmlNodeListGetString(doc, cur->children, 1);
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief if there's node's name asked, get its value
    @param doc xml-document
    @param cur xml-node
    @param name asked name
    @param field value to return

    calls atoi_and_free()
*//////////////////////////////////////////////////////////////////////////////
void try_to_get_int_field(xmlDocPtr doc, xmlNodePtr cur,
                          const char * name, int * field)
{
  if ( !xmlStrcmp(cur->name, (xmlChar*)name) )
    *field= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
}

///////////////////////////////////////////////////////////////////////////////
/** @brief if there's node's name asked, get its value
    @param doc xml-document
    @param cur xml-node
    @param name asked name
    @param field value to return

    calls atoi_and_free()
*//////////////////////////////////////////////////////////////////////////////
void try_to_get_char_field(xmlDocPtr doc, xmlNodePtr cur,
                           const char * name, char * field)
{
  if ( !xmlStrcmp(cur->name, (xmlChar*)name) )
    *field= (char)atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
}

///////////////////////////////////////////////////////////////////////////////
/** @brief if there's node's name asked, get its value
    @param doc xml-document
    @param cur xml-node
    @param name asked name
    @param field value to return

    calls atof_and_free()
*//////////////////////////////////////////////////////////////////////////////
void try_to_get_float_field(xmlDocPtr doc, xmlNodePtr cur,
                           const char * name, double * field)
{
  if ( !xmlStrcmp(cur->name, (xmlChar*)name) )
    *field= (char)atof_and_free(xmlNodeListGetString(doc, cur->children, 1));
}

///////////////////////////////////////////////////////////////////////////////
/** @brief try to get a child with given name
    @param doc xml-document
    @param cur xml-node
    @param name asked name
    @return child xmlNodePtr or NULL

*//////////////////////////////////////////////////////////////////////////////
xmlNodePtr try_to_get_child(xmlDocPtr doc, 
                            xmlNodePtr cur,
                            const char * name)
{
  xmlNodePtr child;
  for (child= cur->children; child; child= child->next)
  {
    if (!xmlStrcmp(child->name, (xmlChar*)name) )
    {
      return child;
    }
  }
  return NULL;
}
