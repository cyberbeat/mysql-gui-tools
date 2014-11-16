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

#ifndef myx_xml_util_functions_h
#define myx_xml_util_functions_h

#ifdef __cplusplus
extern "C" {
#endif

#include "myx_util.h"

#include <libxml/tree.h>

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif

typedef char myx_bool;

MYX_PUBLIC_FUNC xmlDocPtr myx_xmlParseFile(const char *filename);
MYX_PUBLIC_FUNC int myx_xmlSaveFile(const char *filename, xmlDocPtr doc);

MYX_PUBLIC_FUNC char* int_to_str(int number);

MYX_PUBLIC_FUNC char* bool_to_str(myx_bool b);

MYX_PUBLIC_FUNC int atoi_and_free(xmlChar *string);
MYX_PUBLIC_FUNC double atof_and_free(xmlChar *string);

MYX_PUBLIC_FUNC xmlChar* get_failsafe_utf8(const unsigned char* latin_str);

MYX_PUBLIC_FUNC int file_exists(const char *filename);

MYX_PUBLIC_FUNC long long file_size(const char *filename);

MYX_PUBLIC_FUNC xmlNodePtr NewTextChild_int_content(xmlNodePtr parent,
                                                    xmlNsPtr ns, 
                                                    const xmlChar *name,
                                                    int content_int);
MYX_PUBLIC_FUNC
      xmlNodePtr NewTextChild_double_content(xmlNodePtr parent, xmlNsPtr ns,
                                             const xmlChar *name,
                                             double content_double);

MYX_PUBLIC_FUNC int get_child_count(const xmlNodePtr parent,
                                    const xmlChar *element_type_name);

MYX_PUBLIC_FUNC void try_to_get_string_field(xmlDocPtr doc, xmlNodePtr cur,
                                             const char * name, char ** field);
MYX_PUBLIC_FUNC void try_to_get_int_field(xmlDocPtr doc, xmlNodePtr cur,
                                          const char * name, int * field);
MYX_PUBLIC_FUNC void try_to_get_char_field(xmlDocPtr doc, xmlNodePtr cur,
                                           const char * name, char * field);
MYX_PUBLIC_FUNC void try_to_get_float_field(xmlDocPtr doc, xmlNodePtr cur,
                                           const char * name, double * field);

MYX_PUBLIC_FUNC xmlNodePtr try_to_get_child(xmlDocPtr doc, 
                                            xmlNodePtr cur,
                                            const char * name);

#ifdef __cplusplus
}
#endif
#endif
