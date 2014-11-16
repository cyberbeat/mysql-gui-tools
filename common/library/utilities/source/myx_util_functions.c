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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "myx_util.h"
#include "myx_shared_util_functions.h"

#include "entities.h"

// Windows includes
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#include <direct.h>
#else
// unix/linux includes

#include <sys/time.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/utsname.h> // uname()
#include <fcntl.h>

#define SIZE_T size_t

# include <sys/types.h>
# include <sys/stat.h>
# include <unistd.h>
#endif

// MacOS X
#if defined(__APPLE__) && defined(__MACH__)
#include <sys/sysctl.h>
#include <mach/machine.h>
#endif

#include "myx_util_functions.h"


//----------------------------------------------------------------------------------------------------------------------

/* Transforms a string consisting of
 * 2byte hexadecimal characters into
 * a binary string.
 *
 * If ret_str_len is not NULL it will be set
 * to the length of the resulting string.
 *
 * The (newly allocated) returned string
 * is exactly strlen(hex_str)/2 bytes long.
 * The returned string should be considered
 * binary. It may contain embedded 0's.
 */
char* hex_decode(const char *hex_str, int *ret_str_len)
{
  int i;
  int hex_str_len, result_len;
  char *result;

  g_return_val_if_fail(hex_str, NULL);

  hex_str_len= (int) strlen(hex_str);
  g_return_val_if_fail(hex_str_len%2 == 0, NULL);

  result_len= hex_str_len/2;  
  result= (char*) g_malloc(result_len+1);
  if (ret_str_len) *ret_str_len= result_len;

  for (i= 0; i < result_len; i++)
  {
    unsigned int num;
    sscanf(hex_str+i*2, "%2x", &num);
    result[i]= num;
  }
  result[i]= 0;

  return (char*)result;
}

//----------------------------------------------------------------------------------------------------------------------

/* This functions transforms an arbitrary sequence of bytes
 * into a string consisting of 0-9 and A-F characters.
 * Each byte of the input string is mapped to two bytes 
 * in the resulting-string.
 *
 * len is the length of the binary_string or -1 if the
 * string is a normal null-terminated string.
 *
 * The returned string is a valid ASCII string and thus also
 * a valid UTF-8 string.
 */
char *hex_encode(const char *binary_string, int len)
{
  static char* hex_char= "0123456789ABCDEF";
  int binary_string_len;
  char *hex_str;
  int j,k;
  const char *from;

  g_return_val_if_fail(binary_string,NULL);
  g_return_val_if_fail(len, NULL);

  if (len == -1)
    binary_string_len= (int) strlen(binary_string);
  else
    binary_string_len= len;

  
  hex_str= (char*) g_malloc(binary_string_len*2+1);

  from= binary_string;
  for (j=0,k=0; k < binary_string_len; j+=2, k++)
  {
    unsigned int data= (unsigned int) (unsigned char) from[k];
    hex_str[j]= hex_char[data >> 4];
    hex_str[j+1]= hex_char[data & 15];
  }
  hex_str[j]= 0;

  return hex_str;
}

//----------------------------------------------------------------------------------------------------------------------

static char *find_name_with_value(const MYX_NAME_VALUE_PAIR *list, const char *value)
{
  for (; list->name; list++)
  {
    if ( !g_utf8_casecollate(list->value,value) )
      return list->name;
  }

  return "latin1";
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Translates a mysql character-set name to a
 * character-set name that is understood by iconv
 *
 * Return value: The corresponding iconv char name
 *               or "-" if there is no appropriate
 *               iconv character-set
 *               Don't free the result of this function!
 */
char *iconv_char_name(const char *mysql_character_set_name)
{
  //attribute "name": iconv-name
  //attribute "value": mysql-name
  static MYX_NAME_VALUE_PAIR iconv_mysql[]=
  {
    /* different names */
    {"HP-ROMAN8","hp8"},
    {"KOI8-R", "koi8r"},
    {"US-ASCII", "ascii"},
    {"EUC-JP", "ujis"},
    {"KOI8-U", "koi8u"},
    {"ARMSCII-8", "armscii8"},
    {"UCS-2", "ucs2"},
    {"MACCENTRALEUROPE", "macce"},

    /* same names */
    {"big5", "big5"},
    {"sjis", "sjis"},
    {"cp850","cp850"},
    {"cp866", "cp866"},
    {"latin1", "latin1"},
    {"latin2", "latin2"},
    {"latin5", "latin5"},
    {"latin7", "latin7"},
    {"hebrew", "hebrew"},
    {"tis620", "tis620"},
    {"euckr", "euckr"},
    {"gb2312", "gb2312"},
    {"greek", "greek"},
    {"cp1250", "cp1250"},
    {"gbk", "gbk"},
    {"utf8", "utf8"},
    {"macroman", "macroman"},
    {"cp1251", "cp1251"},
    {"cp1256", "cp1256"},
    {"cp1257", "cp1257"},
    {NULL, NULL}
  };
  char *iconv_name;

  iconv_name= find_name_with_value(iconv_mysql,mysql_character_set_name);
  return iconv_name;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Normally a mysql identifier is quoted with ` (backtick).
 * In Ansi Mode " is also a valid identifier.
 * This function handles both.
 */
char* unquote_identifier(char *table_name)
{
  char *beginning= table_name;

  if (*table_name == '"' || *table_name == '`')
  {
    size_t len = strlen(table_name) - 2;
    strncpy(beginning, table_name + 1, len);
    beginning[len] = '\0';
  };
  return beginning;
}

//----------------------------------------------------------------------------------------------------------------------

/* This is a helper function
 * that is only used by
 * quote_identifier
 */
static char *quote_name(const char *name, char *buff, char quote_char)
{
  char *to= buff;

  *to++= quote_char;
  while (*name)
  {
    if (*name == quote_char)
      *to++= quote_char;
    *to++= *name++;
  }
  to[0]= quote_char;
  to[1]= 0;
  return buff;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Returns a string allocated with gmalloc and surrounded by the given quote char.
 *
 * @param identifier The string to quote.
 *
 * @return A newly allocated string containing the quoted version of identifier.
 */
char *quote_identifier(const char *identifier, const char quote_char)
{
  char *buff;

  /* worst case: identifier consists of backticks only */
  buff= (char*) g_malloc((gulong)(strlen(identifier) * 2 + 3));

  return quote_name(identifier,buff, quote_char); 
}

//----------------------------------------------------------------------------------------------------------------------

char *mask_out_string_re(char *str, const char *open_re,
                         const char open_trigger, const char close_trigger,
                         const char mask)
{
  int maskout= 0;
  unsigned int i;
  pcre *pcre_exp;
  const char *error_str;
  int erroffset;
  int o_vector[10];
  int rc;
  int start= -1;
  size_t len= strlen(str);
  
  pcre_exp= pcre_compile(open_re, PCRE_CASELESS, &error_str, &erroffset, NULL);
  g_return_val_if_fail(pcre_exp!=NULL, str);

  rc= pcre_exec(pcre_exp, NULL, str, (int)len, 0, 0, o_vector, 3);
  if (rc > 0)
    start= o_vector[0];
  pcre_free(pcre_exp);

  if (start < 0)
    return str;

  for (i= start; str[i]!=0; i++)
  {
    int c= str[i];
    if((c==close_trigger)&&(maskout>0))
    {
      maskout--;
      if(!maskout)
        continue;
    }
    if(maskout>0)
    {
      str[i]= mask;
    }
    if(c==open_trigger)
    {
      maskout++;
    }
  }

  return str;
}

//----------------------------------------------------------------------------------------------------------------------

char *mask_out_string(char *str, const char open_trigger,
                      const char close_trigger, const char mask)
{
  int maskout= 0;
  unsigned int i;

  for(i=0;str[i]!=0;i++)
  {
    int c= str[i];
    if((c==close_trigger)&&(maskout>0))
    {
      maskout--;

      if(!maskout)
        continue;
    }

    if(maskout>0)
    {
      str[i]= mask;
    }

    if(c==open_trigger)
    {
      maskout++;
    }
  }

  return str;
}

//----------------------------------------------------------------------------------------------------------------------

static void __sappend(char **str, int *ressize, int *reslen, const char *sbegin, int count)
{
  if (*reslen + count + 1 >= *ressize)
  {
    *ressize+= count + 100;
    *str= (char*) g_realloc(*str, *ressize);
  }
  strncpy(*str+*reslen, sbegin, count);
  *reslen+= count;
  (*str)[*reslen]= 0;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_g_subst(const char *str, const char *search, const char *replace)
{
  int ressize, reslen;
  int replsize= (int)strlen(replace);
  int searchlen= (int)strlen(search);
  char *res;
  const char *bptr, *ptr;

  g_return_val_if_fail(str != NULL, g_strdup(str));
  if(str[0] == '\0')
  {
    return g_strdup(str);
  }
  g_return_val_if_fail(search != NULL && *search, g_strdup(str));
  g_return_val_if_fail(replace != NULL, g_strdup(str));

  ressize= (int)strlen(str)+1;
  reslen= 0;
  res= (char*) g_malloc(ressize);

  bptr= str;

  while ((ptr= strstr(bptr, search)) != NULL)
  {
    __sappend(&res, &ressize, &reslen, bptr, (int)(ptr-bptr));
    bptr= ptr+searchlen;
    __sappend(&res, &ressize, &reslen, replace, replsize);
  }
  __sappend(&res, &ressize, &reslen, bptr, (int)strlen(bptr));

  return res;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Replaces the search string with the replace string
 * in the given str
 *
 * @str becomes invalid after calling this
 * function!
 *
 * Return value: Newly allocated result string
 **/
char *str_g_replace(char *str, const char *search, const char *replace)
{
  char *res;

  if (!replace)
    replace= "";

  res= str_g_subst(str, search, replace);
  g_free(str);
  return res;
}


/**
 * Appends the @addon string to the end of
 * @base_str. @base_str is expected to have
 * been allocated with a glib-memory allocatio
 * function.
 *
 * @base_str becomes invalid after calling this
 * function!
 *
 * Return value: The joined string
 **/
char *str_g_append(char *base_str, const char *addon)
{
  unsigned int addon_len;
  char *tmp;

  if(!base_str)
  {
    if(!addon)
      base_str= g_strdup("");
    else
      base_str= g_strdup(addon);
  }
  else
  {
    if(addon)
    {
      addon_len= (unsigned int)strlen(addon);

      tmp= (char*) g_realloc(base_str, (unsigned int)strlen(base_str)+addon_len+1);

      base_str= strncat(tmp, addon,addon_len);
    }
  }

  return base_str;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_g_append_and_free(char *base_str, char *addon)
{
  char *tmp= str_g_append(base_str, addon);

  g_free(addon);

  return tmp;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_g_insert(const char *base_str, const char *addon, unsigned int index)
{
  char *tmp= (char*) g_malloc(sizeof(char *)*(unsigned int)(strlen(base_str)+strlen(addon)+1));

  g_stpcpy(tmp, base_str);
  g_stpcpy(tmp+index, addon);
  g_stpcpy(tmp+(index+(unsigned int)(strlen(addon))), base_str+index);

  return tmp;
}

//----------------------------------------------------------------------------------------------------------------------

//from libmysql/strmov.c
char *strmov(register char *dst, register const char *src)
{
  while ((*dst++ = *src++) != NULL)
   ;
  return dst-1;
}

//----------------------------------------------------------------------------------------------------------------------

int strcmp2(const char *str1, const char *str2)
{
  if((!str1)&&(!str2))
    return 0;
  if((!str1)||(!str2))
    return -1;
  else
    return strcmp(str1, str2);
}

//----------------------------------------------------------------------------------------------------------------------

int strcmp3(const char *str1, const char *str2)
{
  if((!str1)&&(!str2))
    return 0;
  else if(!str1)
  {
    if(strcmp(str2, "") == 0)
      return 0;
    else
      return -1;
  }
  else if(!str2)
  {
    if(strcmp(str1, "") == 0)
      return 0;
    else
      return -1;
  }
  else
    return strcmp(str1, str2);
}

//----------------------------------------------------------------------------------------------------------------------

unsigned int strlen2(const char *str)
{
  if(!str)
    return 0;
  else
    return (unsigned int)strlen(str);
}

//----------------------------------------------------------------------------------------------------------------------

char *strcpy2(char *dst, const char *src)
{
  if(!src)
  {
    *dst= 0;
  }
  else
  {
    strcpy(dst, src);
  }

  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

char *utf8_str_trim(char *str)
{
  size_t len= strlen(str);
  size_t new_len;
  gchar * start, * finish;

  // Get length of leading spaces
  gchar * pos= str;
  gchar * end= str + len;
  gunichar symbol;
  for ( symbol= g_utf8_get_char(pos);
        g_unichar_isspace(symbol);
        symbol= g_utf8_get_char(pos))
  {
    pos= g_utf8_next_char(pos);
    if (pos==end)
      break;
  }
  start= pos;

  //Get length of trailing spaces
  pos= end;
//XXX
  // the following line and (g_utf8_next_char()) won't do anything
  // whoever wrote it, please check what's really intended and fix (or remove)
  //g_utf8_prev_char(pos);
  for ( symbol= g_utf8_get_char(pos);  
        g_unichar_isspace(symbol);
        symbol= g_utf8_get_char(pos))
  {
    pos= g_utf8_find_prev_char(start,pos);
    pos= g_utf8_prev_char(pos);
    if (pos==start)
      break;
  }
  //g_utf8_next_char(pos);
  finish= pos;
  new_len= finish-start;

  if (start!=end || new_len!=len)
  {
    //shift chars to the begining of the string
    memmove(str, start, new_len);

    //terminate string
    str[new_len]=0;
  }

  return str;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_trim(char *str)
{
  SIZE_T i, j;
  SIZE_T length;

  length= strlen(str);

  //Get length of leading spaces
  for (i=0; (i<length)&&isspace(str[i]); i++) 
  {}

  //Get length of trailing spaces
  for (j=length-1; (j>i)&&isspace(str[j]); j--)
  {}

  j++;
  
  //shift chars to the begining of the string
  memmove(str, str+i, j-i);

  //terminate string
  str[j-i]=0;

  return str;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_left(char *dest, char *src, unsigned int len)
{
    unsigned int i;
    unsigned int s_len= (int)strlen(src);

    if(s_len<=len)
    {
        strncpy(dest, src, s_len);
        dest[len]= 0;
    }
    else
    {
        for(i=0;i<len;i++)
        {
            dest[i]= src[i];
        }
        dest[len]= 0;
    }

    return dest;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_right(char *dest, char *src, unsigned int len)
{
    unsigned int i;
    unsigned int s_len= (unsigned int)strlen(src);

    if(s_len<=len)
    {
        strncpy(dest, src, s_len);
    }
    else
    {
        for(i=0;i<=len;i++)
        {
            dest[i]= src[i+s_len-len];
        }
    }

    return dest;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_align_left(const char *txt, unsigned int width, char linechar)
{
  char *dst= (char*) g_malloc(width + 1);
  unsigned int len= (unsigned int)strlen(txt);
  unsigned int i;

  if (len > width)
    len= width;

  memset(dst, linechar, width);
  dst[width]= 0;

  for (i= 0; i < len; i++)
    dst[i]= txt[i];

  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_align_right(const char *txt, unsigned int width, char linechar)
{
  char *dst= (char*) g_malloc(width + 1);
  unsigned int len= (unsigned int)strlen(txt);
  unsigned int i;

  if (len > width)
    len= width;

  memset(dst, linechar, width);
  dst[width]= 0;

  for (i= 0; i < len; i++)
    dst[i + width - len]= txt[i];

  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_align_center(const char *txt, unsigned int width, char linechar)
{
  char *dst= (char*) g_malloc(width + 1);
  unsigned int i;
  unsigned int len= (unsigned int)strlen(txt);
  unsigned int p= div(width, 2).quot - div(len, 2).quot;

  if (len > width)
    len= width;

  memset(dst, linechar, width);
  dst[width]= 0;

  for (i= 0; i < len; i++)
    dst[p + i]= txt[i];

  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

char *auto_line_break(const char *txt, unsigned int width, char sep)
{
  char *dst= (char*) g_malloc((width + 2) * 80);
  unsigned int i, o= 0, p= 0, w= 0, l= (unsigned int)strlen(txt);

  for (i= 0; i < l;)
  {
    w++;

    if (w > width)
    {
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
      dst[p + o]= '\r';
      dst[p + o + 1]= '\n';

      o+= 1;
#else
      dst[p + o]= '\n';
#endif
      i= p + 1;
      w= 0;
    }
    else
    {
      dst[i + o]= txt[i];

      if (txt[i] == sep)
        p= i;

        i++;
    }
  }

  dst[i + o]= 0;

  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

int sub_str_count(char *search_str, const char *src)
{
    unsigned int i;
    char *pdest;
    char *psrc = (char *)src;
    unsigned int ss_len= (unsigned int)strlen(search_str);

    i= 0;
    while((pdest= strstr(psrc, search_str)) != NULL)
    {
        i++;
        psrc= pdest+ss_len;
    }

    return i;
}

//----------------------------------------------------------------------------------------------------------------------

int str_endswith(const char *str, const char *substr)
{
  int slen= (int)strlen(str);
  int sslen= (int)strlen(substr);

  if (slen >= sslen && strcmp(str+slen-sslen, substr)==0)
      return 1;
  else
      return 0;
}

//----------------------------------------------------------------------------------------------------------------------

int str_beginswith(const char *str, const char *substr)
{
  if (str)
    return strncmp(str, substr, strlen(substr))==0;
  else
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------

int str_is_numeric(const char *str)
{
  unsigned int len= (unsigned int)strlen(str);
  unsigned int i;

  for (i= 0; i < len; i++)
    if(g_ascii_digit_value(str[i]) == -1)
      return 0;

  return 1;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_PUBLIC_FUNC int g_utf8_casecollate(const char *str1, const char *str2)
{
  char *s1= g_utf8_casefold(str1, (gssize)strlen(str1));
  char *s2= g_utf8_casefold(str2, (gssize)strlen(str2));
  int res;

  res= g_utf8_collate(s1, s2);
  g_free(s1);
  g_free(s2);

  return res;
}

//----------------------------------------------------------------------------------------------------------------------

char *str_toupper(char *str)
{
  char *s= str;

  while (*s)
  {
    *s= toupper(*s);
    s++;
  }
  return str;
}

//----------------------------------------------------------------------------------------------------------------------

char *name_of_str(char *dst, const char *src)
{
  char *dst_start= dst;

  while (*src && *src!='=')
    *dst++= *src++;

  *dst= 0;

  return dst_start;
}

//----------------------------------------------------------------------------------------------------------------------

char *value_of_str(char *dst, const char *src)
{
  char *psrc= (char *)src;

  psrc = strchr(psrc, '=');

  if (psrc)
  {
    if (*psrc)
      strcpy(dst, psrc+1);
    else
      *dst= 0;
  }
  else
    *dst= 0;

  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

int set_value(char **string_list, unsigned int string_list_num, char *name, char *new_value)
{
  unsigned int i;
  unsigned int name_len= (unsigned int)strlen(name);
  int rc= -1;

  for(i=0; i<string_list_num; i++)
  {
    if(strncmp(string_list[i], name, name_len)==0 &&
       (string_list[i][name_len]=='\0' || string_list[i][name_len]=='='))
    {

      if (!new_value)
      {
        g_free(string_list[i]);
        if (string_list_num > 0)
          memmove(string_list+i, string_list+i+1, string_list_num-1);
      }
      else
      {
        string_list[i]= (char*) g_realloc(string_list[i], (name_len+strlen(new_value)+2));
        if (string_list[i][name_len]=='=') 
          strcpy(string_list[i]+name_len+1, new_value);
        else
          sprintf(string_list[i], "%s=%s", string_list[i], new_value);
      }

      rc= 0;
      break;
    }
  }

  return rc;
}

//----------------------------------------------------------------------------------------------------------------------

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)

int get_value_from_registry(HKEY root_key, const char *sub_key, const char *key, const char *def, char *value)
{
  HKEY hSubKey;
  DWORD dwType;
  DWORD dwSize;
  LONG retval;
  unsigned char Buffer[512];

  if((retval=RegOpenKeyEx(root_key, sub_key, 0, KEY_READ, &hSubKey))==ERROR_SUCCESS)
  {
    dwSize = 512;
    if((RegQueryValueEx(hSubKey, key, NULL, &dwType, Buffer, &dwSize))==ERROR_SUCCESS)
    {
      if(dwType==REG_DWORD)
      {
        sprintf(value, "%d", Buffer[0] + Buffer[1] * 256 + Buffer[2] * 65536 + Buffer[3] * 16777216);
      }
      else
      {
        strcpy(value, (const char *)Buffer);
      }
    }
    else
    {
      strcpy(value, def);
    }

    return 0;
  }
  else
  {
    strcpy(value, "");
    return retval;
  }
}

//----------------------------------------------------------------------------------------------------------------------

int set_value_to_registry(HKEY root_key, const char *sub_key, const char *key, const char *value)
{
  HKEY hSubKey;
  LONG retval;
  DWORD dwDispo;

  if((retval= RegCreateKeyEx(root_key, sub_key, 0, "",
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hSubKey, &dwDispo))==ERROR_SUCCESS)
  {
    retval = RegSetValueEx(hSubKey, key, 0, REG_SZ, value, (DWORD)strlen(value)+1);

    if(retval != ERROR_SUCCESS)
    {
       return GetLastError();
    } 
    else 
    {
      return 0;
    }
  }
  else
  {
    return -1;
  }
}

//----------------------------------------------------------------------------------------------------------------------

char *get_local_os_name()
{
  char *os_name = (char*) g_malloc(32);

  OSVERSIONINFO info;
  info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&info);

  if (!os_name)
      return NULL;

  strcpy(os_name, "unknown");

  switch (info.dwPlatformId)
  {
    case VER_PLATFORM_WIN32_WINDOWS:
      if((info.dwMajorVersion>=4)&&(info.dwMinorVersion==0))
      {
        strcpy(os_name, "Windows 95");
      }
      else if((info.dwMajorVersion>=4)&&(info.dwMinorVersion>0)&&(info.dwMinorVersion>90))
      {
        strcpy(os_name, "Windows 98");
      }
      else if((info.dwMajorVersion>=4)&&(info.dwMinorVersion>0)&&(info.dwMinorVersion>=90))
      {
        strcpy(os_name, "Windows ME");
      }
      break;
    case VER_PLATFORM_WIN32_NT:
      if((info.dwMajorVersion<=4)&&(info.dwMinorVersion==0))
      {
        strcpy(os_name, "Windows NT");
      }
      else if((info.dwMajorVersion==5)&&(info.dwMinorVersion==0))
      {
        strcpy(os_name, "Windows 2k");
      }
      else if((info.dwMajorVersion==5)&&(info.dwMinorVersion==1))
      {
        strcpy(os_name, "Windows XP");
      }
      else if((info.dwMajorVersion==5)&&(info.dwMinorVersion==2))
      {
        strcpy(os_name, "Windows Server 2003/XP");
      }
      break;
  }

  return os_name;
}

//----------------------------------------------------------------------------------------------------------------------

char *get_local_hardware_info()
{
  char *hardware_string;
  SYSTEM_INFO sysinfo;
  MEMORYSTATUSEX memstat;
  char processor_name[100];
  char processor_mhz[10];
  char total_phys_ram[16];
  DWORDLONG total_phys_ram_val;

  GetSystemInfo(&sysinfo);

  memstat.dwLength = sizeof(memstat);
  GlobalMemoryStatusEx(&memstat);

  strcpy(processor_mhz, "");
  strcpy(processor_name, "");

  get_value_from_registry(HKEY_LOCAL_MACHINE, "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0", "~MHz", "",
    processor_mhz);

  get_value_from_registry(HKEY_LOCAL_MACHINE, "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0", "ProcessorNameString", "",
    processor_name);

  if(!processor_name[0])
  {
    get_value_from_registry(HKEY_LOCAL_MACHINE, "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0", "Identifier", "",
      processor_name);
    sprintf(processor_name, "%s %s", processor_name, processor_mhz);
  }

  str_trim(processor_name);

  total_phys_ram_val= memstat.ullTotalPhys;
  if(total_phys_ram_val >= 1072410624 / 1.9)
  {
    sprintf(total_phys_ram, "%1.1f GB RAM", (double)total_phys_ram_val / 1072410624.0);
  }
  else if(total_phys_ram_val >= 1047276 / 1.9)
  {
    sprintf(total_phys_ram, "%1.0f MB RAM", (double)total_phys_ram_val / 1047276.0);
  }
  else
  {
    sprintf(total_phys_ram, "%d B RAM", total_phys_ram_val);
  }
   
  hardware_string = (char*) g_malloc(16 + (gulong) strlen(processor_name) + (gulong)strlen(total_phys_ram));

  if(sysinfo.dwNumberOfProcessors>1) 
  {
    sprintf(hardware_string, "%dx %s, %s", sysinfo.dwNumberOfProcessors, processor_name, total_phys_ram);
  }
  else
  {
    sprintf(hardware_string, "%s, %s", processor_name, total_phys_ram);
  }

  return hardware_string;
}
#else /* !__WIN__ */

//----------------------------------------------------------------------------------------------------------------------

// Linux/Unix version

char *get_local_os_name()
{
  struct utsname info;

  if (uname(&info) < 0)
  {
    return NULL;
  }
  
  return g_strdup_printf("%s %s", info.sysname, info.release);
}

//----------------------------------------------------------------------------------------------------------------------

#if defined(__APPLE__) && defined(__MACH__)
static const char *get_cpu_type_name(int cpu_type, int cpu_subtype)
{
  switch (cpu_type)
  {
    case CPU_TYPE_I386:
      switch (cpu_subtype)
      {
        case CPU_SUBTYPE_386:
          return "80386";
        case CPU_SUBTYPE_486:
          return "80486";
        case CPU_SUBTYPE_486SX:
          return "80486SX";
        case CPU_SUBTYPE_PENT:
          return "Pentium";
        case CPU_SUBTYPE_PENTPRO:
          return "Pentium Pro";
        case CPU_SUBTYPE_PENTII_M3:
          return "Pentium II";
        case CPU_SUBTYPE_PENTII_M5:
          return "Pentium II";
#ifdef CPU_SUBTYPE_CELERON
        case CPU_SUBTYPE_CELERON:
          return "Celeron";
#endif
#ifdef CPU_SUBTYPE_CELERON_MOBILE
        case CPU_SUBTYPE_CELERON_MOBILE:
          return "Celeron Mobile";
#endif
#ifdef CPU_SUBTYPE_PENTIUM_3
        case CPU_SUBTYPE_PENTIUM_3:
          return "Pentium III";
#endif
#ifdef CPU_SUBTYPE_PENTIUM_3_M
        case CPU_SUBTYPE_PENTIUM_3_M:
          return "Pentium III M";
#endif
#ifdef CPU_SUBTYPE_PENTIUM_3_XEON
        case CPU_SUBTYPE_PENTIUM_3_XEON:
          return "Pentium III Xeon";
#endif
#ifdef CPU_SUBTYPE_PENTIUM_M
        case CPU_SUBTYPE_PENTIUM_M:
          return "Pentium M";
#endif
#ifdef CPU_SUBTYPE_PENTIUM_4
        case CPU_SUBTYPE_PENTIUM_4:
          return "Pentium 4";
#endif
#ifdef CPU_SUBTYPE_PENTIUM_4_M
        case CPU_SUBTYPE_PENTIUM_4_M:
          return "Pentium 4M";
#endif
#ifdef CPU_SUBTYPE_ITANIUM
        case CPU_SUBTYPE_ITANIUM:
          return "Itanium";
#endif
#ifdef CPU_SUBTYPE_ITANIUM_2
        case CPU_SUBTYPE_ITANIUM_2:
          return "Itanium 2";
#endif
#ifdef CPU_SUBTYPE_XEON
        case CPU_SUBTYPE_XEON:
          return "Xeon";
#endif
#ifdef CPU_SUBTYPE_XEON_MP
        case CPU_SUBTYPE_XEON_MP:
          return "Xeon MP";
#endif
        default:
          return "x86";
      }
      break;
    case CPU_TYPE_POWERPC:
      switch (cpu_subtype)
      {
        case CPU_SUBTYPE_POWERPC_601:
          return "PowerPC 601";
        case CPU_SUBTYPE_POWERPC_602:
          return "PowerPC 602";
        case CPU_SUBTYPE_POWERPC_603:
          return "PowerPC 603";
        case CPU_SUBTYPE_POWERPC_603e:
          return "PowerPC 603e";
        case CPU_SUBTYPE_POWERPC_603ev:
          return "PowerPC 603ev";
        case CPU_SUBTYPE_POWERPC_604:
          return "PowerPC 604";
        case CPU_SUBTYPE_POWERPC_604e:
          return "PowerPC 604e";
        case CPU_SUBTYPE_POWERPC_620:
          return "PowerPC 620";
        case CPU_SUBTYPE_POWERPC_750:
          return "PowerPC G3";
        case CPU_SUBTYPE_POWERPC_7400:
          return "PowerPC G4";
        case CPU_SUBTYPE_POWERPC_7450:
          return "PowerPC G4";
        case CPU_SUBTYPE_POWERPC_970:
          return "PowerPC G5";
        default:
          return "PowerPC";
      }
      break;
  }
  return "Unknown";
}

//----------------------------------------------------------------------------------------------------------------------

// MacOS X
static int _get_hardware_info(char **cpu, char **clock, int *cpu_count, unsigned long *mem_kb)
{
  int mib[2];
  size_t length;
  unsigned int tmp;
  char *mclass;
  int cpu_type, cpu_subtype;
  
  // get machine class
  mib[0]= CTL_HW;
  mib[1]= HW_MACHINE;
  sysctl(mib, 2, NULL, &length, NULL, 0);
  mclass= g_malloc(length*sizeof(char*));
  sysctl(mib, 2, mclass, &length, NULL, 0);

  // get machine arch
  length= sizeof(cpu_type);
  sysctlbyname("hw.cputype", &cpu_type, &length, NULL, 0);
  length= sizeof(cpu_subtype);
  sysctlbyname("hw.cpusubtype", &cpu_subtype, &length, NULL, 0);
  
  *cpu= g_strdup_printf("%s (%s)", mclass, get_cpu_type_name(cpu_type, cpu_subtype));
  g_free(mclass);
  
  // get cpu clock
  mib[0]= CTL_HW;
  mib[1]= HW_CPU_FREQ;
  length= sizeof(tmp);
  if (sysctl(mib, 2, &tmp, &length, NULL, 0) < 0)
    *clock= g_strdup_printf("?");  
  else
    *clock= g_strdup_printf("%.01f", (double)tmp/1000000.0);

  // get cpu count
  mib[0]= CTL_HW;
  mib[1]= HW_NCPU;
  length= sizeof(cpu_count);
  if (sysctl(mib, 2, cpu_count, &length, NULL, 0) < 0)
    *cpu_count= 1;

  // get memory size
  *mem_kb= get_physical_memory_size()/1024;

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

#else
// Linux
static int _get_hardware_info(char **cpu, char **clock, int *cpu_count, unsigned long *mem_kb)
{
  FILE *proc;
  char line[256];

  // fetch processor info from /proc/cpuinfo
  
  proc= fopen("/proc/cpuinfo", "r");
  if (!proc) 
  {
    return -1;
  }

  *cpu_count= 0;
  while (!feof(proc)) 
  {
    if (!fgets(line, sizeof(line), proc))
      break;
    
    if (str_beginswith(line,"model name")) 
    {
      (*cpu_count)++;
      *cpu= g_strdup(str_trim(strchr(line, ':')+1));
    } 
    else if (str_beginswith(line,"cpu MHz"))
    {
      *clock= g_strdup(str_trim(strchr(line, ':')+1));
    }
  }
  fclose(proc);
  
  *mem_kb= get_physical_memory_size()/1024;

  return 0;
}
#endif

//----------------------------------------------------------------------------------------------------------------------

char *get_local_hardware_info()
{
  char *hardware_string;
  int processor_count = 0;
  char *processor_name= NULL;
  char *processor_mhz= NULL;
  char total_phys_ram[64];
  unsigned long total_phys_ram_val = 0;

  
  _get_hardware_info(&processor_name, &processor_mhz, &processor_count,
                     &total_phys_ram_val);
  
  if (total_phys_ram_val>=(1024*1024)/1.9)
  {
    sprintf(total_phys_ram, "%1.1f GB RAM", (double)total_phys_ram_val/(1024*1024));
  }
  else if(total_phys_ram_val>=1024/1.9)
  {
    sprintf(total_phys_ram, "%1.0f MB RAM", (double)total_phys_ram_val/1024);
  }
  else
  {
    sprintf(total_phys_ram, "%ld KB RAM", total_phys_ram_val);
  }

  if(processor_count>1)
  {
    hardware_string= g_strdup_printf("%dx %s %s MHz, %s", processor_count, processor_name, processor_mhz, total_phys_ram);
  }
  else
  {
    hardware_string= g_strdup_printf("%s %s MHz, %s", processor_name, processor_mhz, total_phys_ram);
  }
  
  g_free(processor_name);
  g_free(processor_mhz);

  return hardware_string;
}

//----------------------------------------------------------------------------------------------------------------------

FILE *myx_popen(char *const args[], pid_t *pid_ret)
{
  FILE *f;
  int fd[2];
  
  if (pipe(fd) >= 0)
  {
    *pid_ret = fork();
    if (*pid_ret == 0) 
    {
      close(1);
      close(2);
      dup2(fd[1], 1);
      dup2(fd[1], 2);

      close(fd[0]);

      execvp(args[0], args);
      exit(-1);
    }
    else if (*pid_ret > 0)
    {
      close(fd[1]);

      f = fdopen(fd[0], "r");
      if (!f)
      {
        int ret;
        kill(*pid_ret, 9);
        waitpid(*pid_ret, &ret, 0);
        close(fd[0]);
      }
      return f;
    }
  }

  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_read_timeout(FILE *f, int timeout, char *result, size_t result_len)
{
  fd_set fds;
  struct timeval tv;
  int ret;
  
  FD_ZERO(&fds);
  FD_SET(fileno(f), &fds);

  tv.tv_sec = timeout / 1000;
  tv.tv_usec = (timeout % 1000)*1000;
  
  do {
    ret= select(fileno(f)+1, &fds, NULL, NULL, timeout<0?NULL:&tv);
  } while (ret < 0 && errno == EINTR);
  
  if (ret > 0)
  {
    if (fgets(result, result_len, f))
      return strlen(result);
    else
      return 0;
  }
  return -1;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_pclose(FILE *f, pid_t pid)
{
  int status = -1;

  if (kill(pid, SIGKILL) == 0)
      waitpid(pid, &status, 0);
  fclose(f);

  return status;
}

//----------------------------------------------------------------------------------------------------------------------

int copy_file(const char *orig_file, const char *new_file)
{
  char buffer[4*1024];
  int from_fd, to_fd;
  int count;
  
  from_fd= open(orig_file, O_RDONLY, 0666);
  if (from_fd < 0)
    return -1;
  to_fd= open(new_file, O_WRONLY|O_CREAT, 0666);
  if (to_fd < 0)
  {
    int err= errno;
    close(from_fd);
    errno= err;
    return -1;
  }
  
  do {
    count= read(from_fd, buffer, sizeof(buffer));
    if (count < 0)
    {
      int err= errno;
      close(from_fd);
      close(to_fd);
      errno= err;
      return -1;
    }
    if (count > 0 && write(to_fd, buffer, count) < 0)
    {
      int err= errno;
      close(from_fd);
      close(to_fd);
      errno= err;
      return -1;
    }
  } while (count > 0);

  close(from_fd);
  close(to_fd);

  return 0;
}
#endif

//----------------------------------------------------------------------------------------------------------------------

bigint get_physical_memory_size()
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  MEMORYSTATUS memstat;
  
  GlobalMemoryStatus(&memstat);
  
  return memstat.dwTotalPhys;
#elif defined(__APPLE__)
  uint64_t mem64;
  int mib[2];
  int mem32;
  size_t length;
  mib[0]= CTL_HW;
  mib[1]= HW_MEMSIZE;
  length= sizeof(mem64);
  if (sysctl(mib, 2, &mem64, &length, NULL, 0) < 0)
  {
    mib[0]= CTL_HW;
    mib[1]= HW_PHYSMEM;
    length= sizeof(mem32);
    sysctl(mib, 2, &mem32, &length, NULL, 0);
    mem64= mem32;
  }
  
  return mem64;
#else
  FILE *proc;
  bigint mem64;
  mem64= 0;
  // fetch physical memory info from /proc/meminfo
  proc= fopen("/proc/meminfo", "r");
  if (proc)
  {
    char line[1024];
    char *ptr, *end;

    while (fgets(line, sizeof(line), proc))
    {
      if (strncasecmp(line, "MemTotal:", sizeof("MemTotal:")-1)==0)
      {
        char *line_end= line+strlen(line);
        ptr= strchr(line, ':')+1;
        while (*ptr && *ptr==' ') ptr++;
        end= strchr(ptr, ' ');
        if (end)
          *end= 0;
        if (end < line_end)
          end++;
        if (strstr(end, "gB") || strstr(end, "GB"))
          mem64= strtoul(str_trim(ptr), NULL, 10)*1024*1024*1024LL;
        else if (strstr(end, "mB") || strstr(end, "MB"))
          mem64= strtoul(str_trim(ptr), NULL, 10)*1024*1024LL;
        else if (strstr(end, "kB") || strstr(end, "KB"))
          mem64= strtoul(str_trim(ptr), NULL, 10)*1024LL;
        else
          mem64= strtoul(str_trim(ptr), NULL, 10);
        break;
      }
    }
    fclose(proc);
  }
  else
  {
    g_warning("Memory stats retrieval not implemented for this system");
  }
  return mem64;

#endif
}

//----------------------------------------------------------------------------------------------------------------------

bigint get_file_size(const char *filename)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  DWORD dwSizeLow;
  DWORD dwSizeHigh = 0;
  HANDLE hfile;
  char *local_filename;

  if ((local_filename= g_filename_from_utf8(filename,-1,NULL,NULL,NULL)) == NULL)
    return -1;
  hfile = CreateFile(local_filename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

  if (hfile != INVALID_HANDLE_VALUE) 
  {
    dwSizeLow= GetFileSize(hfile, &dwSizeHigh);

    CloseHandle(hfile);
    g_free(local_filename);

    if((dwSizeLow==INVALID_FILE_SIZE)&&(GetLastError()) != NO_ERROR )
    { 
      return -1;
    }
    else
    {
      return (((bigint) dwSizeHigh << 32) + dwSizeLow);
    }
  }
  else
  {
    g_free(local_filename);
    return -1;
  }
#else //!WINDOWS
  struct stat buf;
  char *local_filename;

  if (! (local_filename= g_filename_from_utf8(filename,-1,NULL,NULL,NULL)))
    return -1;

  if (stat(local_filename, &buf) < 0) {
    g_free(local_filename);
    return -1;
  }
  g_free(local_filename);
  return buf.st_size;
#endif //!WINDOWS
}

// note, needle has to be ascii!
char *strcasestr_len(const char *haystack, int haystack_len, const char *needle)
{
  gssize needle_len= (gssize)strlen(needle);
  int i;

  if (needle_len > haystack_len)
    return NULL;

  i= 0;
  while (i <= haystack_len - needle_len)
  {
    if (g_ascii_strncasecmp(needle, haystack+i, needle_len)==0)
      return (char *)haystack+i;
    i++;
  }
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

#define O_VECTOR_COUNT 64 // max # of ()*2+2

char * get_value_from_text_ex_opt(const char *txt, int txt_length,
                                  const char *regexpr,
                                  unsigned int substring_nr,
                                  int options_for_exec)
{
  pcre *pcre_exp;
  const char *error_str;
  int erroffset;
  int o_vector[O_VECTOR_COUNT];
  int rc;
  const char *ret_val;
  char *value= NULL;

  if(txt && *txt)
  {
    pcre_exp= pcre_compile(regexpr, PCRE_CASELESS, &error_str, &erroffset, NULL);
    if (pcre_exp)
    {
      if ((rc= pcre_exec(pcre_exp, NULL, txt, txt_length, 0, 
                          options_for_exec, o_vector, O_VECTOR_COUNT) ) > 0)
      {
        if (o_vector[substring_nr * 2] != -1)
        {
          pcre_get_substring(txt, o_vector, rc, substring_nr, &ret_val);

          value= g_strdup(ret_val);

          pcre_free_substring((char*)ret_val);
        }
      }

      pcre_free(pcre_exp);
    }
  }

  return value;
}

//----------------------------------------------------------------------------------------------------------------------

char * get_value_from_text_ex(const char *txt, int txt_length,
                              const char *regexpr, unsigned int substring_nr)
{
  return get_value_from_text_ex_opt(txt,txt_length,regexpr,substring_nr,0);
}

//----------------------------------------------------------------------------------------------------------------------

char * get_value_from_text(const char *txt, int txt_length, const char *regexpr)
{
  return get_value_from_text_ex(txt, txt_length, regexpr, 1);
}

//----------------------------------------------------------------------------------------------------------------------

int strlist_g_indexof(const char **list, const char *value)
{
  int i= 0;
  while(list[i] != NULL)
  {
    if(strcmp(list[i], value) == 0)
    {
      return i;
    }
  }
  return -1;
}

//----------------------------------------------------------------------------------------------------------------------

void strlist_g_append(char ***list, char *value)
{
  unsigned int i;

  if (*list)
  {
    for (i= 0; (*list)[i]; i++);
      *list= (char**) g_realloc(*list, sizeof(char*)*(i+2));
    (*list)[i]= value;
    (*list)[i+1]= 0;
  }
  else
  {
    *list= (char**) g_malloc(sizeof(char*)*2);
    (*list)[0]= value;
    (*list)[1]= 0;
  }
}

//----------------------------------------------------------------------------------------------------------------------

void strlist_g_append_or_replace(char ***list, char *value)
{
  int i= strlist_g_indexof((const char**)*list, value);
  if(i < 0)
  {
    strlist_g_append(list, value);
  }
  else
  {
    (*list)[i]= value;
  }
}

//----------------------------------------------------------------------------------------------------------------------

const char *strfindword(const char *str, const char *word)
{
  const char* result = NULL;
  const char *ptr;
  size_t wordlen= strlen(word);

  ptr= str;
  for (;;)
  {
    // find match
    ptr= strcasestr_len(ptr, (int)strlen(ptr), word);
    if (!ptr)
      break;

    // check if its acceptable
    if ((ptr == str || !isalnum(*(ptr - 1))) && // space or any other non-alpha-numeric before
      (!isalnum(*(ptr + wordlen)) || *(ptr + wordlen) == '\0')) // space or any other non-alpha-numeric after
    {
      result= ptr;
      break;
    };
    ptr+= wordlen;
  }

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

char *subst_pcre(const char *pattern, const char *repl,
                 int flags, int max_matches, 
                 const char *string)
{
  pcre *pcre_exp;
  const char *error_str;
  int erroffset;
  char *res= NULL;

  pcre_exp= pcre_compile(pattern, flags, &error_str, &erroffset, NULL);
  if (pcre_exp)
  {
    int rc;
    int *matches= (int*)g_malloc(sizeof(int)*(max_matches*3));
    rc= pcre_exec(pcre_exp, NULL, string, (int)strlen(string), 0, 0, matches, max_matches);
    if (rc > 0)
    {
      res= subst_pcre_matches(string, matches, rc, repl);
    }
    pcre_free(pcre_exp);
    g_free(matches);
  }
  else
  {
    g_message("error compiling PCRE pattern: %s", error_str);
  }
  return res;
}

//----------------------------------------------------------------------------------------------------------------------

char *subst_pcre_matches(const char *src, int *matches, int matchcount,
                         const char *repl)
{
  const char *p0, *pf;
  int ressize= (int)strlen(repl);
  int reslen= 0;
  char *res= (char*) g_malloc(ressize);
  char number[4];
  int index;

  p0= repl;
  while (p0)
  {
    pf= strchr(p0, '\\');
    if (pf)
    {
      __sappend(&res, &ressize, &reslen, p0, (int)(pf-p0));
      pf++;
      // no more than 99 groups!
      if (isdigit(*pf) && isdigit(*(pf+1)))
      {
        number[0]=*(pf++);
        number[1]=*(pf++);
        number[2]=0;
        index= atoi(number);
      }
      else if (isdigit(*pf))
      {
        number[0]=*(pf++);
        number[1]=0;
        index= atoi(number);
      }
      else
        index= -1;
      if (index > 0 && index <= matchcount)
        __sappend(&res, &ressize, &reslen, src+matches[index*2],
                  matches[index*2+1]-matches[index*2]);
    }
    else
    {
      __sappend(&res, &ressize, &reslen, p0, (int)strlen(p0));
      pf= NULL;
    }
    p0= pf;
  }

  return (char*) g_realloc(res, reslen+1);
}

//----------------------------------------------------------------------------------------------------------------------

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)

void timer_start(MYX_TIMER_VALUE *tval)
{
  QueryPerformanceCounter(tval);
}

//----------------------------------------------------------------------------------------------------------------------

double timer_stop(MYX_TIMER_VALUE *tval)
{
  MYX_TIMER_VALUE end;
  LARGE_INTEGER freq;

  QueryPerformanceCounter(&end);

  QueryPerformanceFrequency(&freq);

  return ((double)end.QuadPart - (double)tval->QuadPart)/((double)freq.QuadPart);
}

#else

//----------------------------------------------------------------------------------------------------------------------

void timer_start(MYX_TIMER_VALUE *tval)
{
  gettimeofday(tval, NULL);
}

//----------------------------------------------------------------------------------------------------------------------

double timer_stop(MYX_TIMER_VALUE *tval)
{
  MYX_TIMER_VALUE end;
  
  gettimeofday(&end, NULL);
  
  return (end.tv_sec - tval->tv_sec) + (end.tv_usec - tval->tv_usec)/1000000;
}

#endif

//----------------------------------------------------------------------------------------------------------------------

void *vec_insert_resize(void *vec, guint size, guint *vecsize, guint pos, void *data)
{
  vec= g_realloc(vec, size*(*vecsize+1));

  if (*vecsize > 0 && ((size_t)pos) < (*vecsize-1))
    memmove((char*)vec+(pos+1)*size, (char*)vec+pos*size, size*(*vecsize-pos));

  memcpy((char*)vec+pos*size, data, size);

  (*vecsize)++;

  return vec;
}

//----------------------------------------------------------------------------------------------------------------------

void *vec_remove(void *vec, guint size, guint *vecsize, guint pos)
{
  if (*vecsize > 0 && ((size_t)pos) < (*vecsize-1))
    memmove((char*)vec+pos*size, (char*)vec+(pos+1)*size, size*(*vecsize-pos-1));

  (*vecsize)--;

  return vec;
}

//----------------------------------------------------------------------------------------------------------------------

int split_schema_table(const char *ident, char **schema, char **table)
{
  pcre *pcre_exp;
  const char *error_str;
  int erroffset;
  int o_vector[32];
  int rc;

  pcre_exp= pcre_compile("(\\w+|`.+?`|\".+?\")(?:\\.(\\w+|`.+?`|\".+?\"))?",
                         PCRE_CASELESS|PCRE_UTF8|PCRE_DOTALL, &error_str, &erroffset, NULL);

  *schema= NULL;
  *table= NULL;
  
  if (pcre_exp)
  {
    if ((rc= pcre_exec(pcre_exp, NULL, ident, (int)strlen(ident),
                       0, 0, o_vector, sizeof(o_vector)/sizeof(int))) > 0)
    {
      const char *a= NULL, *b= NULL;

      pcre_get_substring(ident, o_vector, rc, 1, &a);
      if (a)
      {
        *schema= unquote_identifier(g_strdup(a));
        pcre_free_substring(a);
      }

      pcre_get_substring(ident, o_vector, rc, 2, &b);
      if (b)
      {
        *table= unquote_identifier(g_strdup(b));
        pcre_free_substring(b);
      }
      
      pcre_free(pcre_exp);
      
      if (*schema && *table)
        return 2;
      else if (*schema && !*table)
      {
        *table= *schema;
        *schema= NULL;
        return 1;
      }
    }
    pcre_free(pcre_exp);
  }
  return -1;
}

//----------------------------------------------------------------------------------------------------------------------

static int cmp_entity(const void *a, const void *b)
{
  HTMLEntity *aa= (HTMLEntity*)a;
  HTMLEntity *bb= (HTMLEntity*)b;

  if (aa->code < bb->code)
    return -1;
  else if (aa->code > bb->code)
    return 1;
  else
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------

static char *escape_entities(const char *str, HTMLEntity *entities)
{
  char *outstr= NULL;
  int alloced= 0;
  int length= 0;
  gunichar uc;
  int entity_num= 0;
  while (entities[entity_num].name) entity_num++;

  while (*str)
  {
    HTMLEntity key;
    HTMLEntity *p;
    
    uc= g_utf8_get_char(str);

    key.code= uc;
    p= (HTMLEntity*)bsearch(&key, entities, entity_num, sizeof(entities[0]), cmp_entity);

    if (p)
    {
      char buffer[100];
      
      buffer[0]= '&';
      strcpy(buffer+1, p->name);
      strcat(buffer, ";");

      __sappend(&outstr, &alloced, &length, buffer, (int)strlen(buffer));
    }
    else
    {
      __sappend(&outstr, &alloced, &length, str, (int)(g_utf8_next_char(str)-str));
    }

    str= g_utf8_next_char(str);
  }

  return outstr;
}

//----------------------------------------------------------------------------------------------------------------------

char *escape_xml_entities(const char *str)
{
  return escape_entities(str, XMLEntities);
}

//----------------------------------------------------------------------------------------------------------------------

char *escape_html_entities(const char *str)
{
  return escape_entities(str, HTMLEntities);
}

//----------------------------------------------------------------------------------------------------------------------

char *unescape_html_entities(const char *str)
{
  char *outstr= NULL;
  int alloced= 0;
  int length= 0;
  int i;
  const char *b, *e;
  gchar utf8char[10];

  b= str;
  while (*b)
  {
    e= strchr(b, '&');
    if (e)
    {
      const char *p;

      if (e-b > 0) // copy until &
        __sappend(&outstr, &alloced, &length, b, (int)(e-b));

      // find end of &thing;
      p= e+1;
      while (*p && *p!='&' && *p!=';') p++;

      if (*p == 0) // no ; found
        __sappend(&outstr, &alloced, &length, e, (int)(p-e));
      else if (*p == '&') // another & found, copy what we have and start over
        __sappend(&outstr, &alloced, &length, e, (int)(p-e));
      else if (*p == ';') // &ewq;
      {
        p++;

        i= -1;
        if (p-e-2 > 0)
        {
          for (i= 0; HTMLEntities[i].name; i++)
          {
            if (strncmp(HTMLEntities[i].name, e+1, p-e-2)==0)
              break;
          }
        }

        if (i >= 0 && HTMLEntities[i].name && p-e-2>0)
        {
          i= g_unichar_to_utf8(HTMLEntities[i].code, utf8char);
          __sappend(&outstr, &alloced, &length, utf8char, i);
        }
        else // invalid &thing; just copy it over
          __sappend(&outstr, &alloced, &length, e, (int)(p-e));
      }

      b= p;
    }
    else
    {
      // no &things; in the rest of the string
      __sappend(&outstr, &alloced, &length, b, (int)strlen(b));
      break;
    }
  }

  return outstr;
}

//----------------------------------------------------------------------------------------------------------------------

//taken from glib
int myx_mkdir(const char *filename, int mode, int *error_no)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  wchar_t *wfilename = (wchar_t*) g_utf8_to_utf16 (filename, -1, NULL, NULL, NULL);
  int retval= _wmkdir (wfilename);
  int save_errno= errno;

  *error_no= errno;

  g_free (wfilename);
  
  errno= save_errno;
  return retval;
#else
  int retval= mkdir (filename, mode);

  *error_no= errno;

  return retval;
#endif
}

//----------------------------------------------------------------------------------------------------------------------

int myx_chdir(const char *path)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  wchar_t *wpath = (wchar_t*) g_utf8_to_utf16 (path, -1, NULL, NULL, NULL);
  int retval;
  int save_errno;

  if (wpath == NULL)
	{
	  errno = EINVAL;
	  return -1;
	}

  retval = _wchdir (wpath);
  save_errno = errno;

  g_free (wpath);
  
  errno = save_errno;
  return retval;
#else
  return chdir(path);
#endif
}

//----------------------------------------------------------------------------------------------------------------------

int get_str_index(char **string_list, unsigned int string_list_num, const char *search)
{
  unsigned int i= 0;

  while (i<string_list_num)
  {
    if (strcmp2(string_list[i], search) == 0)
      return i;

    i++;
  }

  return -1;
}

//----------------------------------------------------------------------------------------------------------------------

/** @brief checks if filename really exists
    @param filename path to file
    @return 0 if the file doesn't exist else 1
    \b vva_todo why don't it use access function?
*/
int check_file_exists(const char *filename)
{
  FILE *f;

  /*check if we can open the file */    
  if ((f= myx_fopen(filename, "r")) == NULL)
  {
    return 0;
  }
  else
  {
    fclose(f);
    return 1;
  }
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scans the given string and duplicates it to result while inserting a back slash character in front of each
 * character that must be masked.
 *
 * @param source The source string to examine.
 * @param special_chars All characters that must be masked.
 *
 * @return A new copy of source with all special characters escaped. This string must be freed by the caller (using g_free).
 * @note covered by unit tests
 */
char* internal_escape_string(const char* source, const char* special_chars)
{
  int i;
  int j;
  char* result= NULL;

  if (source != NULL)
  {
    // Do a first loop to determine how many characters must be escaped.
    int count= 0;
    i= 0;
    while (source[i] != '\0')
    {
      if (strchr(special_chars, source[i++]) != NULL)
        count++;
    };

    // If no special char was found then just duplicate the string otherwise do a second round.
    if (count == 0)
    {
      result= g_strdup(source);
    }
    else
    {
      result= (char*) g_malloc0(i + count + 1);
      i= 0;
      j= 0;
      
      while (source[i] != '\0')
      {
        if (strchr(special_chars, source[i]) != NULL)
        {
          result[j++]= '\\';
          switch (source[i])
          {
            case '\n':
              result[j++]= 'n';
              break;
            case '\r':
              result[j++]= 'r';
              break;
            case '\b':
              result[j++]= 'b';
              break;
            case '\t':
              result[j++]= 't';
              break;
            case '\x1a':
              result[j++]= 'Z';
              break;
          default:
            result[j++]= source[i];
          };
        }
        else
          result[j++]= source[i];
        ++i;
      };
      result[j]= '\0';
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scans the given string and duplicates it to result while inserting a back slash character in front of each
 * character that must be masked.
 *
 * @param source The source string to examine.
 *
 * @return A new copy of source with all special characters escaped. This string must be freed by the caller (using g_free).
 * @note This function does not escape % and _ as they are used only with pattern matching. Use escape_string_for_search
 *       if you need these characters escaped too.
 * @note covered by unit tests
 */
char * escape_string(const char* source)
{
  return internal_escape_string(source, "'\"\x8\n\r\t\x1a\\");
}

//----------------------------------------------------------------------------------------------------------------------

char * escape_string_for_search(const char* source)
{
  return internal_escape_string(source, "'\"\x8\n\r\t\x1a\\%_");
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Similar to the (non-portable) itoa function. Converts a number to a string using the given base, which can be
 * between 2 and 16 (inclusive).
 *
 * @param num The number to convert.
 * @param base The base to use for the output.
 *
 * @return A string containing the number in the given base. The string must be freed using g_free (caller is responsible).
 */
char* baseconv(ubigint num, int base)
{
  char buffer[65];
  char *p;

  if (base < 2 || base > 16)
    return NULL;

  p = &buffer[sizeof(buffer) - 1];
  *p = '\0';

  do
  {
    *--p = "0123456789abcdef"[(int)(num % base)];
    num /= base;
  } while (num != 0);

  return g_strdup(p);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Case insensitive variant of strstr. Use this only for ANSI text!
 *
 * @param haystack The string to search through.
 * @param needle The string to search in haystack.
 *
 * @return The position of needle in haystack if there is a match or NULL if not.
 */
const char *stristr(const char *haystack, const char *needle)
{
  if (!*needle)
  {
    return haystack;
  };

  for ( ; *haystack; ++haystack )
  {
    if (toupper(*haystack) == toupper(*needle))
    {
      // Matched starting char -- loop through remaining chars.
      const char *h, *n;
      for ( h = haystack, n = needle; *h && *n; ++h, ++n )
      {
        if ( toupper(*h) != toupper(*n) )
          break;
      };

      if (!*n) // Matched all of 'needle' to null termination?
        return haystack; // Return the start of the match.
    }
  }
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------


