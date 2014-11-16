/* Copyright (c) 2005 MySQL AB
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
 */


#ifndef __GRT_PHP_H_
#define __GRT_PHP_H_

/* PHP Defines */
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define ZEND_WIN32
#define PHP_WIN32
#define ZTS 1
#define ZEND_DEBUG 0
#endif
 
/* PHP Includes */
#ifdef __cplusplus
extern "C" {
#endif
	#include <php.h>
	#include <php_embed.h>
#ifdef __cplusplus
}
#endif

#define APPLNAME "grt"
#define PHP_GRT_VERSION "1.0"

#include "myx_grt_private.h"


typedef struct MYX_GRT_MODULE_LOADER_PRIVATE
{
  int state;
} MYX_PHP_LOADER;


typedef struct MYX_GRT_MODULE_PRIVATE
{
  char *classname;
} MYX_PHP_MODULE;

typedef struct MYX_GRT_FUNCTION_PRIVATE
{
  char *php_signature;
} MYX_PHP_FUNCTION;

enum eval_ret_t { 
  EVAL_RET_ERROR, 
  EVAL_RET_OK, 
  EVAL_RET_BAIL
};

MYX_GRT_MODULE_LOADER *myx_php_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error);

#endif
